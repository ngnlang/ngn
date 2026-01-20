#include "llama_bridge.h"

#include "llama.h"
#include "ggml-backend.h"

#include <string>
#include <vector>
#include <cstring>

struct ngn_llama_handle {
    llama_model   * model = nullptr;
    llama_context * ctx   = nullptr;
    const llama_vocab * vocab = nullptr;
};

static void write_err(char * buf, int32_t len, const char * msg) {
    if (!buf || len <= 0) {
        return;
    }
    std::snprintf(buf, (size_t) len, "%s", msg ? msg : "");
    buf[len - 1] = '\0';
}

void ngn_llama_backend_init(void) {
    static bool inited = false;
    if (!inited) {
        llama_backend_init();
        // Recent llama.cpp requires explicit backend loading.
        // For CPU-only builds, this should at least register the built-in CPU backend.
        ggml_backend_load_all();
        inited = true;
    }
}

ngn_llama_handle * ngn_llama_load(const char * path, ngn_llama_load_opts opts, char * err_buf, int32_t err_len) {
    write_err(err_buf, err_len, "");
    if (!path || !*path) {
        write_err(err_buf, err_len, "model path is empty");
        return nullptr;
    }

    ngn_llama_backend_init();

    auto mparams = llama_model_default_params();
    mparams.use_mmap = opts.use_mmap;
    mparams.vocab_only = false;
    mparams.n_gpu_layers = 0;

    llama_model * model = llama_model_load_from_file(path, mparams);
    if (!model) {
        write_err(err_buf, err_len, "failed to load model");
        return nullptr;
    }

    auto cparams = llama_context_default_params();
    if (opts.n_ctx > 0) {
        cparams.n_ctx = (uint32_t) opts.n_ctx;
    }
    if (opts.n_threads > 0) {
        cparams.n_threads = opts.n_threads;
        cparams.n_threads_batch = opts.n_threads;
    }

    llama_context * ctx = llama_init_from_model(model, cparams);
    if (!ctx) {
        llama_model_free(model);
        write_err(err_buf, err_len, "failed to init context");
        return nullptr;
    }

    const llama_vocab * vocab = llama_model_get_vocab(model);
    if (!vocab) {
        llama_free(ctx);
        llama_model_free(model);
        write_err(err_buf, err_len, "failed to get vocab");
        return nullptr;
    }

    if (opts.n_threads > 0) {
        llama_set_n_threads(ctx, opts.n_threads, opts.n_threads);
    }

    auto * h = new ngn_llama_handle();
    h->model = model;
    h->ctx = ctx;
    h->vocab = vocab;
    return h;
}

void ngn_llama_free(ngn_llama_handle * h) {
    if (!h) {
        return;
    }
    if (h->ctx) {
        llama_free(h->ctx);
        h->ctx = nullptr;
    }
    if (h->model) {
        llama_model_free(h->model);
        h->model = nullptr;
    }
    delete h;
}

static bool tokenize(const llama_vocab * vocab, const char * text, std::vector<llama_token> & out, std::string & err) {
    if (!vocab) {
        err = "invalid vocab";
        return false;
    }
    if (!text) {
        err = "prompt is null";
        return false;
    }

    const int32_t len = (int32_t) std::strlen(text);
    int32_t cap = len + 8;
    out.assign((size_t) cap, 0);

    // Avoid double-BOS when the prompt already includes a BOS token (e.g. "<s>")
    // and we're also asking llama_tokenize() to add special tokens.
    bool add_special = true;
    {
        const char * p = text;
        while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') {
            ++p;
        }
        if (std::strncmp(p, "<s>", 3) == 0) {
            add_special = false;
        }
        if (std::strncmp(p, "<|begin_of_text|>", 17) == 0) {
            add_special = false;
        }
    }

    int32_t n = llama_tokenize(vocab, text, len, out.data(), cap, add_special, true);
    if (n == INT32_MIN) {
        err = "tokenize overflow";
        return false;
    }
    if (n < 0) {
        const int32_t need = -n;
        out.assign((size_t) need, 0);
        n = llama_tokenize(vocab, text, len, out.data(), need, add_special, true);
        if (n < 0) {
            err = "tokenize failed";
            return false;
        }
    }

    out.resize((size_t) n);
    return true;
}

static bool token_to_piece(const llama_vocab * vocab, llama_token tok, std::string & out) {
    out.clear();
    if (!vocab) {
        return false;
    }

    std::vector<char> buf(256);
    int32_t n = llama_token_to_piece(vocab, tok, buf.data(), (int32_t) buf.size(), 0, true);
    if (n < 0) {
        const int32_t need = -n;
        buf.assign((size_t) need, 0);
        n = llama_token_to_piece(vocab, tok, buf.data(), (int32_t) buf.size(), 0, true);
    }
    if (n <= 0) {
        return true;
    }
    out.assign(buf.data(), buf.data() + n);
    return true;
}

int32_t ngn_llama_generate(
    ngn_llama_handle * h,
    const char * prompt,
    ngn_llama_gen_opts opts,
    ngn_llama_chunk_cb cb,
    void * user_data,
    char * err_buf,
    int32_t err_len) {

    write_err(err_buf, err_len, "");
    if (!h || !h->ctx || !h->model || !h->vocab) {
        write_err(err_buf, err_len, "invalid model handle");
        return -1;
    }
    if (!prompt) {
        write_err(err_buf, err_len, "prompt is null");
        return -1;
    }
    if (!cb) {
        write_err(err_buf, err_len, "chunk callback is null");
        return -1;
    }

    if (opts.max_tokens <= 0) {
        opts.max_tokens = 128;
    }
    if (opts.top_k <= 0) {
        opts.top_k = 40;
    }
    if (opts.top_p <= 0.0f) {
        opts.top_p = 0.95f;
    }
    if (opts.temperature <= 0.0f) {
        opts.temperature = 0.8f;
    }

    // Clear KV/memory between calls.
    llama_memory_clear(llama_get_memory(h->ctx), true);

    std::vector<llama_token> tokens;
    std::string err;
    if (!tokenize(h->vocab, prompt, tokens, err)) {
        write_err(err_buf, err_len, err.c_str());
        return -1;
    }
    if (tokens.empty()) {
        return 0;
    }

    llama_batch batch = llama_batch_get_one(tokens.data(), (int32_t) tokens.size());
    const int32_t rc_prompt = llama_decode(h->ctx, batch);
    if (rc_prompt < 0) {
        write_err(err_buf, err_len, "llama_decode(prompt) failed");
        return -1;
    }

    auto sparams = llama_sampler_chain_default_params();
    llama_sampler * chain = llama_sampler_chain_init(sparams);
    if (!chain) {
        write_err(err_buf, err_len, "failed to init sampler chain");
        return -1;
    }

    llama_sampler_chain_add(chain, llama_sampler_init_top_k(opts.top_k));
    llama_sampler_chain_add(chain, llama_sampler_init_top_p(opts.top_p, 1));
    llama_sampler_chain_add(chain, llama_sampler_init_temp(opts.temperature));
    llama_sampler_chain_add(chain, llama_sampler_init_dist(opts.seed));

    std::string piece;
    for (int32_t i = 0; i < opts.max_tokens; i++) {
        const llama_token tok = llama_sampler_sample(chain, h->ctx, -1);
        llama_sampler_accept(chain, tok);

        if (llama_vocab_is_eog(h->vocab, tok)) {
            break;
        }

        if (!token_to_piece(h->vocab, tok, piece)) {
            llama_sampler_free(chain);
            write_err(err_buf, err_len, "token_to_piece failed");
            return -1;
        }

        if (!piece.empty()) {
            if (!cb(piece.data(), (int32_t) piece.size(), user_data)) {
                llama_sampler_free(chain);
                return 1;
            }
        }

        llama_token t = tok;
        batch = llama_batch_get_one(&t, 1);
        const int32_t rc = llama_decode(h->ctx, batch);
        if (rc < 0) {
            llama_sampler_free(chain);
            write_err(err_buf, err_len, "llama_decode(token) failed");
            return -1;
        }
    }

    llama_sampler_free(chain);
    return 0;
}
