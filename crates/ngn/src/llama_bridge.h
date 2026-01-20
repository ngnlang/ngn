// Minimal stable C ABI wrapper around llama.cpp
//
// This exists so the Rust side doesn't have to mirror llama.cpp struct layouts.

#pragma once

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ngn_llama_handle ngn_llama_handle;

typedef struct ngn_llama_load_opts {
    int32_t n_ctx;      // 0 = model default
    int32_t n_threads;  // 0 = llama default
    bool use_mmap;
} ngn_llama_load_opts;

typedef struct ngn_llama_gen_opts {
    int32_t max_tokens;
    float temperature;
    float top_p;
    int32_t top_k;
    uint32_t seed;
} ngn_llama_gen_opts;

// Called for each generated chunk (token piece). Return false to stop generation.
typedef bool (*ngn_llama_chunk_cb)(const char * data, int32_t len, void * user_data);

void ngn_llama_backend_init(void);

// Returns NULL on failure and writes a message into err_buf.
ngn_llama_handle * ngn_llama_load(const char * path, ngn_llama_load_opts opts, char * err_buf, int32_t err_len);
void ngn_llama_free(ngn_llama_handle * h);

// Returns:
//  0 = success
//  1 = cancelled (callback returned false)
// -1 = error (see err_buf)
int32_t ngn_llama_generate(
    ngn_llama_handle * h,
    const char * prompt,
    ngn_llama_gen_opts opts,
    ngn_llama_chunk_cb cb,
    void * user_data,
    char * err_buf,
    int32_t err_len);

#ifdef __cplusplus
}
#endif
