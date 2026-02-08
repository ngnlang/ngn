---
title: LLM Inference
description: Use a local model
---

ngn has an LLM toolbox mod, used for inference. You must install ngn with the `-f llm` feature, in order for `tbx::llm` to be available.

```ngn
import { load, stream } from "tbx::llm"

global MODEL = load("./tinyllama.gguf")

fn handler(req: Request): StreamingResponse {
  const prompt = req.body
  const chunks = channel<string>()

  thread(|| {
    match (MODEL) {
      Ok(m) => {
        const llm_ch = stream(m, prompt)
        for (token in <-? llm_ch) {
          if (chunks.isClosed()) {
            llm_ch.close()
            break
          }
          chunks <- token
        }
      },
      Error(e) => chunks <- "Error: ${e}"
    }
    chunks.close()
  })

  return StreamingResponse { body: chunks }
}

export default { fetch: handler }
```
