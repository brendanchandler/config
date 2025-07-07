;; Site specific configuration, e.g. at home or work you can have
;; different versions of this file.

(use-package gptel
  :config
  (setq gptel-model "claudesonnet4"
        gptel-backend
        (gptel-make-openai "Argo"
          :host "argo-bridge.cels.anl.gov"  ; your custom host
          :endpoint "/chat/completions"   ; standard OpenAI endpoint
          :stream t
          :key ""                ; your API key
          :models '("gpt35" "gpt35large" "gpt4" "gpt4large" "gpt4turbo" "gpt4o" "gpt4olatest" "gpto1preview" "gpto1mini" "gpto3mini" "gpto1" "gpto3" "gpto4mini" "gpt41" "gpt41mini" "gpt41nano" "gemini25pro" "gemini25flash" "claudeopus4" "claudesonnet4" "claudesonnet37" "claudesonnet35v2"))))
