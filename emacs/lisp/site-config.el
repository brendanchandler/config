;; Site specific configuration, e.g. at home or work you can have
;; different versions of this file.

(use-package gptel
  :config
  (setq gptel-model "gpto1"
        gptel-backend
        (gptel-make-openai "Argo"
          :host "argo-bridge.cels.anl.gov"  ; your custom host
          :endpoint "/chat/completions"   ; standard OpenAI endpoint
          :stream t
          :key ""                ; your API key
          :models '("gpto1"))))
