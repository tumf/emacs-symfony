(define-derived-mode symfony-mode php-mode "sf"
  "Major mode for editing symfony codes.\n\n\\{symfony-mode-map}"
  
  )

(define-derived-mode symfony-class-mode symfony-mode "sf-c"
  "Major mode for editing symfony classes.\n\n\\{symfony-class-mode-map}"
  
  )

(define-derived-mode symfony-template-mode symfony-mode "sf-t"
  "Major mode for editing symfony templates.\n\n\\{symfony-template-mode-map}"
  
  )

(define-derived-mode symfony-yaml-mode yaml-mode "sf-y"
  "Major mode for editing symfony yamls.\n\n\\{symfony-yaml-mode-map}"
  
  )
