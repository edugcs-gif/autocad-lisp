;;; carregar.lsp - Carrega todas as rotinas .lsp da pasta src\
;;;
;;; A pasta do repositorio precisa estar no "Support File Search Path"
;;; do AutoCAD (OPTIONS > Files) para que este arquivo se encontre.

(vl-load-com)

(defun carregar:todos (/ base pasta n)
  (if (setq base (findfile "carregar.lsp"))
    (progn
      (setq pasta (strcat (vl-filename-directory base) "\\src")
            n     0)
      (foreach arq (vl-directory-files pasta "*.lsp" 1)
        (if (vl-catch-all-error-p
              (vl-catch-all-apply 'load (list (strcat pasta "\\" arq))))
          (princ (strcat "\nErro ao carregar: " arq))
          (setq n (1+ n))
        )
      )
      (princ (strcat "\n" (itoa n) " rotina(s) carregada(s) de " pasta))
    )
    (princ "\ncarregar.lsp: adicione a pasta do repositorio ao Support File Search Path.")
  )
  (princ)
)

(carregar:todos)
(princ)
