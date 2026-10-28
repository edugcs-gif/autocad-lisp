;;; NOME.lsp - Descricao curta do que a rotina faz
;;; Comando: NOME
;;; Versao:  1.0 (AAAA-MM-DD)

(vl-load-com)

(defun c:NOME (/ *error* doc)
  ;; Tratamento de erro: fecha o grupo de UNDO e ignora ESC
  (defun *error* (msg)
    (if doc (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; ... codigo da rotina aqui ...

  (vla-EndUndoMark doc)
  (princ)
)

(princ "\nNOME carregado. Digite NOME para executar.")
(princ)
