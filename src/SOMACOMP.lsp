;;; SOMACOMP.lsp - Soma o comprimento dos objetos selecionados
;;; Comando: SOMACOMP
;;; Objetos: LINE, ARC, CIRCLE, ELLIPSE, SPLINE, LWPOLYLINE, POLYLINE
;;; Versao:  1.0 (2026-10-28)

(vl-load-com)

(defun somacomp:comprimento (ent)
  (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))
)

(defun c:SOMACOMP (/ ss i ent len total n)
  (setq total 0.0
        n     0)
  (if (setq ss (ssget '((0 . "LINE,ARC,CIRCLE,ELLIPSE,SPLINE,LWPOLYLINE,POLYLINE"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              len (vl-catch-all-apply 'somacomp:comprimento (list ent))
              i   (1+ i))
        ;; malhas (POLYLINE do tipo mesh) nao tem comprimento e sao ignoradas
        (if (numberp len)
          (setq total (+ total len)
                n     (1+ n))
        )
      )
      (princ (strcat "\n" (itoa n) " objeto(s) | Comprimento total: " (rtos total)))
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (princ)
)

(princ "\nSOMACOMP carregado. Digite SOMACOMP para executar.")
(princ)
