(defun c:RA (/ ss i en ed pt poly close_pt param deriv ang)
  (vl-load-com)
  
  (prompt "\nSelecione os textos para alinhamento automático: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i)
              ed (entget en)
              pt (cdr (assoc 10 ed)) ;; Ponto de inserção
        )
        
        ;; Busca a polilinha mais próxima do ponto de inserção do texto
        (setq poly (ssget "_C" 
                          (list (- (car pt) 1.0) (- (cadr pt) 1.0)) 
                          (list (+ (car pt) 1.0) (+ (cadr pt) 1.0)) 
                          '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
        
        ;; Se não encontrar nada a 1 metro, tenta uma busca maior (5 metros)
        (if (not poly)
            (setq poly (ssget "_C" 
                          (list (- (car pt) 5.0) (- (cadr pt) 5.0)) 
                          (list (+ (car pt) 5.0) (+ (cadr pt) 5.0)) 
                          '((0 . "LWPOLYLINE,POLYLINE,LINE"))))
        )

        (if poly
          (progn
            (setq poly (ssname poly 0)) ;; Pega a primeira entidade encontrada
            
            ;; Cálculos Geométricos
            (setq close_pt (vlax-curve-getClosestPointTo poly pt))
            (setq param (vlax-curve-getParamAtPoint poly close_pt))
            (setq deriv (vlax-curve-getFirstDeriv poly param))
            (setq ang (atan (cadr deriv) (car deriv)))
            
            ;; Correção de leitura (Up-side down)
            (while (>= ang (/ pi 2)) (setq ang (- ang pi)))
            (while (< ang (/ pi -2)) (setq ang (+ ang pi)))

            ;; Aplica a rotação
            (setq ed (subst (cons 50 ang) (assoc 50 ed) ed))
            (entmod ed)
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nFinalizado: " (itoa i) " textos processados."))
    )
    (princ "\nNenhum texto selecionado.")
  )
  (princ)
)