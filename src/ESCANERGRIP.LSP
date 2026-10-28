(defun c:EscanerGrip (/ ss i j ent1 vla1 coords idx pt ptClosest dist minDist erros minFuzz maxFuzz)
  (vl-load-com)
  
  ;; Tolerância Mínima: Menos de 0.1 milímetro (0.0001) a LISP considera que está colado e ignora.
  (setq minFuzz 0.0001) 
  
  ;; Define a "malha fina" do escaner (o padrão é 5 centímetros, mas você pode digitar outro valor na hora)
  (setq maxFuzz (getreal "\nQual a distancia MAXIMA do micro-erro em metros? (Padrão = 0.05 para 5cm): "))
  (if (not maxFuzz) (setq maxFuzz 0.05))

  (prompt "\nSelecione as polilinhas para passar o escaner: ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  
  (if ss
    (progn
      (setq erros nil i 0)
      (princ "\nPassando o escaner nos grips... Aguarde.")
      
      (while (< i (sslength ss))
        (setq ent1 (ssname ss i)
              vla1 (vlax-ename->vla-object ent1)
              coords (vlax-get vla1 'Coordinates)
              idx 0)
        
        ;; Pega cada grip (vértice) da linha atual
        (while (< idx (length coords))
          (setq pt (list (nth idx coords) (nth (1+ idx) coords) 0.0))
          (setq minDist 100000.0)
          (setq j 0)
          
          ;; Mede a distância desse grip para todas as outras linhas selecionadas
          (while (< j (sslength ss))
            (if (/= i j)
              (progn
                (setq ptClosest (vlax-curve-getClosestPointTo (vlax-ename->vla-object (ssname ss j)) pt))
                (if ptClosest
                  (progn
                    (setq dist (distance pt ptClosest))
                    (if (< dist minDist)
                      (setq minDist dist)
                    )
                  )
                )
              )
            )
            (setq j (1+ j))
          )
          
          ;; O PULO DO GATO: Se a distância for maior que zero, mas MENOR que a sua tolerância (ex: 5mm)...
          (if (and (> minDist minFuzz) (<= minDist maxFuzz))
            (setq erros (cons pt erros))
          )
          
          (setq idx (+ idx 2))
        )
        (setq i (1+ i))
      )
      
      (if erros
        (progn
          (princ (strcat "\nESCANER CONCLUIDO! Foram detectados " (itoa (length erros)) " grips com micro-desalinhamentos!"))
          (foreach pt erros
            ;; Desenha o círculo vermelho proporcional ao tamanho do erro
            (entmake (list '(0 . "CIRCLE") (cons 10 pt) (cons 40 (* maxFuzz 10.0)) '(62 . 1)))
            
            ;; Dá o Zoom focado absurdamente perto do micro-erro
            (command "_.ZOOM" "_C" pt (* maxFuzz 50.0))
            
            ;; Pausa para você consertar
            (getstring "\nMicro-erro na tela! Arraste o grip e pressione ENTER para o proximo...")
          )
          (princ "\n==========================================")
          (princ "\nTodos os micro-erros foram varridos!")
          (princ "\n==========================================")
        )
        (princ "\nEscaner limpo! Nao ha nenhum grip causando micro-buracos nessa tolerancia.")
      )
    )
    (princ "\nNada foi selecionado.")
  )
  (princ)
)