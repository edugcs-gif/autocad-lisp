(defun c:totularSOLO ( / ss i ent pt x y z layer txtSize lstLayers currentNum yOffTop yOffMid yOffBot xOff userInput format-num)
  (vl-load-com)
  
  ;; Subfunção para formatar números com separador de milhar (.) e decimal (,)
  (defun format-num (val / str pos whole frac res)
    (setq str (rtos val 2 3))
    (setq pos (vl-string-search "." str))
    (if (not pos)
      (setq whole str frac "")
      (setq whole (substr str 1 pos) frac (substr str (+ pos 2)))
    )
    (setq res "")
    (while (> (strlen whole) 3)
      (setq res (strcat "." (substr whole (- (strlen whole) 2)) res))
      (setq whole (substr whole 1 (- (strlen whole) 3)))
    )
    (setq res (strcat whole res))
    (if (> (strlen frac) 0)
      (strcat res "," frac)
      res
    )
  )
  
  ;; Solicita o tamanho do texto ao usuário (com valor padrão de 1.0)
  ;; Com a entrada dinâmica (F12) ativada no AutoCAD, isso aparecerá como uma janela no cursor
  (setq userInput (getreal "\nDigite o tamanho do texto <1.0>: "))
  (if (not userInput)
    (setq txtSize 1.0)
    (setq txtSize userInput)
  )
  
  ;; Ajuste do espaçamento (Agora TOTALMENTE proporcional ao tamanho do texto escolhido)
  (setq xOff (* txtSize 0.3))              ; Pequeno deslocamento para a direita
  (setq yOffTop (* txtSize 1.0))           ; Posiciona o Número acima do Z (Proporcional)
  (setq yOffMid (* txtSize -0.5))          ; Centraliza o Z na altura do ponto (Proporcional)
  (setq yOffBot (* txtSize -2.0))          ; Posiciona o nome da Layer abaixo do Z (Proporcional)
  
  (setq lstLayers '())
  
  (princ "\nSelecione os pontos que deseja descrever: ")
  (if (setq ss (ssget '((0 . "POINT"))))
    (progn
      (setvar "CMDECHO" 0)
      (command "._undo" "_begin")
      
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        
        ;; Pega as coordenadas e a layer do ponto atual
        (setq pt (cdr (assoc 10 (entget ent))))
        (setq x (car pt)
              y (cadr pt)
              z (caddr pt))
        (setq layer (cdr (assoc 8 (entget ent))))
        
        ;; Gerencia a contagem de números independentes por layer
        (if (assoc layer lstLayers)
          (progn
            (setq currentNum (cdr (assoc layer lstLayers)))
            (setq lstLayers (subst (cons layer (1+ currentNum)) (assoc layer lstLayers) lstLayers))
          )
          (progn
            (setq currentNum 1)
            (setq lstLayers (cons (cons layer 2) lstLayers))
          )
        )
        
        ;; 1. Cria o Texto Superior (Número)
        (entmake
          (list
            '(0 . "TEXT")
            (cons 8 layer) ;; Garante que o texto vai para a MESMA LAYER do ponto
            (cons 10 (list (+ x xOff) (+ y yOffTop) 0.0))
            (cons 40 txtSize) ;; Altura do texto informada pelo usuário
            (cons 1 (itoa currentNum))
          )
        )
        
        ;; 2. Cria o Texto do Meio (Cota Z Formatada)
        (entmake
          (list
            '(0 . "TEXT")
            (cons 8 layer) ;; Garante que o texto vai para a MESMA LAYER do ponto
            (cons 10 (list (+ x xOff) (+ y yOffMid) 0.0))
            (cons 40 txtSize)
            (cons 1 (format-num z)) ;; Aplica a formatação técnica de pontuação
          )
        )
        
        ;; 3. Cria o Texto Inferior (Nome da Layer)
        (entmake
          (list
            '(0 . "TEXT")
            (cons 8 layer) ;; Garante que o texto vai para a MESMA LAYER do ponto
            (cons 10 (list (+ x xOff) (+ y yOffBot) 0.0))
            (cons 40 txtSize)
            (cons 1 layer)
          )
        )
      )
      
      (command "._undo" "_end")
      (setvar "CMDECHO" 1)
      (princ (strcat "\nSucesso! " (itoa (sslength ss)) " pontos rotulados e separados visualmente."))
    )
    (princ "\nNenhum ponto selecionado.")
  )
  (princ)
)