(defun c:3 (/ p1 p2 p3 p4 ang dist_perp p3_final)
  (setvar "cmdecho" 0)
  
  ;; 1. Coleta os pontos
  (setq p1 (getpoint "\nPrimeiro ponto da base: "))
  (if (setq p2 (getpoint p1 "\nSegundo ponto da base (define inclinação): "))
    (progn
      (setq p3 (getpoint p2 "\nTerceiro ponto (distância da altura): "))
      
      (if p3
        (progn
          ;; 2. Cálculos Geométricos
          (setq ang (angle p1 p2)) ;; Ângulo da base
          
          ;; Calcula a distância perpendicular de P3 em relação à reta P1-P2
          ;; Isso garante que, não importa onde você clique, a altura seja reta
          (setq dist_perp (* (distance p1 p3) (sin (- (angle p1 p3) ang))))
          
          ;; 3. Define os pontos exatos do retângulo (90 graus)
          ;; P3 final é P2 rotacionado 90 graus em relação à base
          (setq p3_final (polar p2 (+ ang (/ pi 2)) dist_perp))
          ;; P4 é P1 rotacionado 90 graus em relação à base
          (setq p4 (polar p1 (+ ang (/ pi 2)) dist_perp))
          
          ;; 4. Desenha o polígono fechado
          (command "_pline" "non" p1 "non" p2 "non" p3_final "non" p4 "_c")
          
          (princ "\nRetângulo de 90° finalizado.")
        )
      )
    )
  )
  (setvar "cmdecho" 1)
  (princ)
)