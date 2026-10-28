(vl-load-com)

(defun c:sq ( / entQuadra vlaQuadra coords lstPoints ssLotes i entLote vlaLote
                          pt1 pt2 cp1 cp2 d1 d2 total_len fwd_d2 reverse_dir lstLotes
                          lstIntersecs j ptContato cpContato d_pt fwd_dist dist
                          minDist maxDist minpt maxpt ptTexto n totalLotes item numDist textNum numInicial)
  
  ;; 1. Selecionar a Quadra
  (setq entQuadra (car (entsel "\nSelecione a polilinha da QUADRA: ")))
  (if (not entQuadra) (exit))
  (setq vlaQuadra (vlax-ename->vla-object entQuadra))
  
  ;; 2. Seleção Automática dos Lotes dentro da Quadra
  (setq coords (vlax-get vlaQuadra 'Coordinates))
  (setq lstPoints nil)
  (while coords
    (setq lstPoints (cons (list (car coords) (cadr coords)) lstPoints))
    (setq coords (cddr coords))
  )
  (setq lstPoints (reverse lstPoints))
  
  ;; Seleciona lotes cruzando o perímetro
  (setq ssLotes (ssget "_CP" lstPoints '((0 . "LWPOLYLINE"))))
  (if ssLotes (ssdel entQuadra ssLotes))
  
  (if (or (not ssLotes) (= (sslength ssLotes) 0))
    (progn
      (princ "\nNenhum lote encontrado dentro desta quadra.")
      (exit)
    )
  )
  
  ;; 3. Redefinir Vértice Inicial e Direção
  (setq pt1 (getpoint "\nClique no Vértice 1 (Origem da numeração na quadra): "))
  (setq pt2 (getpoint pt1 "\nClique no Vértice 2 (Direção do sentido de medição): "))
  
  (setq total_len (vlax-curve-getDistAtParam vlaQuadra (vlax-curve-getEndParam vlaQuadra)))
  (setq cp1 (vlax-curve-getClosestPointTo vlaQuadra pt1))
  (setq cp2 (vlax-curve-getClosestPointTo vlaQuadra pt2))
  (setq d1 (vlax-curve-getDistAtPoint vlaQuadra cp1))
  (setq d2 (vlax-curve-getDistAtPoint vlaQuadra cp2))
  
  (setq fwd_d2 (if (>= d2 d1) (- d2 d1) (+ (- total_len d1) d2)))
  (if (> fwd_d2 (/ total_len 2.0))
    (setq reverse_dir t)
    (setq reverse_dir nil)
  )
  
  (setq lstLotes nil)
  (setq i 0)
  
  ;; 4. Processar Início e Fim de cada Lote
  (while (< i (sslength ssLotes))
    (setq entLote (ssname ssLotes i))
    (setq vlaLote (vlax-ename->vla-object entLote))
    
    (setq lstIntersecs (vlax-invoke vlaLote 'IntersectWith vlaQuadra acextendnone))
    
    (if lstIntersecs
      (progn
        (setq minDist 1e99) ; Valor inicial alto
        (setq maxDist -1.0) ; Valor inicial baixo
        
        (setq j 0)
        (while (< j (length lstIntersecs))
          (setq ptContato (list (nth j lstIntersecs) (nth (+ j 1) lstIntersecs) (nth (+ j 2) lstIntersecs)))
          (setq cpContato (vlax-curve-getClosestPointTo vlaQuadra ptContato))
          (setq d_pt (vlax-curve-getDistAtPoint vlaQuadra cpContato))
          
          (setq fwd_dist (if (>= d_pt d1) (- d_pt d1) (+ (- total_len d1) d_pt)))
          (setq dist (if reverse_dir 
                       (if (equal fwd_dist 0.0 1e-4) 0.0 (- total_len fwd_dist))
                       fwd_dist))
          
          ;; Identifica onde o lote começa e onde ele termina
          (if (< dist minDist) (setq minDist dist))
          
          ;; Impede que o maxDist pegue acidentalmente o "fim da linha fechada" se for próximo do início
          (if (and (> dist maxDist) (< dist (- total_len 0.1))) 
            (setq maxDist dist)
          )
          
          (setq j (+ j 3))
        )
        
        ;; Ponto central para o texto
        (vla-GetBoundingBox vlaLote 'minpt 'maxpt)
        (setq minpt (vlax-safearray->list minpt)
              maxpt (vlax-safearray->list maxpt))
        (setq ptTexto (list (/ (+ (car minpt) (car maxpt)) 2.0)
                            (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                            0.0))
        
        (if (and (< minDist 1e98) (> maxDist -0.1))
          (setq lstLotes (append lstLotes (list (list minDist maxDist ptTexto))))
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; 5. Ordenar pela distância inicial (Primeiro vértice de cada lote)
  (setq lstLotes (vl-sort lstLotes (function (lambda (a b) (< (car a) (car b))))))
  
  ;; 5.1 Perguntar o número inicial da numeração
  (setq numInicial (getint "\nInforme o número inicial da numeração <1>: "))
  (if (not numInicial) (setq numInicial 1))
  
  ;; 6. Atribuir a Numeração Sequencial a partir do número informado
  (setq n 0)
  (setq totalLotes (length lstLotes))
  
  (while (< n totalLotes)
    (setq item (nth n lstLotes))
    (setq ptTexto (caddr item))
    
    ;; REGRA DE NUMERAÇÃO: sequencial a partir de numInicial
    (setq textNum (itoa (+ numInicial n)))
    
    (entmake
      (list
        '(0 . "TEXT")
        '(100 . "AcDbEntity")
        '(100 . "AcDbText")
        (cons 10 ptTexto)     
        (cons 11 ptTexto)     
        '(72 . 1)             
        '(73 . 2)             
        (cons 40 2.0)         
        (cons 1 textNum)      
        '(50 . 0.0)           
      )
    )
    (setq n (1+ n))
  )
  
  (princ "\nNumeração sequencial inserida a partir do número informado.")
  (princ)
)