(vl-load-com)

(defun c:2NUMERARLOTES ( / entQuadra vlaQuadra coords lstPoints ssLotes i entLote vlaLote
                          pt1 pt2 cp1 cp2 d1 d2 total_len fwd_d2 reverse_dir lstLotes
                          lstIntersecs j ptContato cpContato d_pt fwd_dist dist lstDists
                          minDist maxDist minpt maxpt ptTexto n totalLotes item numDist textNum)
  
  ;; 1. Selecionar a Quadra
  (setq entQuadra (car (entsel "\nSelecione a polilinha da QUADRA: ")))
  (if (not entQuadra) (exit))
  (setq vlaQuadra (vlax-ename->vla-object entQuadra))
  
  ;; 2. Seleção Automática dos Lotes
  (setq coords (vlax-get vlaQuadra 'Coordinates))
  (setq lstPoints nil)
  (while coords
    (setq lstPoints (cons (list (car coords) (cadr coords)) lstPoints))
    (setq coords (cddr coords))
  )
  (setq lstPoints (reverse lstPoints))
  
  (setq ssLotes (ssget "_CP" lstPoints '((0 . "LWPOLYLINE"))))
  (if ssLotes (ssdel entQuadra ssLotes))
  (if (or (not ssLotes) (= (sslength ssLotes) 0))
    (progn (princ "\nNenhum lote encontrado.") (exit))
  )
  
  ;; 3. Redefinir Origem e Direção
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
  
  ;; 4. Processar Geometria e Interseções
  (while (< i (sslength ssLotes))
    (setq entLote (ssname ssLotes i))
    (setq vlaLote (vlax-ename->vla-object entLote))
    (setq lstIntersecs (vlax-invoke vlaLote 'IntersectWith vlaQuadra acextendnone))
    
    (if lstIntersecs
      (progn
        (setq lstDists nil)
        (setq j 0)
        ;; Mapear todas as distâncias de contato do lote
        (while (< j (length lstIntersecs))
          (setq ptContato (list (nth j lstIntersecs) (nth (+ j 1) lstIntersecs) (nth (+ j 2) lstIntersecs)))
          (setq cpContato (vlax-curve-getClosestPointTo vlaQuadra ptContato))
          (setq d_pt (vlax-curve-getDistAtPoint vlaQuadra cpContato))
          
          (setq fwd_dist (if (>= d_pt d1) (- d_pt d1) (+ (- total_len d1) d_pt)))
          (setq dist (if reverse_dir 
                       (if (equal fwd_dist 0.0 1e-4) 0.0 (- total_len fwd_dist))
                       fwd_dist))
          
          (if (not (vl-some (function (lambda (x) (equal x dist 1e-3))) lstDists))
            (setq lstDists (cons dist lstDists))
          )
          (setq j (+ j 3))
        )
        
        (if lstDists
          (progn
            (setq minDist (apply 'min lstDists))
            (setq maxDist (apply 'max lstDists))
            
            ;; -------------------------------------------------------------
            ;; O DETECTOR DE QUEBRA (WRAPAROUND FIX)
            ;; Se a distância entre o menor e o maior vértice do lote for
            ;; maior que a metade da quadra, ele cruzou a "costura" do clique.
            ;; Solução: Joga o vértice '0' para o fim matemático da quadra.
            ;; -------------------------------------------------------------
            (if (> (- maxDist minDist) (/ total_len 2.0))
              (progn
                (setq lstDists (mapcar '(lambda (x) (if (< x (/ total_len 2.0)) (+ x total_len) x)) lstDists))
                (setq minDist (apply 'min lstDists))
                (setq maxDist (apply 'max lstDists))
              )
            )
            
            ;; Cálculo do Centro Geométrico para o texto
            (vla-GetBoundingBox vlaLote 'minpt 'maxpt)
            (setq minpt (vlax-safearray->list minpt)
                  maxpt (vlax-safearray->list maxpt))
            (setq ptTexto (list (/ (+ (car minpt) (car maxpt)) 2.0)
                                (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                                0.0))
            
            (setq lstLotes (append lstLotes (list (list minDist maxDist ptTexto))))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; 5. Ordenar a fila de lotes pelo Primeiro Vértice real
  (setq lstLotes (vl-sort lstLotes (function (lambda (a b) (< (car a) (car b))))))
  
  ;; 6. Atribuir Numeração (Regra Híbrida: Lote 1 rouba do Lote 2)
  (setq n 0)
  (setq totalLotes (length lstLotes))
  
  (while (< n totalLotes)
    (setq item (nth n lstLotes))
    (setq ptTexto (caddr item))
    (setq maxDist (cadr item))
    
    (if (and (= n 0) (> totalLotes 1))
      ;; Lote 1: Pega o vértice de início do Lote 2
      (setq numDist (car (nth 1 lstLotes)))
      ;; Demais Lotes: Pegam o seu próprio vértice final
      (setq numDist maxDist)
    )
    
    ;; Regra de Ouro: Nada de Lote 0
    (if (> numDist 0.1)
      (progn
        ;; O 'rtos' devolve a string redonda, sem nenhum ponto ou vírgula
        (setq textNum (rtos numDist 2 0))
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
      )
    )
    (setq n (1+ n))
  )
  
  (princ "\nTopologia corrigida. Numeração executada com sucesso.")
  (princ)
)