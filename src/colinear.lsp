;;; =====================================================================
;;;  COLINEAR.lsp
;;;  Identifica vertices COLINEARES (desnecessarios) em polilinhas
;;;  e marca cada um deles com um circulo.
;;;
;;;  NAO marca o vertice quando existe OUTRO VERTICE SOBREPOSTO nele
;;;  (vertice de outra polilinha/linha na mesma coordenada) - esses
;;;  vertices sao pontos de divisa e devem ser mantidos.
;;;
;;;  Comandos:
;;;    COLINEAR        -> analisa as polilinhas e marca os vertices
;;;    LIMPACOLINEAR   -> apaga todos os circulos de marcacao criados
;;;
;;;  Observacoes:
;;;    - Funciona com LWPOLYLINE e POLYLINE (2D).
;;;    - Segmentos em arco (bulge <> 0) sao ignorados.
;;;    - Polilinha fechada: o primeiro/ultimo vertice tambem e testado.
;;;    - Tolerancia dada em GRAUS (desvio angular em relacao a reta).
;;; =====================================================================

;;; ------------------ configuracoes -----------------------------------
(setq *CL-LAYER* "VERT_COLINEAR")  ; layer dos circulos de marcacao
(setq *CL-FUZZ*  0.001)  ; distancia maxima p/ considerar vertices sobrepostos
(setq *CL-CELL*  1.000)  ; tamanho da celula da busca (>= *CL-FUZZ*)
(setq *CL-PONTOS*     nil) ; T = entidade POINT tambem conta como vertice sobreposto
(setq *CL-DUPLICADOS* nil) ; T = marca tambem vertices duplicados da propria polilinha

;;; ------------------ utilitarios -------------------------------------
(defun cl:layer (nome cor)
  (if (not (tblsearch "LAYER" nome))
    (entmakex (list '(0 . "LAYER")
                    '(100 . "AcDbSymbolTableRecord")
                    '(100 . "AcDbLayerTableRecord")
                    (cons 2 nome)
                    '(70 . 0)
                    (cons 62 cor)
                    '(6 . "Continuous")))
  )
  nome
)

;; normaliza angulo para o intervalo (-pi, pi]
(defun cl:norm (a)
  (while (>  a pi)     (setq a (- a (* 2.0 pi))))
  (while (<= a (- pi)) (setq a (+ a (* 2.0 pi))))
  a
)

;; lista ((ponto bulge) ...) de uma LWPOLYLINE  (coordenadas em OCS)
(defun cl:pts-lw (el / lst)
  (foreach x el
    (cond
      ((= 10 (car x)) (setq lst (cons (list (cdr x) 0.0) lst)))
      ((and (= 42 (car x)) lst)
       (setq lst (cons (list (car (car lst)) (cdr x)) (cdr lst))))
    )
  )
  (reverse lst)
)

;; lista ((ponto bulge) ...) de uma POLYLINE antiga (vertices como entidades)
(defun cl:pts-pl (en / e d f lst)
  (setq e (entnext en))
  (while (and e (setq d (entget e)) (/= "SEQEND" (cdr (assoc 0 d))))
    (if (= "VERTEX" (cdr (assoc 0 d)))
      (progn
        (setq f (cdr (assoc 70 d)))
        (if (or (null f) (zerop (logand f 17)))   ; ignora vertices de ajuste/spline
          (setq lst (cons (list (cdr (assoc 10 d))
                                (if (assoc 42 d) (cdr (assoc 42 d)) 0.0))
                          lst))
        )
      )
    )
    (setq e (entnext e))
  )
  (reverse lst)
)

;; cria o circulo de marcacao (centro em OCS da entidade analisada)
(defun cl:circulo (pt r ext z)
  (entmakex (list '(0 . "CIRCLE")
                  '(100 . "AcDbEntity")
                  (cons 8 *CL-LAYER*)
                  '(100 . "AcDbCircle")
                  (cons 10 (list (car pt) (cadr pt)
                                 (cond ((caddr pt)) (z) (0.0))))
                  (cons 40 r)
                  (cons 210 ext)))
)

;;; ------------------ "hash" espacial de vertices ----------------------
;;; Guarda todos os vertices do desenho em celulas, para saber rapidamente
;;; se existe outro vertice sobreposto a um ponto qualquer.

(defun cl:key (ix iy) (read (strcat "CLH" (itoa ix) "_" (itoa iy))))

(defun cl:hash-limpa ()
  (foreach s *CL-SYMS* (set s nil))
  (setq *CL-SYMS* nil)
)

(defun cl:hash-add (p / ix iy s)
  (setq ix (fix (/ (car  p) *CL-CELL*))
        iy (fix (/ (cadr p) *CL-CELL*))
        s  (cl:key ix iy))
  (if (null (eval s)) (setq *CL-SYMS* (cons s *CL-SYMS*)))
  (set s (cons p (eval s)))
)

;; quantos vertices existem sobre o ponto p (inclui o proprio)
(defun cl:hash-conta (p / n ix iy dx dy)
  (setq n  0
        ix (fix (/ (car  p) *CL-CELL*))
        iy (fix (/ (cadr p) *CL-CELL*))
        dx -1)
  (while (<= dx 1)
    (setq dy -1)
    (while (<= dy 1)
      (foreach q (eval (cl:key (+ ix dx) (+ iy dy)))
        (if (and (< (abs (- (car  q) (car  p))) *CL-FUZZ*)
                 (< (abs (- (cadr q) (cadr p))) *CL-FUZZ*))
          (setq n (1+ n))
        )
      )
      (setq dy (1+ dy))
    )
    (setq dx (1+ dx))
  )
  n
)

;; percorre o desenho (aba atual) e carrega o hash com todos os vertices
(defun cl:monta-hash ( / ss i en el tipo elev pts flt)
  (cl:hash-limpa)
  (setq flt (if *CL-PONTOS*
              "LWPOLYLINE,POLYLINE,LINE,POINT"
              "LWPOLYLINE,POLYLINE,LINE"))
  (if (setq ss (ssget "_X" (list (cons 0 flt) (cons 410 (getvar "CTAB")))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en   (ssname ss i)
              el   (entget en)
              tipo (cdr (assoc 0 el))
              elev (cond ((cdr (assoc 38 el))) (0.0)))
        (cond
          ((= tipo "LINE")
           (cl:hash-add (trans (cdr (assoc 10 el)) en 0))
           (cl:hash-add (trans (cdr (assoc 11 el)) en 0)))
          ((= tipo "POINT")
           (cl:hash-add (trans (cdr (assoc 10 el)) en 0)))
          ((= tipo "LWPOLYLINE")
           (foreach v (cl:pts-lw el)
             (cl:hash-add (trans (list (car (car v)) (cadr (car v)) elev) en 0))))
          ((= tipo "POLYLINE")
           (foreach v (cl:pts-pl en)
             (cl:hash-add (trans (car v) en 0))))
        )
        (setq i (1+ i))
      )
    )
  )
)

;;; ------------------ comando principal --------------------------------
(defun c:COLINEAR ( / ss i en el tipo pts n fech ext elev tol tolr raio ech
                      k a b c ba bb ang pw total totdup totpula marcados)

  (princ "\nSelecione as polilinhas a verificar <ENTER = todas da aba atual>: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  (if (null ss)
    (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE,POLYLINE")
                               (cons 410 (getvar "CTAB")))))
  )

  (if (null ss)
    (progn (princ "\nNenhuma polilinha encontrada.") (princ))
    (progn
      ;; --- parametros ---
      (if (null *CL-TOL*)  (setq *CL-TOL*  1.0))
      (if (null *CL-RAIO*) (setq *CL-RAIO* 0.50))

      (initget 6)
      (if (setq tol (getreal (strcat "\nTolerancia angular em graus <"
                                     (rtos *CL-TOL* 2 3) ">: ")))
        (setq *CL-TOL* tol))
      (setq tol *CL-TOL* tolr (* pi (/ tol 180.0)))

      (initget 6)
      (if (setq raio (getdist (strcat "\nRaio do circulo de marcacao <"
                                      (rtos *CL-RAIO* 2 3) ">: ")))
        (setq *CL-RAIO* raio))
      (setq raio *CL-RAIO*)

      (princ "\nLendo os vertices do desenho...")
      (cl:monta-hash)

      (cl:layer *CL-LAYER* 1)   ; 1 = vermelho
      (setq ech (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.UNDO" "_BE")

      (setq i 0 total 0 totdup 0 totpula 0 marcados 0)
      (while (< i (sslength ss))
        (setq en   (ssname ss i)
              el   (entget en)
              tipo (cdr (assoc 0 el))
              ext  (cond ((cdr (assoc 210 el))) ('(0.0 0.0 1.0)))
              elev (cond ((cdr (assoc 38 el))) (0.0))
              fech (= 1 (logand 1 (cond ((cdr (assoc 70 el))) (0))))
              pts  (if (= tipo "LWPOLYLINE") (cl:pts-lw el) (cl:pts-pl en))
              n    (length pts))

        (if (>= n 3)
          (progn
            (setq k 0)
            (while (< k n)
              ;; so testa o vertice se existir anterior e posterior
              (if (or fech (and (> k 0) (< k (1- n))))
                (progn
                  (setq a  (car (nth (rem (+ k n -1) n) pts))   ; vertice anterior
                        b  (car (nth k pts))                    ; vertice testado
                        c  (car (nth (rem (1+ k) n) pts))       ; vertice seguinte
                        ba (cadr (nth (rem (+ k n -1) n) pts))  ; bulge do segmento a->b
                        bb (cadr (nth k pts)))                  ; bulge do segmento b->c

                  ;; ponto testado em WCS (para procurar vertices sobrepostos)
                  (setq pw (trans (list (car b) (cadr b)
                                        (cond ((caddr b)) (elev)))
                                  en 0))

                  (cond
                    ;; vertice duplicado (segmento de comprimento nulo)
                    ((or (< (distance a b) 1e-8) (< (distance b c) 1e-8))
                     (if *CL-DUPLICADOS*
                       (progn (cl:circulo b raio ext elev)
                              (setq totdup (1+ totdup) marcados (1+ marcados)))
                     )
                    )

                    ;; os dois segmentos precisam ser retos
                    ((and (equal ba 0.0 1e-12) (equal bb 0.0 1e-12))
                     (setq ang (abs (cl:norm (- (angle b c) (angle a b)))))
                     (if (< ang tolr)
                       (if (> (cl:hash-conta pw) 1)
                         ;; existe outro vertice sobreposto -> nao marca
                         (setq totpula (1+ totpula))
                         (progn
                           (cl:circulo b raio ext elev)
                           (setq total (1+ total) marcados (1+ marcados))
                         )
                       )
                     )
                    )
                  )
                )
              )
              (setq k (1+ k))
            )
          )
        )
        (setq i (1+ i))
      )

      (command "_.UNDO" "_E")
      (setvar "CMDECHO" ech)
      (cl:hash-limpa)

      (princ (strcat "\n--------------------------------------------"
                     "\nPolilinhas analisadas      : " (itoa (sslength ss))
                     "\nVertices colineares marcados: " (itoa total)
                     "\nIgnorados (vertice sobreposto): " (itoa totpula)
                     (if *CL-DUPLICADOS*
                       (strcat "\nVertices duplicados        : " (itoa totdup))
                       "")
                     "\nCirculos criados           : " (itoa marcados)
                     " (layer " *CL-LAYER* ", raio " (rtos raio 2 3) ")"
                     "\nUse LIMPACOLINEAR para apagar as marcacoes."
                     "\n--------------------------------------------"))
    )
  )
  (princ)
)

;;; ------------------ apagar as marcacoes ------------------------------
(defun c:LIMPACOLINEAR ( / ss)
  (if (setq ss (ssget "_X" (list '(0 . "CIRCLE") (cons 8 *CL-LAYER*))))
    (progn
      (command "_.ERASE" ss "")
      (princ (strcat "\n" (itoa (sslength ss)) " marcacao(oes) apagada(s)."))
    )
    (princ "\nNenhuma marcacao encontrada.")
  )
  (princ)
)

(princ "\nCOLINEAR.lsp carregado.  Comandos: COLINEAR | LIMPACOLINEAR\n")
(princ)