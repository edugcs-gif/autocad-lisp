;;; ============================================================================
;;;  SELED.lsp  -  Selagem por edificacao
;;; ----------------------------------------------------------------------------
;;;  Identifica quantas edificacoes existem dentro de cada lote (usando o
;;;  CENTROIDE da edificacao) e ajusta o texto de selagem acrescentando o
;;;  sufixo -E1, -E2, -E3 ...  ordenado pela AREA (maior area = E1).
;;;
;;;  - Lote com 1 edificacao  -> o texto existente vira "<selo>-E1"
;;;  - Lote com N edificacoes -> gera N textos, um no centroide de cada
;;;                             edificacao, numerados E1..EN por area.
;;;  - Lote sem edificacao    -> texto NAO e alterado (apenas reportado).
;;;
;;;  COMANDOS:
;;;    SELED        -> processa o desenho (pode ser rodado quantas vezes quiser)
;;;    SELEDLIMPAR  -> desfaz: apaga as copias criadas e remove os sufixos
;;;
;;;  As copias criadas recebem XDATA "SELED", por isso o comando pode ser
;;;  reexecutado sem duplicar textos.
;;; ============================================================================

(vl-load-com)

;;; --------------------------- CONFIGURACAO ----------------------------------
;;; Ajuste os nomes de camada se necessario (aceita curingas * e ?)

(setq *SELED-LAY-LOTE* "_LOTES")             ; poligonos de lote
(setq *SELED-LAY-SELO* "_SELAGEM")           ; textos de selagem

;;; Camadas de edificacao - separe por virgula, sem espaco depois da virgula.
;;; Os curingas (*) cobrem as variacoes de nome e a acentuacao:
;;;   _ED*ALVENARIA   casa com  _ED DE ALVENARIA  e  _EDIFICACAO ALVENARIA
;;;   _EDIFICA*MISTA  casa com  _EDIFICACAO MISTA
;;; Use o comando SELEDCAM para conferir quais camadas do desenho o filtro
;;; esta pegando. Para incluir mais tipos, acrescente uma linha na lista.
(setq *SELED-LAY-EDIF*
  (strcat "_ED*ALVENARIA"
          ",_ED*MISTA"
          ",_ED*MADEIRA"
          ",_ED*PLATIBANDA"
          ",_ED*INACABADA"))
(setq *SELED-SEP*      "-")                  ; separador antes do "E"
(setq *SELED-APP*      "SELED")              ; APPID usado no XDATA

;;; nil = so move o texto quando o lote tem MAIS DE UMA edificacao
;;;       (lote com 1 edificacao mantem o texto onde esta)
;;; T   = sempre centraliza o texto no centroide da edificacao
(setq *SELED-MOVER*    nil)

;;; --------------------------- UTILITARIOS -----------------------------------

(defun seled:regapp ()
  (if (not (tblsearch "APPID" *SELED-APP*)) (regapp *SELED-APP*))
)

(defun seled:xd-get (ent / xd)
  (setq xd (assoc -3 (entget ent (list *SELED-APP*))))
  (if xd (cdr (assoc 1000 (cdr (car (cdr xd))))))
)

(defun seled:xd-put (ent val)
  (seled:regapp)
  (entmod (append (entget ent)
                  (list (list -3 (list *SELED-APP* (cons 1000 val))))))
)

(defun seled:xd-clear (ent)
  (seled:regapp)
  (entmod (append (entget ent) (list (list -3 (list *SELED-APP*)))))
)

;;; Detecta se o segmento que comeca no parametro i e um arco
(defun seled:arcseg-p (obj i / a b m c)
  (setq a (vlax-curve-getPointAtParam obj i)
        b (vlax-curve-getPointAtParam obj (+ i 1.0)))
  (if (and a b)
    (progn
      (setq m (vlax-curve-getPointAtParam obj (+ i 0.5))
            c (mapcar '(lambda (x y) (/ (+ x y) 2.0)) a b))
      (and m (> (distance m c) (* 1e-6 (max 1.0 (distance a b))))))
    nil
  )
)

;;; Lista de pontos que descreve o contorno da entidade
(defun seled:pts (ent / typ obj sp ep i j n pts p)
  (setq typ (cdr (assoc 0 (entget ent)))
        obj (vlax-ename->vla-object ent)
        sp  (vlax-curve-getStartParam obj)
        ep  (vlax-curve-getEndParam obj)
        pts nil)
  (if (member typ '("LWPOLYLINE" "POLYLINE"))
    (progn                                    ; caminha vertice a vertice
      (setq i sp)
      (while (< i ep)
        (setq pts (cons (vlax-curve-getPointAtParam obj i) pts))
        (if (seled:arcseg-p obj i)            ; segmento em arco -> subdivide
          (progn
            (setq j 1)
            (while (< j 8)
              (if (setq p (vlax-curve-getPointAtParam obj (+ i (/ j 8.0))))
                (setq pts (cons p pts)))
              (setq j (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
      (if (not (vlax-curve-isClosed obj))
        (setq pts (cons (vlax-curve-getPointAtParam obj ep) pts)))
    )
    (progn                                    ; circulo / elipse / spline
      (setq n 72 i 0)
      (while (< i n)
        (if (setq p (vlax-curve-getPointAtParam
                      obj (+ sp (* (- ep sp) (/ i (float n))))))
          (setq pts (cons p pts)))
        (setq i (1+ i))
      )
    )
  )
  (reverse pts)
)

;;; Centroide de um poligono (shoelace)
(defun seled:centroid (pts / a cx cy lst p1 p2 f n sx sy)
  (setq a 0.0 cx 0.0 cy 0.0 lst (append pts (list (car pts))))
  (while (cdr lst)
    (setq p1 (car lst) p2 (cadr lst)
          f  (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))
          a  (+ a f)
          cx (+ cx (* f (+ (car p1) (car p2))))
          cy (+ cy (* f (+ (cadr p1) (cadr p2))))
          lst (cdr lst))
  )
  (if (> (abs a) 1e-12)
    (list (/ cx (* 3.0 a)) (/ cy (* 3.0 a)) 0.0)
    (progn                                    ; degenerado -> media simples
      (setq sx 0.0 sy 0.0 n 0)
      (foreach p pts (setq sx (+ sx (car p)) sy (+ sy (cadr p)) n (1+ n)))
      (list (/ sx n) (/ sy n) 0.0)
    )
  )
)

;;; Meio do maior vao interno do poligono na altura y -> (x largura)
(defun seled:span-mid (pts y / lst p1 p2 xi yi xj yj xs bw bx a b)
  (setq lst (append pts (list (car pts))) xs nil)
  (while (cdr lst)
    (setq p1 (car lst) p2 (cadr lst)
          xi (car p1) yi (cadr p1) xj (car p2) yj (cadr p2))
    (if (or (and (> yi y) (<= yj y))
            (and (> yj y) (<= yi y)))
      (setq xs (cons (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi))) xs)))
    (setq lst (cdr lst))
  )
  (setq xs (vl-sort xs '<) bw -1.0 bx nil)
  (while (cdr xs)
    (setq a (car xs) b (cadr xs))
    (if (> (- b a) bw) (setq bw (- b a) bx (/ (+ a b) 2.0)))
    (setq xs (cddr xs))
  )
  (if bx (list bx bw))
)

;;; Ponto representativo GARANTIDAMENTE dentro do poligono.
;;; Usa o centroide; se ele cair fora (edificacao em L, em U, com recorte),
;;; troca pelo meio do maior vao interno - assim a edificacao nunca e
;;; associada ao lote vizinho.
(defun seled:pt-interno (pts / c bb y0 y1 i y r best)
  (setq c (seled:centroid pts))
  (if (seled:inside-p c pts)
    c
    (progn
      (setq bb (seled:bbox pts) y0 (cadr bb) y1 (cadddr bb) best nil i 1)
      (while (< i 40)
        (setq y (+ y0 (* (- y1 y0) (/ i 40.0))))
        (if (setq r (seled:span-mid pts y))
          (if (or (null best) (> (cadr r) (cadr best)))
            (setq best (list (car r) (cadr r) y))))
        (setq i (1+ i))
      )
      (if best (list (car best) (caddr best) 0.0) c)
    )
  )
)

(defun seled:area (ent / a)
  (setq a (vl-catch-all-apply
            '(lambda () (abs (vla-get-Area (vlax-ename->vla-object ent))))))
  (if (vl-catch-all-error-p a) 0.0 a)
)

;;; Caixa envolvente -> (xmin ymin xmax ymax)
(defun seled:bbox (pts / x0 y0 x1 y1)
  (setq x0 (car (car pts)) x1 x0 y0 (cadr (car pts)) y1 y0)
  (foreach p pts
    (setq x0 (min x0 (car p)) x1 (max x1 (car p))
          y0 (min y0 (cadr p)) y1 (max y1 (cadr p))))
  (list x0 y0 x1 y1)
)

(defun seled:in-bbox-p (pt bb)
  (and (>= (car pt)  (- (car bb)   1e-6)) (<= (car pt)  (+ (caddr bb)  1e-6))
       (>= (cadr pt) (- (cadr bb)  1e-6)) (<= (cadr pt) (+ (cadddr bb) 1e-6)))
)

;;; Ponto dentro do poligono? (ray casting)
(defun seled:inside-p (pt pts / x y ins lst p1 p2 xi yi xj yj)
  (setq x (car pt) y (cadr pt) ins nil lst (append pts (list (car pts))))
  (while (cdr lst)
    (setq p1 (car lst) p2 (cadr lst)
          xi (car p1) yi (cadr p1) xj (car p2) yj (cadr p2))
    (if (and (or (and (> yi y) (<= yj y))
                 (and (> yj y) (<= yi y)))
             (< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
      (setq ins (not ins)))
    (setq lst (cdr lst))
  )
  ins
)

;;; Centro visual de um texto (fallback = ponto de insercao)
(defun seled:txt-center (ent / obj mn mx r)
  (setq obj (vlax-ename->vla-object ent))
  (setq r (vl-catch-all-apply
            '(lambda () (vla-GetBoundingBox obj 'mn 'mx) T)))
  (if (and r (not (vl-catch-all-error-p r)) mn mx)
    (mapcar '(lambda (a b) (/ (+ a b) 2.0))
            (vlax-safearray->list mn) (vlax-safearray->list mx))
    (cdr (assoc 10 (entget ent)))
  )
)

;;; Remove sufixo "-E1" / "-E12" do final da string
(defun seled:strip-e (s / L pat)
  (setq L (strlen *SELED-SEP*) pat (strcase (strcat "*" *SELED-SEP* "E")))
  (cond
    ((wcmatch (strcase s) (strcat pat "#"))  (substr s 1 (- (strlen s) (+ L 2))))
    ((wcmatch (strcase s) (strcat pat "##")) (substr s 1 (- (strlen s) (+ L 3))))
    (T s)
  )
)

(defun seled:ss->list (ss / i l)
  (setq i -1 l nil)
  (if ss (repeat (sslength ss) (setq l (cons (ssname ss (setq i (1+ i))) l))))
  (reverse l)
)

(defun seled:filtro (lay tipos)
  (list (cons 8 lay) (cons 0 tipos) (cons 410 (getvar "CTAB")))
)

;;; Devolve o lote (registro) que contem o ponto; se houver sobreposicao,
;;; escolhe o de menor area.
(defun seled:lote-de (pt lotes / cand)
  (setq cand nil)
  (foreach lt lotes
    (if (and (seled:in-bbox-p pt (nth 2 lt))
             (seled:inside-p  pt (nth 1 lt)))
      (setq cand (cons lt cand))))
  (if cand (car (vl-sort cand '(lambda (a b) (< (nth 3 a) (nth 3 b))))))
)

;;; ============================ COMANDO PRINCIPAL =============================
;;; Estruturas:
;;;   lote  = (ename contorno bbox area)
;;;   selo  = (ename textoBase centro)
;;;   edif  = (ename centroide area)
;;;   mapT  = ((loteEname . selo)  ...)
;;;   mapE  = ((loteEname . (edif edif ...)) ...)

(defun c:SELED ( / doc ssL ssE ssT lotes edifs selos ent obj pts cen txt base
                   lt s e mapT mapE lst alvo cel i sufixo cEnt cTxt delta novo
                   nLot nEd total semSelo semLote mover modo cntLay par)

  (vl-load-com)
  (seled:regapp)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  (princ "\n--------------------------------------------------")
  (princ "\nSELED - selagem por edificacao")

  ;; ---------------- 1) LOTES: selecionados, senao desenho inteiro ---------
  (setq ssL (ssget "_I" (seled:filtro *SELED-LAY-LOTE*
                          "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")))
  (if ssL
    (setq modo "APENAS OS LOTES SELECIONADOS")
    (setq ssL  (ssget "_X" (seled:filtro *SELED-LAY-LOTE*
                             "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))
          modo "DESENHO INTEIRO")
  )
  (sssetfirst nil nil)
  (princ (strcat "\nEscopo: " modo))

  (setq lotes nil)
  (foreach ent (seled:ss->list ssL)
    (if (setq pts (seled:pts ent))
      (setq lotes (cons (list ent pts (seled:bbox pts) (seled:area ent)) lotes)))
  )
  (if (null lotes)
    (princ (strcat "\n  ATENCAO: nenhum lote encontrado na camada \""
                   *SELED-LAY-LOTE* "\" - nada sera alterado.")))

  ;; ---------------- 2) EDIFICACOES ----------------
  (setq ssE (ssget "_X" (seled:filtro *SELED-LAY-EDIF*
                          "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))
        edifs nil
        i 0)
  (foreach ent (seled:ss->list ssE)
    (if (setq pts (seled:pts ent))
      (setq edifs (cons (list ent (seled:pt-interno pts) (seled:area ent)
                              (setq i (1+ i)) (cdr (assoc 8 (entget ent))))
                        edifs)))
  )

  ;; ---------------- 3) TEXTOS DE SELAGEM -> LOTE --------------------------
  ;;   So mexe nos textos que caem dentro dos lotes do escopo. Apaga copias
  ;;   de execucoes anteriores e restaura o nome base antes de reprocessar.
  (setq ssT (ssget "_X" (seled:filtro *SELED-LAY-SELO* "TEXT,MTEXT"))
        selos nil mapT nil)
  (foreach ent (seled:ss->list ssT)
    (setq cen  (seled:txt-center ent)
          alvo (seled:lote-de cen lotes))
    (if alvo                                  ; fora do escopo -> nao toca
      (progn
        (setq base (seled:xd-get ent))
        (if (equal base "COPY")
          (entdel ent)                        ; copia de execucao anterior
          (progn
            (setq obj (vlax-ename->vla-object ent))
            (setq txt (if (and base (/= base "")) base
                        (seled:strip-e (vla-get-TextString obj))))
            (if (/= txt (vla-get-TextString obj)) (vla-put-TextString obj txt))
            (setq s     (list ent txt cen)
                  selos (cons s selos))
            (if (not (assoc (car alvo) mapT))
              (setq mapT (cons (cons (car alvo) s) mapT)))
          )
        )
      )
    )
  )

  ;; ---------------- 5) EDIFICACAO -> LOTE ----------------
  (setq mapE nil semLote 0)
  (foreach e edifs
    (if (setq alvo (seled:lote-de (cadr e) lotes))
      (progn
        (setq cel (assoc (car alvo) mapE))
        (if cel
          (setq mapE (subst (cons (car cel) (cons e (cdr cel))) cel mapE))
          (setq mapE (cons (cons (car alvo) (list e)) mapE))))
      (setq semLote (1+ semLote))
    )
  )

  ;; ---------------- 6) APLICA A SELAGEM ----------------
  (setq nLot 0 nEd 0 total 0 semSelo 0 cntLay nil)
  (foreach cel mapE
    (setq lst (cdr cel)
          s   (cdr (assoc (car cel) mapT))
          nLot (1+ nLot)
          nEd  (+ nEd (length lst)))
    ;; maior area primeiro; empate resolvido pela posicao (X e depois Y)
    (setq lst (vl-sort lst
                '(lambda (a b)
                   (cond
                     ((> (- (caddr a) (caddr b)) 1e-6) T)
                     ((> (- (caddr b) (caddr a)) 1e-6) nil)
                     ((> (abs (- (car (cadr a)) (car (cadr b)))) 1e-6)
                      (< (car (cadr a)) (car (cadr b))))
                     ((> (abs (- (cadr (cadr a)) (cadr (cadr b)))) 1e-6)
                      (< (cadr (cadr a)) (cadr (cadr b))))
                     (T (< (nth 3 a) (nth 3 b)))))))
    (setq mover (or *SELED-MOVER* (> (length lst) 1)))
    (if (null s)
      (setq semSelo (1+ semSelo))
      (progn
        (setq i 0)
        (foreach e lst
          (setq i      (1+ i)
                sufixo (strcat (cadr s) *SELED-SEP* "E" (itoa i)))
          (if (= i 1)
            (setq cEnt (car s))                       ; reaproveita o original
            (progn                                    ; cria uma copia
              (setq novo (vla-Copy (vlax-ename->vla-object (car s)))
                    cEnt (vlax-vla-object->ename novo))
              (seled:xd-put cEnt "COPY")
            )
          )
          (if mover
            (progn
              (setq cTxt  (seled:txt-center cEnt)
                    delta (mapcar '- (cadr e) cTxt))
              (vla-Move (vlax-ename->vla-object cEnt)
                        (vlax-3d-point '(0.0 0.0 0.0))
                        (vlax-3d-point (list (car delta) (cadr delta) 0.0)))
            )
          )
          (vla-put-TextString (vlax-ename->vla-object cEnt) sufixo)
          (if (= i 1) (seled:xd-put cEnt (cadr s)))    ; guarda o nome base
          (setq total (1+ total))
          ;; contagem por camada de edificacao
          (setq par (assoc (nth 4 e) cntLay))
          (if par
            (setq cntLay (subst (cons (car par) (1+ (cdr par))) par cntLay))
            (setq cntLay (cons (cons (nth 4 e) 1) cntLay)))
        )
      )
    )
  )

  (vla-EndUndoMark doc)

  ;; ---------------- 7) RELATORIO ----------------
  (princ (strcat "\n  Lotes no escopo ........... " (itoa (length lotes))))
  (princ (strcat "\n  Edificacoes no escopo ..... " (itoa nEd)))
  (princ (strcat "\n  Textos de selagem ......... " (itoa (length selos))))
  (princ (strcat "\n  Lotes com edificacao ...... " (itoa nLot)))
  (princ (strcat "\n  Textos gravados ........... " (itoa total)))
  (if cntLay
    (progn
      (princ "\n  Por tipo de edificacao:")
      (foreach par (vl-sort cntLay '(lambda (a b) (> (cdr a) (cdr b))))
        (princ (strcat "\n     " (car par) " : " (itoa (cdr par)))))
    )
  )
  (if (> semSelo 0)
    (princ (strcat "\n  ATENCAO: " (itoa semSelo)
                   " lote(s) com edificacao mas SEM texto de selagem dentro.")))
  (if (and (= modo "DESENHO INTEIRO") (> semLote 0))
    (princ (strcat "\n  ATENCAO: " (itoa semLote)
                   " edificacao(oes) fora de qualquer lote.")))
  (princ "\n--------------------------------------------------")
  (princ)
)

;;; ============================== DESFAZER ====================================

(defun c:SELEDLIMPAR ( / doc ssL ssT ent obj base n d cur pts lotes modo)
  (vl-load-com)
  (seled:regapp)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; mesmo criterio de escopo do SELED
  (setq ssL (ssget "_I" (seled:filtro *SELED-LAY-LOTE*
                          "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")))
  (if ssL
    (setq modo "APENAS OS LOTES SELECIONADOS")
    (setq ssL  (ssget "_X" (seled:filtro *SELED-LAY-LOTE*
                             "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))
          modo "DESENHO INTEIRO")
  )
  (sssetfirst nil nil)
  (setq lotes nil)
  (foreach ent (seled:ss->list ssL)
    (if (setq pts (seled:pts ent))
      (setq lotes (cons (list ent pts (seled:bbox pts) (seled:area ent)) lotes)))
  )

  (setq ssT (ssget "_X" (seled:filtro *SELED-LAY-SELO* "TEXT,MTEXT")) n 0 d 0)
  (foreach ent (seled:ss->list ssT)
    (if (seled:lote-de (seled:txt-center ent) lotes)
      (progn
        (setq base (seled:xd-get ent))
        (if (equal base "COPY")
          (progn (entdel ent) (setq d (1+ d)))
          (progn
            (setq obj (vlax-ename->vla-object ent)
                  cur (vla-get-TextString obj))
            (cond
              ((and base (/= base ""))
               (vla-put-TextString obj base) (seled:xd-clear ent) (setq n (1+ n)))
              ((/= (seled:strip-e cur) cur)
               (vla-put-TextString obj (seled:strip-e cur)) (setq n (1+ n)))
            )
          )
        )
      )
    )
  )
  (princ (strcat "\nEscopo: " modo))
  (vla-EndUndoMark doc)
  (princ (strcat "\nSELEDLIMPAR: " (itoa d) " copia(s) apagada(s), "
                 (itoa n) " texto(s) restaurado(s)."))
  (princ)
)

;;; ====================== DIAGNOSTICO DE CAMADAS ==============================
;;; Lista as camadas do desenho e mostra quais o filtro esta reconhecendo.

(defun c:SELEDCAM ( / lay nome tipo n)
  (princ "\n--------------------------------------------------")
  (princ "\nSELEDCAM - camadas reconhecidas pelo SELED\n")
  (setq lay (tblnext "LAYER" T) n 0)
  (while lay
    (setq nome (cdr (assoc 2 lay))
          tipo (cond
                 ((wcmatch (strcase nome) (strcase *SELED-LAY-LOTE*)) "LOTE")
                 ((wcmatch (strcase nome) (strcase *SELED-LAY-EDIF*)) "EDIFICACAO")
                 ((wcmatch (strcase nome) (strcase *SELED-LAY-SELO*)) "SELAGEM")
                 (T nil)))
    (if tipo
      (progn (princ (strcat "\n  [" tipo "] " nome)) (setq n (1+ n)))
      (princ (strcat "\n   ...        " nome)))
    (setq lay (tblnext "LAYER"))
  )
  (princ (strcat "\n\n  " (itoa n) " camada(s) reconhecida(s)."))
  (if (= n 0)
    (princ "\n  Nenhuma! Ajuste os nomes no topo do SELED.lsp."))
  (princ "\n--------------------------------------------------")
  (princ)
)

(princ "\nSELED.lsp carregado.  Comandos:  SELED  |  SELEDLIMPAR  |  SELEDCAM\n")
(princ)