;;; -------------------------------------------------------------------------
;;; COMANDO: sy2
;;; DESCRICAO:
;;;   Versao Ultra-optimizada para grandes volumes de LWPOLYLINEs.
;;;   - Utiliza Spatial Grid Partitioning (Grade Espacial) inspirado
;;;     na media dimensional (raiz da area de delimitacao).
;;;   - Elimina comparacoes O(N^2), testando apenas vizinhos de quadrante.
;;;
;;;   v6:
;;;   - Fase 0: apaga poligonos degenerados (menos de 3 vertices distintos
;;;     ou todos os vertices colineares). Antes eles ficavam no desenho:
;;;     o Clean contava o vertice removido mas nao aplicava, porque sobrava
;;;     menos de 2 vertices.
;;;   - Fase 0: zera a elevacao de poligonos com Elevation <> 0
;;;     (controlado pela variavel zerar_elev).
;;;   - Sync: decide entre "mover vertice" e "criar vertice" pela distancia
;;;     real ate o vertice (tol), e nao mais por 1% do comprimento do
;;;     segmento (que em lado de 40 m arrastava o vertice ate 0.4 m).
;;;   - Clean: so conta vertice removido quando a alteracao e aplicada.
;;; -------------------------------------------------------------------------

(vl-load-com)

(defun SAC:_to-list (v)
  (cond
    ((null v) nil)
    ((listp v) v)
    ((= (type v) 'VARIANT)
     (SAC:_to-list (vlax-variant-value v)))
    ((= (type v) 'SAFEARRAY)
     (vlax-safearray->list v))
    (t nil)
  )
)

(defun SAC:_pt->list (p)
  (SAC:_to-list p)
)

(defun SAC:_coords-list (obj)
  (SAC:_to-list (vlax-get obj 'Coordinates))
)

(defun SAC:_bbox2d (obj / pmin pmax a b)
  (vla-GetBoundingBox obj 'pmin 'pmax)
  (setq a (SAC:_pt->list pmin)
        b (SAC:_pt->list pmax))
  (if (and a b)
    (list
      (min (car a)   (car b))
      (min (cadr a)  (cadr b))
      (max (car a)   (car b))
      (max (cadr a)  (cadr b))
    )
  )
)

(defun SAC:_expand-bbox (box d)
  (list
    (- (car box) d)
    (- (cadr box) d)
    (+ (nth 2 box) d)
    (+ (nth 3 box) d)
  )
)

(defun SAC:_bboxes-intersect-p (box1 box2)
  (not
    (or (< (nth 2 box1) (car box2))
        (> (car box1) (nth 2 box2))
        (< (nth 3 box1) (cadr box2))
        (> (cadr box1) (nth 3 box2))
    )
  )
)

(defun SAC:_sqdist2d (p q / dx dy)
  (setq dx (- (car p)  (car q))
        dy (- (cadr p) (cadr q)))
  (+ (* dx dx) (* dy dy))
)

(defun SAC:_sqdist2d-xy (x1 y1 x2 y2 / dx dy)
  (setq dx (- x1 x2)
        dy (- y1 y2))
  (+ (* dx dx) (* dy dy))
)

(defun SAC:_point-in-bbox-p (pt box)
  (and
    (<= (car box)  (car pt))
    (<= (car pt)   (nth 2 box))
    (<= (cadr box) (cadr pt))
    (<= (cadr pt)  (nth 3 box))
  )
)

(defun SAC:_poly-vertices-wcs (obj ename elev / coords out)
  (setq coords (SAC:_coords-list obj)
        out    '())

  (while (and coords (cdr coords))
    (setq out
           (cons
             (trans (list (car coords) (cadr coords) elev) ename 0)
             out
           )
          coords (cddr coords))
  )
  (reverse out)
)

;;; -------------------------------------------------------------------------
;;; NOVAS FUNCOES (v6): DETECCAO DE POLIGONO DEGENERADO
;;; -------------------------------------------------------------------------

;; Lista de vertices 2D distintos: descarta consecutivos repetidos (fuzz)
;; e o ultimo se for igual ao primeiro.
(defun SAC:_distinct-pts (obj fuzz2 / coords x y out)
  (setq coords (SAC:_coords-list obj)
        out    '())
  (while (and coords (cdr coords))
    (setq x (car coords)
          y (cadr coords))
    (if (or (null out)
            (> (SAC:_sqdist2d-xy x y (car (car out)) (cadr (car out))) fuzz2))
      (setq out (cons (list x y) out))
    )
    (setq coords (cddr coords))
  )
  (setq out (reverse out))
  (if (and (cdr out)
           (<= (SAC:_sqdist2d (car out) (last out)) fuzz2))
    (setq out (reverse (cdr (reverse out))))
  )
  out
)

;; T se todos os pontos estao a menos de tol da reta que liga o primeiro
;; ponto ao ponto mais distante dele.
(defun SAC:_collinear-p (pts tol / p0 pf dmax d dx dy len)
  (setq p0   (car pts)
        pf   nil
        dmax 0.0)
  (foreach p pts
    (if (> (setq d (SAC:_sqdist2d p p0)) dmax)
      (setq dmax d
            pf   p)
    )
  )
  (if (null pf)
    t
    (progn
      (setq dx  (- (car pf)  (car p0))
            dy  (- (cadr pf) (cadr p0))
            len (sqrt dmax))
      (vl-every
        '(lambda (p)
           (<= (abs (/ (- (* dx (- (cadr p) (cadr p0)))
                          (* dy (- (car p)  (car p0))))
                       len))
               tol))
        pts
      )
    )
  )
)

(defun SAC:_has-bulge-p (obj / n k found)
  (setq n     (/ (length (SAC:_coords-list obj)) 2)
        k     0
        found nil)
  (while (and (not found) (< k n))
    (if (not (equal (vla-GetBulge obj k) 0.0 1e-12))
      (setq found t)
    )
    (setq k (1+ k))
  )
  found
)

;; Poligono degenerado = nao delimita area:
;;  - sem arcos: menos de 3 vertices distintos ou todos colineares;
;;  - com arcos: menos de 2 vertices distintos.
(defun SAC:_degenerate-p (obj fuzz / pts)
  (setq pts (SAC:_distinct-pts obj (* fuzz fuzz)))
  (if (SAC:_has-bulge-p obj)
    (< (length pts) 2)
    (or (< (length pts) 3)
        (SAC:_collinear-p pts fuzz))
  )
)

;; Apaga o objeto; devolve o handle se conseguiu, nil se falhou
;; (ex.: layer travado).
(defun SAC:_try-delete (obj / h)
  (setq h (vla-get-Handle obj))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-Delete (list obj)))
    nil
    h
  )
)

(defun SAC:_join (lst sep / out)
  (setq out "")
  (foreach s lst
    (setq out (if (= out "") s (strcat out sep s)))
  )
  out
)

;;; -------------------------------------------------------------------------

(defun SAC:_pd-id     (pd) (car pd))
(defun SAC:_pd-obj    (pd) (cadr pd))
(defun SAC:_pd-ename  (pd) (caddr pd))
(defun SAC:_pd-elev   (pd) (cadddr pd))
(defun SAC:_pd-bbox   (pd) (nth 4 pd))
(defun SAC:_pd-ebbox  (pd) (nth 5 pd))
(defun SAC:_pd-verts  (pd) (nth 6 pd))
(defun SAC:_pd-handle (pd) (nth 7 pd))

(defun SAC:_collect-polydata (ss tol / i ename obj elev bbox ebbox verts handle pd out)
  (setq i   0
        out '())

  (repeat (sslength ss)
    (setq ename  (ssname ss i)
          obj    (vlax-ename->vla-object ename)
          elev   (vla-get-Elevation obj)
          bbox   (SAC:_bbox2d obj)
          ebbox  (SAC:_expand-bbox bbox tol)
          verts  (SAC:_poly-vertices-wcs obj ename elev)
          handle (cdr (assoc 5 (entget ename)))
          pd     (list i obj ename elev bbox ebbox verts handle))

    (setq out (cons pd out)
          i   (1+ i))
  )
  (reverse out)
)

;;; -------------------------------------------------------------------------
;;; NOVAS FUNCOES: SPATIAL GRID PARTITIONING
;;; -------------------------------------------------------------------------

;; 1. Calcula o tamanho do grid baseado na dimensao media (conceito de raiz de area)
(defun SAC:_calc-grid-size (polydata / sum_area w h count gs)
  (setq sum_area 0.0 count (length polydata))
  (foreach pd polydata
    (setq w (- (nth 2 (SAC:_pd-bbox pd)) (car (SAC:_pd-bbox pd)))
          h (- (nth 3 (SAC:_pd-bbox pd)) (cadr (SAC:_pd-bbox pd)))
          sum_area (+ sum_area (* w h)))
  )
  ;; A raiz quadrada da area media serve como o lado ideal de nossa "gaveta"
  (setq gs (* (sqrt (/ sum_area count)) 1.5))
  (if (> gs 1e-6) gs 1.0)
)

;; 2. Constroi as gavetas e guarda os poligonos nelas
(defun SAC:_build-grid (polydata grid_size / grid box startX startY endX endY x y key cell)
  (setq grid nil)
  (foreach pd polydata
    (setq box    (SAC:_pd-ebbox pd)
          startX (fix (/ (car box) grid_size))
          startY (fix (/ (cadr box) grid_size))
          endX   (fix (/ (nth 2 box) grid_size))
          endY   (fix (/ (nth 3 box) grid_size)))

    (setq x startX)
    (while (<= x endX)
      (setq y startY)
      (while (<= y endY)
        (setq key (strcat (itoa x) ":" (itoa y)))
        (if (setq cell (assoc key grid))
          (setq grid (subst (cons key (cons pd (cdr cell))) cell grid))
          (setq grid (cons (list key pd) grid))
        )
        (setq y (1+ y))
      )
      (setq x (1+ x))
    )
  )
  grid
)

;; 3. Busca apenas vizinhos locais nas gavetas que o poligono toca
(defun SAC:_get-grid-candidates (pd grid grid_size / box startX startY endX endY x y key cell out h handle_list)
  (setq box    (SAC:_pd-ebbox pd)
        startX (fix (/ (car box) grid_size))
        startY (fix (/ (cadr box) grid_size))
        endX   (fix (/ (nth 2 box) grid_size))
        endY   (fix (/ (nth 3 box) grid_size))
        out    '()
        handle_list '())

  (setq x startX)
  (while (<= x endX)
    (setq y startY)
    (while (<= y endY)
      (setq key (strcat (itoa x) ":" (itoa y)))
      (if (setq cell (assoc key grid))
        (foreach pdB (cdr cell)
          (setq h (SAC:_pd-handle pdB))
          ;; Evita adicionar o proprio poligono e evita duplicados se dividirem multiplas gavetas
          (if (and (/= (SAC:_pd-handle pd) h)
                   (not (vl-position h handle_list))
                   (SAC:_bboxes-intersect-p box (SAC:_pd-ebbox pdB)))
            (setq out         (cons pdB out)
                  handle_list (cons h handle_list))
          )
        )
      )
      (setq y (1+ y))
    )
    (setq x (1+ x))
  )
  out
)

;;; -------------------------------------------------------------------------

(defun SAC:_make-2d-point-array (pt / sa)
  (setq sa (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill sa (list (car pt) (cadr pt)))
  sa
)

(defun SAC:_sync-one-poly (pdA candidates tol tol2 / added moved ptA pdB objB enameB ptClose
                                pt_target_OCS sa param idx num_verts coordsB vx vy)
  (setq added 0
        moved 0)

  (foreach ptA (SAC:_pd-verts pdA)
    (foreach pdB candidates
      (if (SAC:_point-in-bbox-p ptA (SAC:_pd-ebbox pdB))
        (progn
          (setq objB    (SAC:_pd-obj pdB)
                ptClose (vlax-curve-getClosestPointTo objB ptA))

          (if (and ptClose (< (SAC:_sqdist2d ptA ptClose) tol2))
            (progn
              (setq enameB        (SAC:_pd-ename pdB)
                    pt_target_OCS (trans ptA 0 enameB)
                    sa            (SAC:_make-2d-point-array pt_target_OCS)
                    param         (vlax-curve-getParamAtPoint objB ptClose)
                    coordsB       (SAC:_coords-list objB))

              (if (and param coordsB)
                (progn
                  (setq num_verts (/ (length coordsB) 2)
                        idx       (rem (fix (+ param 0.5)) num_verts)
                        vx        (nth (* 2 idx) coordsB)
                        vy        (nth (1+ (* 2 idx)) coordsB))

                  ;; v6: se o ponto de A esta a menos de tol do vertice mais
                  ;; proximo de B, move esse vertice; senao cria vertice novo.
                  (if (< (SAC:_sqdist2d-xy (car pt_target_OCS) (cadr pt_target_OCS) vx vy) tol2)
                    (progn
                      (vla-put-Coordinate objB idx sa)
                      (setq moved (1+ moved))
                    )
                    (progn
                      (vla-AddVertex objB (min (1+ (fix param)) num_verts) sa)
                      (setq added (1+ added))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (list added moved)
)

(defun SAC:_apply-flat-data (obj flat bulges / sa k)
  (if (and flat (>= (length flat) 4))
    (progn
      (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length flat)))))
      (vlax-safearray-fill sa flat)
      (vla-put-Coordinates obj sa)

      (setq k 0)
      (foreach b bulges
        (vla-SetBulge obj k b)
        (setq k (1+ k))
      )
    )
  )
)

(defun SAC:_clean-one-poly-fast (obj fuzz2 / coords cur idx x y bulge prevx prevy
                                       flat_rev bulge_rev removed flat bulges len
                                       firstx firsty lastx lasty)
  (setq removed   0
        coords    (SAC:_coords-list obj)
        flat_rev  '()
        bulge_rev '()
        prevx     nil
        prevy     nil
        idx       0)

  (if (and coords (>= (length coords) 4))
    (progn
      (setq cur coords)
      (while (and cur (cdr cur))
        (setq x (car cur)
              y (cadr cur)
              bulge (vla-GetBulge obj idx))

        (if (or (null prevx)
                (> (SAC:_sqdist2d-xy x y prevx prevy) fuzz2))
          (progn
            (setq flat_rev  (cons y (cons x flat_rev))
                  bulge_rev (cons bulge bulge_rev)
                  prevx     x
                  prevy     y)
          )
          (setq removed (1+ removed))
        )

        (setq cur (cddr cur)
              idx (1+ idx))
      )

      (setq flat   (reverse flat_rev)
            bulges (reverse bulge_rev))

      (if (and (= :vlax-true (vla-get-Closed obj))
               flat
               (> (/ (length flat) 2) 2))
        (progn
          (setq len    (length flat)
                firstx (car flat)
                firsty (cadr flat)
                lastx  (nth (- len 2) flat)
                lasty  (nth (1- len) flat))

          (if (<= (SAC:_sqdist2d-xy firstx firsty lastx lasty) fuzz2)
            (progn
              (setq flat   (reverse (cddr (reverse flat)))
                    bulges (reverse (cdr  (reverse bulges)))
                    removed (1+ removed))
            )
          )
        )
      )

      ;; v6: so conta o que foi realmente aplicado
      (if (and (> removed 0) flat (>= (length flat) 4))
        (SAC:_apply-flat-data obj flat bulges)
        (setq removed 0)
      )
    )
  )

  removed
)

(defun c:sy2 (/ *error* doc undo_started old_cmdecho old_nomutt old_regen
                         tol fuzz tol2 fuzz2 filter ss ss_ok raw_polydata
                         grid_size spatial_grid zerar_elev
                         i ename obj elev
                         total total_added total_moved total_removed processed
                         total_sync_err total_clean_err total_degen total_elev
                         degen_handles lock_handles
                         pdA res pd result msg handle candidates)

  (vl-load-com)

  (defun *error* (msg)
    (if undo_started
      (vla-EndUndoMark doc)
    )

    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_nomutt  (setvar "NOMUTT"  old_nomutt))
    (if old_regen   (setvar "REGENMODE" old_regen))

    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\nErro: " msg))
    )
    (princ)
  )

  (setq doc            (vla-get-ActiveDocument (vlax-get-acad-object))
        undo_started   nil
        tol            0.05
        fuzz           0.001
        tol2           (* tol tol)
        fuzz2          (* fuzz fuzz)
        zerar_elev     t      ; nil = nao mexe na elevacao
        filter         '((0 . "LWPOLYLINE") (-4 . "&") (70 . 1))
        total          0
        total_added    0
        total_moved    0
        total_removed  0
        total_sync_err 0
        total_clean_err 0
        total_degen    0
        total_elev     0
        degen_handles  '()
        lock_handles   '())

  (princ "\nSelecione as LWPOLYLINEs fechadas para Sincronizar e Limpar: ")

  (if (setq ss (ssget filter))
    (progn
      (setq old_cmdecho (getvar "CMDECHO")
            old_nomutt  (getvar "NOMUTT")
            old_regen   (getvar "REGENMODE"))

      (setvar "CMDECHO" 0)
      (setvar "NOMUTT"  1)
      (setvar "REGENMODE" 0)

      (vla-StartUndoMark doc)
      (setq undo_started t)

      ;; --------------------------------------------------------
      ;; FASE 0 (v6): degenerados e elevacao
      (princ "\nFase 0: Verificando poligonos degenerados e elevacao...")
      (setq ss_ok (ssadd)
            i     0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)
              obj   (vlax-ename->vla-object ename)
              i     (1+ i)
              res   (vl-catch-all-apply 'SAC:_degenerate-p (list obj fuzz)))
        (if (and res (not (vl-catch-all-error-p res)))
          ;; degenerado: apaga e NAO entra na sincronizacao
          (if (setq handle (SAC:_try-delete obj))
            (setq total_degen   (1+ total_degen)
                  degen_handles (cons handle degen_handles))
            (setq lock_handles (cons (vla-get-Handle obj) lock_handles))
          )
          (progn
            (if zerar_elev
              (progn
                (setq elev (vla-get-Elevation obj))
                (if (and (not (equal elev 0.0 1e-9))
                         (not (vl-catch-all-error-p
                                (vl-catch-all-apply 'vla-put-Elevation (list obj 0.0)))))
                  (setq total_elev (1+ total_elev))
                )
              )
            )
            (ssadd ename ss_ok)
          )
        )
      )
      (setq ss ss_ok)
      ;; --------------------------------------------------------

      (if (> (sslength ss) 0)
        (progn
          (setq raw_polydata (SAC:_collect-polydata ss tol)
                total        (length raw_polydata))

          ;; --------------------------------------------------------
          ;; PRE-PROCESSAMENTO ESPACIAL
          (princ "\nCalculando malha espacial de otimizacao...")
          (setq grid_size    (SAC:_calc-grid-size raw_polydata)
                spatial_grid (SAC:_build-grid raw_polydata grid_size))
          ;; --------------------------------------------------------

          (princ (strcat "\nFase 1/2: Sincronizando " (itoa total) " poligonos..."))
          (setq processed 0)

          (foreach pdA raw_polydata
            ;; Aqui injetamos apenas os poligonos na mesma "gaveta"
            (setq candidates (SAC:_get-grid-candidates pdA spatial_grid grid_size))

            (setq result (vl-catch-all-apply
                           'SAC:_sync-one-poly
                           (list pdA candidates tol tol2)))

            (if (vl-catch-all-error-p result)
              (progn
                (setq total_sync_err (1+ total_sync_err)
                      handle         (SAC:_pd-handle pdA)
                      msg            (vl-catch-all-error-message result))
                (princ (strcat "\n  Sync erro no handle " handle ": " msg))
              )
              (progn
                (setq total_added (+ total_added (car result))
                      total_moved (+ total_moved (cadr result)))
              )
            )

            (setq processed (1+ processed))
            (if (= 0 (rem processed 100))
              (progn
                (princ (strcat "\n  Sync: " (itoa processed) "/" (itoa total)))
                (gc)
              )
            )
          )

          (princ (strcat "\nFase 2/2: Limpando " (itoa total) " poligonos..."))
          (setq processed 0)

          (foreach pd raw_polydata
            (setq result (vl-catch-all-apply
                           'SAC:_clean-one-poly-fast
                           (list (SAC:_pd-obj pd) fuzz2)))

            (if (vl-catch-all-error-p result)
              (progn
                (setq total_clean_err (1+ total_clean_err)
                      handle          (SAC:_pd-handle pd)
                      msg             (vl-catch-all-error-message result))
                (princ (strcat "\n  Clean erro no handle " handle ": " msg))
              )
              (setq total_removed (+ total_removed result))
            )

            ;; v6: se a limpeza deixou o poligono degenerado, apaga
            (setq obj (SAC:_pd-obj pd))
            (if (and (not (vlax-erased-p obj))
                     (setq res (vl-catch-all-apply 'SAC:_degenerate-p (list obj fuzz)))
                     (not (vl-catch-all-error-p res)))
              (if (setq handle (SAC:_try-delete obj))
                (setq total_degen   (1+ total_degen)
                      degen_handles (cons handle degen_handles))
                (setq lock_handles (cons (vla-get-Handle obj) lock_handles))
              )
            )

            (setq processed (1+ processed))
            (if (= 0 (rem processed 200))
              (progn
                (princ (strcat "\n  Clean: " (itoa processed) "/" (itoa total)))
                (gc)
              )
            )
          )
        )
      )

      (vla-EndUndoMark doc)
      (setq undo_started nil)

      (setvar "CMDECHO" old_cmdecho)
      (setvar "NOMUTT"  old_nomutt)
      (setvar "REGENMODE" old_regen)

      (if degen_handles
        (princ (strcat "\nPoligonos degenerados apagados (handles): "
                       (SAC:_join (reverse degen_handles) ", ")))
      )
      (if lock_handles
        (princ (strcat "\nDegenerados que NAO puderam ser apagados (layer travado?): "
                       (SAC:_join (reverse lock_handles) ", ")))
      )

      (alert
        (strcat
          "Processo concluido!"
          "\n----------------------------------"
          "\nPoligonos processados: " (itoa total)
          "\nDegenerados apagados: " (itoa total_degen)
          "\nElevacao zerada: " (itoa total_elev)
          "\nVertices criados (Sync): " (itoa total_added)
          "\nVertices ajustados (Snap): " (itoa total_moved)
          "\nVertices removidos (Clean): " (itoa total_removed)
          "\nErros na Sync: " (itoa total_sync_err)
          "\nErros no Clean: " (itoa total_clean_err)
        )
      )
    )
    (princ "\nNenhuma LWPOLYLINE fechada selecionada.")
  )
  (princ)
)

(princ "\nComando 'sy2' v6 (Spatial Grid + degenerados) carregado com sucesso.")
(princ)
