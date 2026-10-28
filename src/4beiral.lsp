;;; ============================================================
;;; Comando: 4BEIRAL (Beiral para Dentro)
;;; Cria o offset interno de 0.6, mantendo as linhas coladas
;;; na divisa do lote intactas (recuo = 0 nesses trechos).
;;;
;;; Versao revisada: validacoes de entrada, protecao contra
;;; erros de geometria (pontos duplicados, poligono degenerado,
;;; arcos/bulges, selecao repetida) e tratamento de erro (*error*)
;;; para nao deixar o AutoCAD em estado inconsistente se o
;;; usuario cancelar (ESC) no meio do comando.
;;; ============================================================

;; Remove pontos consecutivos duplicados (evita erro na funcao ANGLE
;; quando dois vertices ficam exatamente sobrepostos)
(defun 4beiral:remove-dup (lst / result prev)
  (setq result '())
  (foreach p lst
    (if (or (null prev) (> (distance p prev) 1e-6))
      (setq result (append result (list p)))
    )
    (setq prev p)
  )
  result
)

(defun c:4BEIRAL ( / *error* dist selBldg selLot entBldg entLot objLotRes objLot
                     pts area n i is-ccw normal-dir edges v1 v2 mid closest
                     dist2D d ang beta shiftX shiftY ep1 ep2 newPts prev curr
                     newPt has-bulge flag70 oldError newEnt )

  (vl-load-com)

  ;; ---------- Tratamento de erro / cancelamento ----------
  (setq oldError *error*)
  (defun *error* (msg)
    (setq *error* oldError)
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT")))
        )
      (princ (strcat "\n4BEIRAL - Erro: " msg))
    )
    (princ)
  )

  ;; Define a distancia do offset interno como FIXA em 0.6
  (setq dist 0.6)

  ;; ---------- 0. Selecao da edificacao ----------
  (setq selBldg (entsel "\nSelecione a edificacao: "))
  (cond
    ((null selBldg)
     (princ "\nOperacao cancelada: nenhuma edificacao selecionada.")
    )
    ((not (and (entget (car selBldg))
               (= (cdr (assoc 0 (entget (car selBldg)))) "LWPOLYLINE")))
     (princ "\nErro: o que voce selecionou nao e uma polilinha (LWPOLYLINE) valida para a edificacao.")
    )
    (t
     (setq entBldg (car selBldg))

     ;; ---------- 1. Selecao do lote ----------
     (setq selLot (entsel "\nSelecione o lote: "))
     (cond
       ((null selLot)
        (princ "\nOperacao cancelada: nenhum lote selecionado.")
       )
       ((not (and (entget (car selLot))
                  (= (cdr (assoc 0 (entget (car selLot)))) "LWPOLYLINE")))
        (princ "\nErro: o que voce selecionou nao e uma polilinha (LWPOLYLINE) valida para o lote.")
       )
       ((equal (car selBldg) (car selLot))
        (princ "\nErro: voce selecionou a mesma entidade para edificacao e lote. Selecione objetos diferentes.")
       )
       (t
        (setq entLot (car selLot))

        ;; Obtem o objeto VLA do lote com protecao contra falha
        (setq objLotRes (vl-catch-all-apply 'vlax-ename->vla-object (list entLot)))
        (if (vl-catch-all-error-p objLotRes)
          (progn
            (setq objLot nil)
            (princ (strcat "\nErro: nao foi possivel acessar o objeto do lote (" (vl-catch-all-error-message objLotRes) ")."))
          )
          (setq objLot objLotRes)
        )

        (if objLot
          (progn
            ;; Avisa se o lote nao for uma poliline fechada (o offset assume um perimetro fechado)
            (setq flag70 (cdr (assoc 70 (entget entLot))))
            (if (or (null flag70) (= 0 (logand flag70 1)))
              (princ "\nAviso: a polilinha do lote nao esta marcada como fechada. O resultado pode ficar impreciso.")
            )

            ;; ---------- 2. Extrair vertices da edificacao (apenas X e Y) ----------
            (setq pts (mapcar '(lambda (x) (list (cadr x) (caddr x)))
                              (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget entBldg))))

            ;; Detecta arcos (bulges) na edificacao: serao tratados como segmentos retos (corda)
            (setq has-bulge (vl-some '(lambda (x) (and (= (car x) 42) (/= (cdr x) 0.0))) (entget entBldg)))
            (if has-bulge
              (princ "\nAviso: a edificacao possui trechos em arco (bulge); serao aproximados por linhas retas.")
            )

            ;; Remove vertices duplicados/coincidentes antes de qualquer calculo
            (setq pts (4beiral:remove-dup pts))

            (if (< (length pts) 3)
              (princ "\nErro: a polilinha da edificacao nao tem vertices suficientes (minimo 3) apos a limpeza de pontos duplicados.")
              (progn
                ;; Fecha o poligono na lista, se necessario
                (if (> (distance (car pts) (last pts)) 0.001)
                  (setq pts (append pts (list (car pts))))
                )

                ;; ---------- 3. Calcular sentido (Horario ou Anti-horario) ----------
                (setq area 0.0 n (1- (length pts)) i 0)
                (while (< i n)
                  (setq v1 (nth i pts) v2 (nth (1+ i) pts))
                  (setq area (+ area (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1)))))
                  (setq i (1+ i))
                )
                (setq area (/ area 2.0))

                (if (equal area 0.0 1e-6)
                  (princ "\nErro: os vertices da edificacao sao colineares ou a area e nula; nao e possivel calcular o offset.")
                  (progn
                    (setq is-ccw (> area 0))
                    ;; A direcao da normal para dentro depende do sentido do desenho
                    (setq normal-dir (if is-ccw (/ pi 2.0) (- (/ pi 2.0))))

                    ;; ---------- 4. Calcular os segmentos com offset ----------
                    (setq edges '() i 0)
                    (while (< i n)
                      (setq v1 (nth i pts) v2 (nth (1+ i) pts))
                      (setq mid (list (/ (+ (car v1) (car v2)) 2.0) (/ (+ (cadr v1) (cadr v2)) 2.0) 0.0))

                      ;; Distancia do ponto medio em relacao a divisa do lote (protegida)
                      (setq closest (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list objLot mid)))
                      (if (vl-catch-all-error-p closest)
                        (setq dist2D 999999.0) ;; falha ao calcular -> assume "longe" da divisa, aplica recuo normal
                        (setq dist2D (distance (list (car mid) (cadr mid)) (list (car closest) (cadr closest))))
                      )

                      ;; Se estiver colado na divisa (margem de erro de 0.05), recuo = 0. Senao, recuo = dist
                      (if (< dist2D 0.05)
                        (setq d 0.0)
                        (setq d dist)
                      )

                      ;; Vetor de deslocamento (protegido contra pontos coincidentes)
                      (if (equal v1 v2 1e-9)
                        (setq ang 0.0)
                        (setq ang (angle v1 v2))
                      )
                      (setq beta (+ ang normal-dir))
                      (setq shiftX (* d (cos beta)) shiftY (* d (sin beta)))

                      (setq ep1 (list (+ (car v1) shiftX) (+ (cadr v1) shiftY)))
                      (setq ep2 (list (+ (car v2) shiftX) (+ (cadr v2) shiftY)))
                      (setq edges (append edges (list (list ep1 ep2))))

                      (setq i (1+ i))
                    )

                    ;; ---------- 5. Intersectar segmentos para fechar os cantos ----------
                    (setq newPts '() i 0)
                    (while (< i n)
                      (setq prev (if (= i 0) (last edges) (nth (1- i) edges)))
                      (setq curr (nth i edges))

                      (setq newPt (inters (car prev) (cadr prev) (car curr) (cadr curr) nil))
                      (if (not newPt)
                        (setq newPt (cadr prev)) ;; Seguranca para linhas paralelas
                      )
                      (setq newPts (append newPts (list newPt)))
                      (setq i (1+ i))
                    )

                    ;; ---------- 6. Desenhar a nova polilinha ----------
                    (if (< (length newPts) 3)
                      (princ "\nErro: nao foi possivel gerar um poligono valido com o offset calculado.")
                      (progn
                        (setq newEnt
                          (entmake
                            (append
                              (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                    (cons 8 (cdr (assoc 8 (entget entBldg)))) ;; mesma layer da edificacao original
                                    (cons 90 (length newPts)) '(70 . 1)
                              )
                              (mapcar '(lambda (pt) (cons 10 pt)) newPts)
                            )
                          )
                        )
                        (if newEnt
                          (princ "\nOffset interno de 0.6 gerado com sucesso!")
                          (princ "\nErro: falha ao criar a nova polilinha (ENTMAKE retornou nil).")
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
     )
    )
  )

  ;; Restaura o tratador de erro original
  (setq *error* oldError)
  (princ)
)

(princ "\n4BEIRAL carregado. Digite 4BEIRAL para executar.")
(princ)
