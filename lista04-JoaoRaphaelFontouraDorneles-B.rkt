;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista4-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Nome: João Raphael Fontoura Dorneles

;; ========================================================================
;;                                 QUESTÃO 1
;; ========================================================================

;; Ações especiais
(define PULA_VEZ 10) ;; bloqueio
(define COMPRA2 11)  ;; +2
(define INVERTE 12)  ;; troca de sentido horário e anti horário

;; Ações especiais curinga (cor preta)
(define CURINGA 13)         ;; troca de cor
(define CURINGA_COMPRA4 14) ;; troca de cor e +4

;; ========================================================================
;;                                 QUESTÃO 2
;; ========================================================================

;; -----------------
;; TIPO CARTA:
;; -----------------

(define-struct carta (cor valor))

;; Um elemento do conjunto carta é uma estrutura (make-carta cor-var
;; valor-var), onde:
;; cor-var: String, é a cor da carta, que pode ser "verde", "amarelo",
;;          "vermelho", "azul", "preto" (cartas curinga) ou "livre"
;;          (representa nenhuma carta);
;; valor-var: Número, é o número de uma carta, sendo de 0 a 9 para ações
;;            normais, de 10 a 12 para ações especiais e 13 e 14 para ações
;;            especiais curinga.

;; Exemplos de constantes do tipo Carta:
(define VERDE_4 (make-carta "verde" 4))
(define AZUL_7 (make-carta "azul" 7))
(define PRETO_CURINGA (make-carta "preto" CURINGA))
(define PRETO_COMPRA4 (make-carta "preto" CURINGA_COMPRA4))


;; ========================================================================
;;                                 QUESTÃO 3
;; ========================================================================

;; jogada-válida? : Carta Carta -> Booleano
;; Objetivo: dadas duas cartas, sendo uma a carta da mesa e a outra a carta
;;           da mão, nessa ordem, verifica se é possível jogar a carta da
;;           mão na mesa, de acordo com as regras do jogo UNO.
;; Exemplos:
;;          (jogada-válida? (make-carta "Verde" 4)
;;            (make-carta "Verde" 8)
;;            ) = true
;;          (jogada-válida? (make-carta "Amarelo" 7)
;;            (make-carta "Vermelho" 7)
;;            ) = true
;;          (jogada-válida? (make-carta "Preto" CURINGA)
;;            (make-carta "Amarelo" 5)
;;            ) = true
;;          (jogada-válida? (make-carta "Verde" 0)
;;            (make-carta "Preto" CURINGA_COMPRA4)
;;            ) = true
;;          (jogada-válida? (make-carta "Vermelho" 5)
;;            (make-carta "Amarelo" 3)
;;            ) = false

(define (jogada-válida? carta-mesa carta-mao)
  (cond
    [
     (or
      ;; se a carta da mesa for uma carta especial preta,
      ;; uma carta de qualquer cor pode ser jogada
      (string=? "preto" (carta-cor carta-mesa))
      ;; se a carta da mão for uma carta especial preta, ela sempre
      ;; pode ser jogada na mesa
      (string=? "preto" (carta-cor carta-mao))
      ;; se a carta da mão for da mesma cor que a carta da mesa, ela pode
      ;; ser jogada
      (string=? (carta-cor carta-mao) (carta-cor carta-mesa))
      ;; se a carta da mão for do mesmo número que a carta da mesa, ela pode
      ;; ser jogada
      (= (carta-valor carta-mao) (carta-valor carta-mesa))
      )
     true
     ]
    ;; caso contrário
    [else false]
    )
  )

;; Testes:

(check-expect (jogada-válida? (make-carta "preto" CURINGA)
                              (make-carta "preto" CURINGA))
              true)

(check-expect (jogada-válida? (make-carta "vermelho" COMPRA2)
                              (make-carta "preto" CURINGA))
              true)

(check-expect (jogada-válida? (make-carta "preto" CURINGA)
                              (make-carta "amarelo" 7))
              true)

(check-expect (jogada-válida? (make-carta "verde" COMPRA2)
                              (make-carta "verde" INVERTE))
              true)

(check-expect (jogada-válida? (make-carta "amarelo" COMPRA2)
                              (make-carta "verde" INVERTE))
              false)

(check-expect (jogada-válida? (make-carta "verde" 2)
                              (make-carta "vermelho" 3))
              false)

;; ========================================================================
;;                                 QUESTÃO 4
;; ========================================================================

;; -----------------
;; TIPO MÃO:
;; -----------------
(define-struct mão (carta1 carta2 carta3 carta4))
;; Um elemento do conjunto Mão é uma estrutura (make-mão carta1-var
;; carta2-var carta3-var carta4-var), onde:
;; carta1-var: Carta, é a primeira carta na mão do jogador;
;; carta2-var: Carta, é a segunda carta na mão do jogador;
;; carta3-var: Carta, é a terceira carta na mão do jogador;
;; carta4-var: Carta, é a quarta carta na mão do jogador.

;; Constante LIVRE:

(define LIVRE (make-carta "livre" -1))

;; Exemplos de constantes do tipo Mão:

(define MÃO1 (make-mão (make-carta "vermelho" 2)
                           (make-carta "amarelo" 2)
                           (make-carta "preto" CURINGA)
                           (make-carta "vermelho" 7)))

(define MÃO2 (make-mão (make-carta "azul" 3)
                           (make-carta "amarelo" 5)
                           (make-carta "preto" CURINGA_COMPRA4)
                           (make-carta "amarelo" 3)))

(define MÃO3 (make-mão (make-carta "preto" CURINGA)
                           LIVRE
                           (make-carta "preto" CURINGA_COMPRA4)
                           LIVRE))

(define MÃO4 (make-mão (make-carta "amarelo" 9)
                           LIVRE
                           LIVRE
                           LIVRE))

;; ========================================================================
;;                                 QUESTÃO 5
;; ========================================================================

;; livre? : Carta -> Booleano
;; Objetivo: dado um elemento do conjunto Carta, retorna se
;;           esse elemento é uma carta vazia/livre.
;; Exemplos:
;;          (livre? (make-carta "vermelho" 5)) = false
;;          (livre? LIVRE) = true

(define (livre? carta-var)
  (cond
    ;; Se a cor da carta for igual a "livre", ela é uma carta vazia
    [(string=? (carta-cor carta-var) "livre") true]

    ;; Caso contrário, é uma carta válida/existente
    [else false])
  )

;; Testes:
(check-expect (livre? (make-carta "vermelho" 5)) false)
(check-expect (livre? (make-carta "amarelo" 0)) false)
(check-expect (livre? (make-carta "azul" 7)) false)
(check-expect (livre? LIVRE) true)

;; conta-cartas : Mão -> Número
;; Objetivo: dada uma mão, conta quantas cartas há nela e devolve
;;           esse número.
;; Exemplos:
;;          (conta-cartas (make-mao LIVRE LIVRE LIVRE LIVRE)) = 0

;;          (conta-cartas (make-mao LIVRE LIVRE
;;                                  (make-carta "vermelho" 5)
;;                                  (make-carta "vermelho" 7))) = 2

;;          (conta-cartas (make-mao LIVRE
;;                                  (make-carta "preto" CURINGA)
;;                                  (make-carta "vermelho" 5)
;;                                  (make-carta "vermelho" 7))) = 3

;;          (conta-cartas (make-mao (make-carta "preto" CURINGA_COMPRA4)
;;                                  (make-carta "preto" CURINGA)
;;                                  (make-carta "vermelho" 5)
;;                                  (make-carta "vermelho" 7))) = 4

(define (conta-cartas mão-var)
  (+
   (cond
     ;; se a cor da carta1 for "livre", essa carta representa o vazio, logo
     ;; nada é somado na contagem de cartas na mão
     [(livre? (mão-carta1 mão-var)) 0]
     [else 1]
     )
   (cond
     ;; se a cor da carta2 for "livre", essa carta representa o vazio, logo
     ;; nada é somado na contagem de cartas na mão
     [(livre? (mão-carta2 mão-var)) 0]
     [else 1]
     )
   (cond
     ;; se a cor da carta3 for "livre", essa carta representa o vazio, logo
     ;; nada é somado na contagem de cartas na mão
     [(livre? (mão-carta3 mão-var)) 0]
     [else 1]
     )
   (cond
     ;; se a cor da carta4 for "livre", essa carta representa o vazio, logo
     ;; nada é somado na contagem de cartas na mão
     [(livre? (mão-carta4 mão-var)) 0]
     [else 1]
     )
   )
  )

;; Testes:

(check-expect (conta-cartas MÃO1) 4)
(check-expect (conta-cartas MÃO2) 4)
(check-expect (conta-cartas MÃO3) 2)
(check-expect (conta-cartas MÃO4) 1)

(check-expect (conta-cartas (make-mão
                             (make-carta "verde" 5)
                             (make-carta "verde" 7)
                             (make-carta "amarelo" COMPRA2)
                             LIVRE))3)


;; ========================================================================
;;                                 QUESTÃO 6
;; ========================================================================

;; -----------------
;; TIPO RESULTADO:
;; -----------------

;; Um elemento do conjunto Resultado é uma estrutura (make-resultado
;; jogada-var mensagem-var), onde:
;; jogada-var: Carta, é a próxima carta a ser jogada;
;; mensagem-var: String, é o estado do jogo após a jogada da carta.

(define-struct resultado (jogada mensagem))

;; define-jogada : Mão Carta -> Resultado
;; Objetivo: dado uma mão e a carta que está na mesa, devolve um resultado.
;; Exemplos:
;;          (define-jogada MÃO1 (make-carta "verde" 5)) =
;;          (make-resultado (make-carta "preto" 13) "Segue o jogo")

;;          (define-jogada MÃO2 (make-carta "azul" 7)) =
;;          (make-resultado (make-carta "azul" 3) "Segue o jogo")

;;          (define-jogada MÃO3 (make-carta "vermelho" 1)) =
;;          (make-resultado (make-carta "preto" 13) "UNO")

;;          (define-jogada MÃO4 (make-carta "amarelo" 7)) =
;;          (make-resultado (make-carta "amarelo" 9) "Ganhei")
;;

;;          (define-jogada
;;            (make-mão LIVRE LIVRE LIVRE (make-carta "vermelho" 8))
;;            ((make-carta "azul" 7))) =
;;          (make-resultado (make-carta "azul" 3) "Segue o jogo")


(define (define-jogada mão-var carta-var)
  (make-resultado
   ;; jogada
   (cond
     ;; se não houver nenhuma carta válida para jogar
     [(not (or
            (jogada-válida? carta-var (mão-carta1 mão-var))
            (jogada-válida? carta-var (mão-carta2 mão-var))
            (jogada-válida? carta-var (mão-carta3 mão-var))
            (jogada-válida? carta-var (mão-carta4 mão-var))
            )) LIVRE]
     
     ;; se a carta 1 for válida
     [(jogada-válida? carta-var (mão-carta1 mão-var))
      (mão-carta1 mão-var)]

     ;; se a carta 2 for válida
     [(jogada-válida? carta-var (mão-carta2 mão-var))
      (mão-carta2 mão-var)]

     ;; se a carta 3 for válida
     [(jogada-válida? carta-var (mão-carta3 mão-var))
      (mão-carta3 mão-var)]

     ;; se a carta 4 for válida
     [(jogada-válida? carta-var (mão-carta4 mão-var))
      (mão-carta4 mão-var)]

     )
   ;; mensagem
   (cond

     ;; se não houver nenhuma carta válida para jogar ou o número
     ;; de cartas após a jogada for igual maior que 1
     [(or
       (not (or
             (jogada-válida? carta-var (mão-carta1 mão-var))
             (jogada-válida? carta-var (mão-carta2 mão-var))
             (jogada-válida? carta-var (mão-carta3 mão-var))
             (jogada-válida? carta-var (mão-carta4 mão-var))
               ))
       (> (- (conta-cartas mão-var) 1) 1)
       )
      "Segue o jogo"]
          
     ;; se o número de cartas após a jogada for igual a 0
     [(= (- (conta-cartas mão-var) 1) 0)
      "Ganhei"]

     ;; se o número de cartas após a jogada for igual a 1
     [(= (- (conta-cartas mão-var) 1) 1)
      "UNO"]
     )
   )
  )

;; Testes:

(check-expect
 (define-jogada
   (make-mão LIVRE LIVRE LIVRE
             (make-carta "vermelho" 8))
   (make-carta "vermelho" 5)
   )
 (make-resultado (make-carta "vermelho" 8) "Ganhei")
 )

(check-expect
 (define-jogada
   (make-mão LIVRE LIVRE LIVRE
             (make-carta "vermelho" 7))
   (make-carta "verde" 6)
   )
 (make-resultado LIVRE "Segue o jogo")
 )

(check-expect
 (define-jogada
   (make-mão (make-carta "amarelo" 9) LIVRE LIVRE
             (make-carta "vermelho" 7))
   (make-carta "azul" 7)
   )
 (make-resultado (make-carta "vermelho" 7) "UNO")
 )

(check-expect
 (define-jogada
   (make-mão (make-carta "amarelo" 9) (make-carta "azul" 7) LIVRE
             (make-carta "vermelho" 7))
   (make-carta "verde" 6)
   )
 (make-resultado LIVRE "Segue o jogo")
 )

(check-expect
 (define-jogada
   (make-mão (make-carta "amarelo" 9) (make-carta "azul" 7)
             (make-carta "amarelo" 0) (make-carta "vermelho" 7))
   (make-carta "verde" 6)
   )
 (make-resultado LIVRE "Segue o jogo")
 )


;; ========================================================================
;;                                 QUESTÃO 7 
;; ========================================================================

;; converte-cor : Carta -> String
;; Objetivo: dada uma carta, devolve o nome da sua cor em inglês.
;; Exemplos:
;;          (converte-cor (make-carta "verde" 1)) = "green"
;;          (converte-cor (make-carta "amarelo" 1)) = "yellow"
;;          (converte-cor (make-carta "vermelho" 1)) = "red"
;;          (converte-cor (make-carta "azul" 1)) = "blue"
;;          (converte-cor (make-carta "preto" CURINGA)) = "black"
;;          (converte-cor LIVRE) = "transparent"

(define (converte-cor carta-var)
  (cond
    [(string=? (carta-cor carta-var) "verde") "green"]
    [(string=? (carta-cor carta-var) "amarelo") "gold"]
    [(string=? (carta-cor carta-var) "vermelho") "red"]
    [(string=? (carta-cor carta-var) "azul") "blue"]
    [(string=? (carta-cor carta-var) "preto") "black"]
    [(string=? (carta-cor carta-var) "livre") "transparent"]
    )
  )

;; Testes:
(check-expect (converte-cor (make-carta "verde" 1)) "green")
(check-expect (converte-cor (make-carta "amarelo" 2)) "gold")
(check-expect (converte-cor (make-carta "vermelho" 3)) "red")
(check-expect (converte-cor (make-carta "azul" 4)) "blue")
(check-expect (converte-cor (make-carta "preto" CURINGA)) "black")
(check-expect (converte-cor LIVRE) "transparent")

;; desenha-carta : Carta -> Imagem
;; Objetivo: dada uma carta, devolve a imagem dela.

(define (desenha-carta carta-var)
  (overlay
   ;; número
   (text
    ;; texto a ser exibido
    (cond
      ;; se o valor da carta pertencer ao conjunto [0,9]
      [(and
        (>= (carta-valor carta-var) 0)
        (<= (carta-valor carta-var) 9)
        ) (number->string (carta-valor carta-var))]
      ;; se não for uma carta (livre)
      [(= (carta-valor carta-var) (carta-valor LIVRE)) "LIVRE"]
      ;; se for uma carta de pular a vez (bloqueio)
      [(= (carta-valor carta-var) PULA_VEZ) "Ø"]
      ;; se for uma carta de compra 2
      [(= (carta-valor carta-var) COMPRA2) "+2"]
      ;; se for uma carta de inversão
      [(= (carta-valor carta-var) INVERTE) "<<"]
      ;; se for uma carta curinga
      [(= (carta-valor carta-var) CURINGA) "Curinga"]
      ;; se for uma carta curinga de compra 4
      [(= (carta-valor carta-var) CURINGA_COMPRA4) "+4"]
      )
    ;; tamanho do texto
    (cond
      ;; se o valor da carta pertencer ao conjunto [0,9], for uma carta
      ;; de bloqueio, uma carta de comprar 2 ou uma carta curinga de
      ;; comprar 4, o tamanho do texto deverá ser maior
      [(or
        (and
         (>= (carta-valor carta-var) 0)
         (<= (carta-valor carta-var) 9)
         )
        (= (carta-valor carta-var) PULA_VEZ)
        (= (carta-valor carta-var) COMPRA2)
        (= (carta-valor carta-var) CURINGA_COMPRA4)
        )
       40
       ]
      ;; caso contrário, o texto será menor, visando que não exceda os
      ;; limites da bolinha
      [else 18]
      )
    "black")
   ;; bolinha
   (circle 34 "solid" "white")
   ;; carta em si
   (rectangle 75 120 "solid" (converte-cor carta-var))
   ;; borda da carta
   (rectangle 82 127 "outline" "black")
   )
  )

;; Testes:
(check-expect (desenha-carta (make-carta "vermelho" 7))
              (overlay
               (text "7" 40 "black")
               (circle 34 "solid" "white")
               (rectangle 75 120 "solid" "red")
               (rectangle 82 127 "outline" "black")
               )
              )

(check-expect (desenha-carta (make-carta "amarelo" 0))
              (overlay
               (text "0" 40 "black")
               (circle 34 "solid" "white")
               (rectangle 75 120 "solid" "gold")
               (rectangle 82 127 "outline" "black")
               )
              )

(check-expect (desenha-carta LIVRE)
              (overlay
               (text "LIVRE" 18 "black")
               (circle 34 "solid" "white")
               (rectangle 75 120 "solid" "transparent")
               (rectangle 82 127 "outline" "black")
               )
              )

(check-expect (desenha-carta PRETO_COMPRA4)
              (overlay
               (text "+4" 40 "black")
               (circle 34 "solid" "white")
               (rectangle 75 120 "solid" "black")
               (rectangle 82 127 "outline" "black")
               )
              )

;; ========================================================================
;;                                 QUESTÃO 8 (DESAFIO)
;; =========================================================================

;; mostra-jogada : Mão Carta -> Imagem
;; Objetivo: dado uma mão e a carta da mesa, devolve uma imagem mostrando
;;           a mão, a carta da mesa, a carta selecionada para jogar e uma
;;           mensagem sobre o estado do jogo.

(define (mostra-jogada mão-var carta-var)
  (above
   ;; mão
   (beside
    (desenha-carta (mão-carta1 mão-var))
    (desenha-carta (mão-carta2 mão-var))
    (desenha-carta (mão-carta3 mão-var))
    (desenha-carta (mão-carta4 mão-var))
    )
   ;; espaço
   (circle 7 "solid" "transparent")
   ;; mesa
   (overlay
    (desenha-carta carta-var)
    (circle 100 "solid" "brown")
    )
   ;; espaço
   (circle 7 "solid" "transparent")
   ;; carta selecionada e estado do jogo
   (above
    (text "Carta selecionada: " 18 "black")
    (desenha-carta (resultado-jogada (define-jogada mão-var carta-var)))
    (text
     (resultado-mensagem (define-jogada mão-var carta-var))
     16 "black")
    )
   )
  )

;; Testes:
(check-expect
 (mostra-jogada MÃO1 (make-carta "vermelho" 0))
 (above
   (beside
    (desenha-carta (make-carta "vermelho" 2))
    (desenha-carta (make-carta "amarelo" 2))
    (desenha-carta (make-carta "preto" CURINGA))
    (desenha-carta (make-carta "vermelho" 7))
    )
   (circle 7 "solid" "transparent")
   (overlay
    (desenha-carta (make-carta "vermelho" 0))
    (circle 100 "solid" "brown")
    )
   (circle 7 "solid" "transparent")
   (above
    (text "Carta selecionada: " 18 "black")
    (desenha-carta (resultado-jogada (define-jogada MÃO1 (make-carta "vermelho" 0))))
    (text
     (resultado-mensagem (define-jogada MÃO1 (make-carta "vermelho" 0)))
     16 "black")
    )
   )
  )

(check-expect
 (mostra-jogada MÃO2 (make-carta "amarelo" 1))
 (above
   (beside
    (desenha-carta (make-carta "azul" 3))
    (desenha-carta (make-carta "amarelo" 5))
    (desenha-carta (make-carta "preto" CURINGA_COMPRA4))
    (desenha-carta (make-carta "amarelo" 3))
    )
   (circle 7 "solid" "transparent")
   (overlay
    (desenha-carta (make-carta "amarelo" 1))
    (circle 100 "solid" "brown")
    )
   (circle 7 "solid" "transparent")
   ;; carta selecionada e estado do jogo
   (above
    (text "Carta selecionada: " 18 "black")
    (desenha-carta (resultado-jogada (define-jogada MÃO2 (make-carta "amarelo" 1))))
    (text
     (resultado-mensagem (define-jogada MÃO2 (make-carta "amarelo" 1)))
     16 "black")
    )
   )
  )