;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista6-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 6

;; ========================================================================
;;                        DEFINIÇÕES DE DADOS
;; ========================================================================  

;; CONSTANTES:
(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA_COMPRA4 -4)
(define CURINGA -5)

;; -----------------
;; TIPO CARTA:
;; -----------------
(define-struct carta (cor valor))  
;; Um elemento do conjunto Carta é uma estrutura
;; (make-carta c v), onde:
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo", "vermelho" ou "preto" ou "livre"
;;   v : Número, é o valor da carta, que pode ser qualquer inteiro entre 0 e 9,
;;               ou um número negativo -1 (PulaVez), -2 (Compra2), -3 (Inverte),-4 (Compra4) ou -5 (Curinga)


;; ========================================================================
;;                         FUNÇÕES - Use se quiser
;; ========================================================================

;; jogada-válida? : Carta Carta -> Booleano
;; Objetivo: Dada uma carta da mao e uma da mesa, nesta ordem,
;; verifica se é possível a carta da mão, de acordo com as regras do UNO
;; Exemplos:
;;    (jogada-válida? (make-carta "azul" 3) (make-carta "azul" 4))=  #t 
;;    (jogada-válida? (make-carta "azul" 3) (make-carta "preto" CURINGA_COMPRA4)) = #t 
;;    (jogada-válida? (make-carta "azul" 3) (make-carta "vermelho" 4)) = #f
(define (jogada-válida? carta-mao carta-mesa)
     (cond
          ;; se a carta da mão ou a da mesa for preta, a jogada é válida
          [(or (string=? (carta-cor carta-mao) "preto") (string=? (carta-cor carta-mesa) "preto")) #t]
          ;; se as duas cartas forem da mesma cor ou do mesmo tipo/valor, a jogada é válida
          [(or (string=? (carta-cor carta-mesa)(carta-cor carta-mao))
               (= (carta-valor carta-mesa) (carta-valor carta-mao))) #t]
          ;; senão, a jogada é inválida
          [else #f] ))

;; Testes:
(check-expect (jogada-válida?
               (make-carta "azul" 7)
               (make-carta "vermelho" 8)) false)

(check-expect (jogada-válida?
               (make-carta "azul" 7)
               (make-carta "azul" 8)) true)

(check-expect (jogada-válida?
               (make-carta "preto" CURINGA)
               (make-carta "amarelo" 8)) true)

;; desenha-carta : Carta -> Imagem
;; Objetivo: A função cria uma imagem para a carta que é recebida de entrada
;; Exemplos:
;;     (desenha-carta (make-carta "azul" 3) devolve a imagem da carta de UNO azul 3
;;     (desenha-carta (make-carta "preto" CURINGA_COMPRA4)) devolve a imagem da carta de UNO curinga compra4
(define (desenha-carta carta)
  (overlay ;; sobrepor:    
              (desenha-texto (carta-valor carta))     ;; texto da carta,
              (circle 45 "solid" "white")             ;; círculo branco, 
              (rectangle 100 150 "solid" (cor carta)) ;; retângulo da cor da carta, e  
              (rectangle 110 160 "outline" "black"))) ;; contorno preto

;; desenha-texto : Número -> Imagem
;; Dado um valor, desenha uma imagem de carta de UNO correspondendo a este valor
;; Exemplos:
;;     (desenha-texto 3) devolve a imagem do número 3
;;     (desenha-texto CURINGA_COMPRA4)) devolve a imagem +4
(define (desenha-texto valor)
  (text (cond ;; escolher o símbolo a ser desenhado, dependendo do valor:
               [(= valor CURINGA_COMPRA4) "+4"]
               [(= valor COMPRA2)         "+2"]
               [(= valor INVERTE)         "«"]
               [(= valor PULA_VEZ)        "Ø"]
               [(= valor CURINGA)         "T"]
               [else (number->string valor)])
        ;; o texto será desenhado em tamanho 70 em preto
        70 "black"))

;; cor : Carta -> String
;; Objetivo: a função recebe uma carta UNO, e retorna a respectiva cor
;; em ingles, ou seja, "blue", "green", "yellow", "red" ou "black".
;; Exemplos:
;; (cor? "azul") = "blue"
;; (cor? "verde") = "green"
;; (cor? "rosa") = "black"
(define (cor c)
  (cond
    [(string=? "azul" (carta-cor c)) "blue"]
    [(string=? "verde" (carta-cor c)) "green"]
    [(string=? "vermelho" (carta-cor c)) "red"]
    [(string=? "amarelo" (carta-cor c)) "yellow"]
    [else "black"]))

;; ========================================================================
;;                                 QUESTÕES
;; =========================================================================

;; Todas as funções recursivas devem ter o modelo da solução (pode haver mais casos,
;; dependendo de como o problema está sendo solucionado):

;; Dados ....
;; Se <é o caso base da definição da lista> então <definir como resolver este caso>
;; Se <não é o caso base>
;;    então  <combinar>
;;              <fazer algo com> <o primeiro elemento da lista>
;;              <solucionar o problema para> <o resto da lista>

;; ========================================================================
;;                                 QUESTÃO 1
;; =========================================================================

;; --------------------
;; TIPO LISTA DE CARTAS:
;; --------------------

;; Uma ListaDeCartas é:
;; 1. uma lista vazia empty, ou
;; 2. (cons elemento resto), onde:
;;    elemento: um elemento do conjunto Carta e
;;    resto: ListaDeCartas.

;; Exemplos de constante de ListaDeCartas:
(define L1 (cons (make-carta "preto" CURINGA)
                 (cons (make-carta "preto" CURINGA_COMPRA4)
                                   (cons (make-carta "amarelo" 4)
                                                     (cons (make-carta
                                                            "vermelho"
                                                            5) empty)))))

(define L2 (cons (make-carta "verde" PULA_VEZ)
                 (cons (make-carta "amarelo" PULA_VEZ)
                                   (cons (make-carta "azul" PULA_VEZ)
                                                     (cons (make-carta
                                                            "vermelho"
                                                            PULA_VEZ) empty)))))

(define L3 (cons (make-carta "vermelho" 3)
                 (cons (make-carta "amarelo" 5)
                                   (cons (make-carta "azul" 7)
                                                     (cons (make-carta
                                                            "verde"
                                                            1) empty)))))

(define L4 (cons (make-carta "amarelo" 4)
                 (cons (make-carta "verde" COMPRA2)
                                   (cons (make-carta "azul" 6)
                                                     (cons (make-carta
                                                            "verde"
                                                            INVERTE) empty)))))

(define L5 (cons (make-carta "amarelo" 7)
                 (cons (make-carta "amarelo" 5)
                       (cons (make-carta "amarelo" 2)
                             (cons (make-carta "vermelho" 2)
                                   (cons (make-carta "verde" 5)
                                         (cons (make-carta "vermelho" PULA_VEZ) empty)))))))

;; -----------------
;; TIPO JOGADOR:
;; -----------------
(define-struct jogador (nome mão pontos))
;; Um elemento do conjunto Jogador é uma estrutura
;; (make-jogador n m p), onde:
;; n : String, é o nome do jogador;
;; m : ListaDeCartas, são as cartas que estão em sua mão;
;; p : Número, é a pontuação do jogador.

;; Exemplos de constante de Jogador:
(define J1 (make-jogador "João" L1 109))
(define J2 (make-jogador "Ana" L2 80))
(define J3 (make-jogador "Nicolas" L3 16))
(define J4 (make-jogador "Caroline" L4 50))
(define J5 (make-jogador "Luana" L5 41))

;; ========================================================================
;;                                 QUESTÃO 2
;; ========================================================================

;; desenha-cartas: ListaDeCartas -> Imagem
;; Objetivo: desenha lado a lado as cartas de uma lista de cartas.
;; Exemplos:
;;          (desenha-cartas L1) = (beside
;;                                 (desenha-carta (make-carta "preto" CURINGA))
;;                                 (desenha-carta (make-carta "preto" CURINGA_COMPRA4))
;;                                 (desenha-carta (make-carta "amarelo" 4))
;;                                 (desenha-carta (make-carta "vermelho" 5)))


(define (desenha-cartas L)
  ;; Dada uma lista de cartas L
  (cond
       ;; Se L é a lista vazia então devolve uma imagem vazia
       [(empty? L) empty-image]
       ;; Se L não for vazia
       ;;    então agrupa as  imagens lado a lado
       [else (beside
              ;; desenha a primeira carta da lista
              (desenha-carta (first L))

              ;; desenha a imagem das cartas do resto da lista L
              (desenha-cartas (rest L))
              )]
       )
  )

;; Chamadas da função:
(desenha-cartas L1)
(desenha-cartas L5)

;; ========================================================================
;;                                 QUESTÃO 3
;; ========================================================================

;; número-opções : ListaDeCartas Carta -> Número
;; Objetivo: retorna quantas cartas de uma lista de cartas são válidas para
;;           jogar com a carta que está na mesa.
;; Exemplos:
;;          (número-opções L1 (make-carta "verde" 6)) = 2
;;          (número-opções L1 (make-carta "amarelo" 7)) = 3
;;          (número-opções L1 (make-carta "amarelo" 5)) = 4
;;          (número-opções L2 (make-carta "verde" 5)) = 1
;;          (número-opções L2 (make-carta "verde" PULA_VEZ)) = 4

(define (número-opções L C)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se L estiver vazia, então retorna 0
    [(empty? L) 0]

    ;; Se L tiver elementos, então
    [else
     ;; Soma
     (+
      ;; 1, se o primeiro elemento de L for uma jogada válida
      (cond
        [(jogada-válida? (first L) C) 1]
        ;; Caso contrário, soma 0, pois não é uma jogada válida
        [else 0])

      ;; Com o número de opções do resto de L
      (número-opções (rest L) C)
      )]
    )
  )

;; Testes:
(check-expect (número-opções empty (make-carta "azul" 8)) 0)
(check-expect (número-opções empty (make-carta "preto" CURINGA)) 0)
(check-expect (número-opções L1 (make-carta "verde" 6)) 2)
(check-expect (número-opções L1 (make-carta "amarelo" 7)) 3)
(check-expect (número-opções L1 (make-carta "amarelo" 5)) 4)
(check-expect (número-opções L1 (make-carta "preto" CURINGA)) 4)
(check-expect (número-opções L2 (make-carta "verde" 5)) 1)
(check-expect (número-opções L2 (make-carta "verde" PULA_VEZ)) 4)
(check-expect (número-opções L2 (make-carta "preto" CURINGA)) 4)
(check-expect (número-opções L3 (make-carta "vermelho" 9)) 1)
(check-expect (número-opções L3 (make-carta "amarelo" 4)) 1)
(check-expect (número-opções L3 (make-carta "azul" 2)) 1)
(check-expect (número-opções L3 (make-carta "verde" 0)) 1)
(check-expect (número-opções L3 (make-carta "vermelho" 5)) 2)
(check-expect (número-opções L3 (make-carta "amarelo" 3)) 2)
(check-expect (número-opções L3 (make-carta "azul" 1)) 2)
(check-expect (número-opções L3 (make-carta "verde" 7)) 2)
(check-expect (número-opções L4 (make-carta "vermelho" 9)) 0)
(check-expect (número-opções L4 (make-carta "amarelo" 5)) 1)
(check-expect (número-opções L4 (make-carta "azul" 5)) 1)
(check-expect (número-opções L4 (make-carta "amarelo" COMPRA2)) 2)
(check-expect (número-opções L4 (make-carta "azul" 4)) 2)
(check-expect (número-opções L4 (make-carta "verde" 5)) 2)
(check-expect (número-opções L4 (make-carta "verde" 4)) 3)
(check-expect (número-opções L4 (make-carta "preto" CURINGA_COMPRA4)) 4)

;; ========================================================================
;;                                 QUESTÃO 4
;; ========================================================================

;; soma-pontos : ListaDeCartas -> Número
;; Objetivo: devolve a soma da pontuação de todas as cartas de um conjunto
;;           ListaDeCartas, de acordo com regras do UNO.
;; Exemplos:
;;          (soma-pontos L1) = 109
;;          (soma-pontos L2) = 80
;;          (soma-pontos L3) = 16
;;          (soma-pontos L4) = 50

(define (soma-pontos L)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se L estiver vazia, então retorna 0
    [(empty? L) 0]

    ;; Se L tiver elementos, então
    [else
     ;; Soma
     (+
      ;; O valor do seu primeiro elemento de acordo com os casos
      (cond
        ;; Caso 1: é uma carta númerada, então vale seu valor nominal
        
        ;; Dica 4 da lista 5: Para testar se um valor x está compreendido
        ;; no intervalo entre 1 e 3 (neste caso, fechado), você pode usar
        ;; (<= 1 x 3).
        
        [(<= 0 (carta-valor (first L)) 9) (carta-valor (first L))]

        ;; Caso 2: é uma carta de ação especial, então vale 20
        [(or
          (= (carta-valor (first L)) PULA_VEZ)
          (= (carta-valor (first L)) COMPRA2)
          (= (carta-valor (first L)) INVERTE)) 20]

        ;; Caso 3: é uma carta de ação especial curinga, então vale 50
        [(or
          (= (carta-valor (first L)) CURINGA)
          (= (carta-valor (first L)) CURINGA_COMPRA4)) 50]
        )

      ;; Com a soma dos pontos do resto de L
      (soma-pontos (rest L)))]
    )
  )

;; Testes:
(check-expect (soma-pontos empty) 0)
(check-expect (soma-pontos L1) 109)
(check-expect (soma-pontos L2) 80)
(check-expect (soma-pontos L3) 16)
(check-expect (soma-pontos L4) 50)
(check-expect (soma-pontos (cons (make-carta "preto" CURINGA) empty)) 50)
(check-expect (soma-pontos (cons (make-carta "amarelo" 5) empty)) 5)
(check-expect (soma-pontos (cons (make-carta "vermelho" PULA_VEZ) (cons (make-carta "amarelo" 5) empty))) 25)
(check-expect (soma-pontos (cons (make-carta "vermelho" 0) (cons (make-carta "azul" 0) empty))) 0)
(check-expect (soma-pontos (cons (make-carta "amarelo" 1) (cons (make-carta "azul" 0) empty))) 1)

;; ========================================================================
;;                                 QUESTÃO 5
;; ========================================================================

;; Um elemento do conjunto CartaOuString é
;;        1) uma Carta, ou
;;        2) uma String

;; define-jogada: Jogador Carta -> CartaOuString
;; Objetivo: dado um jogador e a carta da mesa, retorna a primeira carta que
;;           pode ser jogada, se não for possível jogar nenhuma carta retorna
;;           a string "Impossível jogar carta".
;; Exemplos:
;;          (define-jogada J1 (make-carta "amarelo" 5)) =
;;          (make-carta "preto" CURINGA)
;;          (define-jogada J2 (make-carta "amarelo" 5)) =
;;          (make-carta "amarelo" PULA_VEZ)
;;          (define-jogada J3 (make-carta "amarelo" 5)) =
;;          (make-carta "amarelo" 5)
;;          (define-jogada J4 (make-carta "amarelo" 5)) =
;;          (make-carta "amarelo" 4)
;;          (define-jogada J4 (make-carta "vermelho" 7)) =
;;          "Impossível jogar carta"

(define (define-jogada J C)
  (cond
    ;; Se for possível fazer alguma jogada
    [(>= (número-opções (jogador-mão J) C) 1)
     (cond
       ;; Se a primeira carta da mão de J for uma jogada válida, então
       ;; retorna esta carta
       [(jogada-válida? (first (jogador-mão J)) C)
        (first (jogador-mão J))]
       
       ;; Defina a jogada do resto da lista de cartas de J para a carta da mesa C
       [else (define-jogada
               (make-jogador
                (jogador-nome J)
                (rest (jogador-mão J))
                (jogador-pontos J))
               C)]
       )]

    ;; Caso contrário, se não for possível fazer nenhuma jogada, retorna
    ;; uma string informando isso
    [else "Impossível jogar carta"]
    )
  )

;; Testes:
(check-expect (define-jogada J1 (make-carta "amarelo" 5))
              (make-carta "preto" CURINGA))
(check-expect (define-jogada J2 (make-carta "vermelho" PULA_VEZ))
              (make-carta "verde" PULA_VEZ))
(check-expect (define-jogada J2 (make-carta "verde" 5))
              (make-carta "verde" PULA_VEZ))
(check-expect (define-jogada J2 (make-carta "amarelo" 5))
              (make-carta "amarelo" PULA_VEZ))
(check-expect (define-jogada J2 (make-carta "azul" 5))
              (make-carta "azul" PULA_VEZ))
(check-expect (define-jogada J2 (make-carta "vermelho" 5))
              (make-carta "vermelho" PULA_VEZ))
(check-expect (define-jogada J3 (make-carta "vermelho" COMPRA2))
              (make-carta "vermelho" 3))
(check-expect (define-jogada J3 (make-carta "amarelo" INVERTE))
              (make-carta "amarelo" 5))
(check-expect (define-jogada J3 (make-carta "amarelo" 5))
              (make-carta "amarelo" 5))
(check-expect (define-jogada J4 (make-carta "amarelo" 5))
              (make-carta "amarelo" 4))
(check-expect (define-jogada J4 (make-carta "vermelho" 7))
              "Impossível jogar carta")

;; ========================================================================
;;                                 QUESTÃO 6 (Desafio)
;; =========================================================================

;; desenha-cartas-possíveis : ListaDeCartas Carta -> Imagem
;; Objetivo: desenha lado a lado as cartas de uma lista de cartas, com
;;           uma indicação sobre a possibilidade jogar cada uma sobre a carta
;;           da mesa.
;; Exemplos:
;;          (desenha-cartas-possíveis
;;           L1
;;           (make-carta "verde" 7)) = (beside
;;                                      (above
;;                                       (desenha-carta (make-carta "preto" CURINGA))
;;                                       (triangle 25 "solid" "red"))
;;                                      (above
;;                                       (desenha-carta (make-carta "preto" CURINGA_COMPRA4))
;;                                       (triangle 25 "solid" "red"))
;;                                      (above
;;                                       (desenha-carta (make-carta "amarelo" 4))
;;                                       (triangle 25 "solid" "transparent"))
;;                                      (above
;;                                       (desenha-carta (make-carta "vermelho" 5))
;;                                       (triangle 25 "solid" "transparent")))

(define (desenha-cartas-possíveis L C)
  ;; Dada uma lista de cartas L
  (cond
       ;; Se L é a lista vazia então devolve uma imagem vazia
       [(empty? L) empty-image]
       ;; Se L não for vazia
       ;;    então agrupa as  imagens lado a lado
       [else (beside
              
              ;; Agrupa uma em cima da outra
              (above
               ;; Desenha a primeira carta da lista
               (desenha-carta (first L))
              
               (cond
                 ;; Caso seja possível jogar a primeira carta da lista na mesa,
                 ;; desenha um triângulo vermelho para indicar isso
                 [(jogada-válida? (first L) C) (triangle 25 "solid" "red")]

                 ;; Caso contrário, desenha um triângulo transparente para
                 ;; questões de alinhamento de imagem
                 [else (triangle 25 "solid" "transparent")]))
               
              ;; Desenha a imagem das cartas do resto da lista L
              (desenha-cartas-possíveis (rest L) C))]))

;; Nesta questão você não deve incluir testes com check-expect, deixe apenas
;; chamadas da sua função.

;; mostra-jogadas-possíveis : Carta Jogador -> Imagem
;; Objetivo: dada a carta da mesa e um jogador, devolve uma imagem 
;;           mostrando o nome do jogador, a lista de cartas do jogador
;;           (com uma indicação sobre a possibilidade jogar cada uma),
;;           a carta da mesa e o número de opções de jogada.

(define (mostra-jogadas-possíveis C J)
  (above/align "left"
   ;; Nome do jogador
   (text (jogador-nome J) 20 "black")

   ;; Espaço
   (circle 4 "solid" "transparent")

   ;; Cartas do jogador
   (beside
    (text "Mão: " 20 "black")
    (desenha-cartas-possíveis (jogador-mão J) C)
    )

   ;; Espaço
   (circle 7 "solid" "transparent")
   
   ;; Carta da mesa
   (beside
    (text "Mesa: " 20 "black")
    (overlay
     (desenha-carta C)
     (circle 110 "solid" "brown"))
   )

   ;; Espaço
   (circle 4 "solid" "transparent")
   

   ;; Número de opções de jogada
   (cond
     ;; Se for possível fazer alguma jogada, informa quantas
     [(>= (número-opções (jogador-mão J) C) 1)
      (text
       (string-append
        "Número de opções: "
        (number->string (número-opções (jogador-mão J) C)))
       20 "black")]

     ;; Caso contrário
     [else (text "Impossível jogar carta" 20 "black")])

   ;; Espaço
   (circle 10 "solid" "transparent")
   
   )
  )

;; Chamadas da função:
(mostra-jogadas-possíveis  (make-carta "preto" CURINGA) J1)
(mostra-jogadas-possíveis (make-carta "vermelho" 7) J4)
(mostra-jogadas-possíveis (make-carta "verde" 8) J5)

