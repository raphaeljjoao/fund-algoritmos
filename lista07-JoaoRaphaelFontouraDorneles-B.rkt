;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista7-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 7

;; ========================================================================
;;                        DEFINIÇÕES DE DADOS
;; ========================================================================  

;; CONSTANTES:
(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA -5)
(define CURINGA_COMPRA4 -4)

;; -----------------
;; TIPO CARTA:
;; -----------------
(define-struct carta (cor valor))
;; Um elemento do conjunto Carta é
;;   (make-carta c v)     onde
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo", "vermelho" ou "preto" ou "livre"
;;   v : Número, é o valor da carta, que pode ser qualquer inteiro entre 0 e 9,
;;               ou um número negativo -1 (PulaVez), -2 (Compra2), -3 (Inverte),-4 (Compra4) ou -5 (Curinga)

;; --------------------
;; TIPO LISTA DE CARTAS:
;; --------------------
;; Uma ListaDeCartas é
;; 1. vazia (empty), ou
;; 2. (cons c lc), onde 
;;        c: Carta;
;;       lc: ListaDeCartas

;; Exemplos de elementos do conjunto ListaDeCartas (usados nas entradas e
;; saídas dos exemplos e testes de cada função):
(define LC1 (cons (make-carta "amarelo" 5)
                  (cons (make-carta "amarelo" 7)
                        (cons (make-carta "amarelo" 8)
                              (cons (make-carta "amarelo" 9) empty)))))

(define LC2 (cons (make-carta "verde" INVERTE)
                  (cons (make-carta "verde" PULA_VEZ)
                        (cons (make-carta "verde" 5)
                              (cons (make-carta "verde" 0) empty)))))

(define LC3 (cons (make-carta "vermelho" COMPRA2)
                  (cons (make-carta "vermelho" PULA_VEZ)
                        (cons (make-carta "vermelho" 1) empty))))

(define LC4 (cons (make-carta "azul" 5)
                  (cons (make-carta "azul" 7) empty)))

(define LC5 (cons (make-carta "vermelho" 7)
                  (cons (make-carta "amarelo" 5)
                        (cons (make-carta "verde" 5)
                              (cons (make-carta "verde" 7) empty)))))

(define LC6 (cons (make-carta "verde" 5)
                  (cons (make-carta "verde" 7) empty)))

(define LC7 (cons (make-carta "amarelo" 7)
                  (cons (make-carta "amarelo" 5)
                        (cons (make-carta "verde" 5) empty))))

(define LC8 (cons (make-carta "amarelo" 7)
                  (cons (make-carta "amarelo" 5) empty)))

(define LC9 (cons (make-carta "amarelo" 7)
                   (cons (make-carta "amarelo" 5)
                         (cons (make-carta "verde" 5) empty))))

(define LC10 (cons (make-carta "verde" 5) empty))

(define LC11 (cons (make-carta "vermelho" 5)
                   (cons (make-carta "vermelho" 7)
                         (cons (make-carta "vermelho" 8)
                               (cons (make-carta "vermelho" 9) empty)))))

(define LC12 (cons (make-carta "azul" INVERTE)
                   (cons (make-carta "azul" PULA_VEZ)
                         (cons (make-carta "azul" 5)
                               (cons (make-carta "azul" 0) empty)))))

(define LC13 (cons (make-carta "vermelho" 7)
                   (cons (make-carta "amarelo" 5)
                         (cons (make-carta "verde" 5)
                               (cons (make-carta "verde" 8) empty)))))

(define LC14 (cons (make-carta "vermelho" 7)
                   (cons (make-carta "amarelo" 5)
                         (cons (make-carta "verde" 5)
                               (cons (make-carta "vermelho" 7) empty)))))

(define LC15 (cons (make-carta "amarelo" 5)
                   (cons (make-carta "verde" 5)
                         (cons (make-carta "vermelho" 7) empty))))

(define LC16 (cons (make-carta "vermelho" 7)
                   (cons (make-carta "verde" 5)
                         (cons (make-carta "vermelho" 7) empty))))

(define LC17 (cons (make-carta "verde" 5)
                   (cons (make-carta "vermelho" 7) empty)))

(define LC18 (cons (make-carta "verde" 5)
                   (cons (make-carta "vermelho" 7)
                         (cons (make-carta "vermelho" 7)
                               (cons (make-carta "verde" 5) empty)))))

(define LC19 (cons (make-carta "vermelho" 7)
                   (cons (make-carta "verde" 5) empty)))

(define LC20 (cons (make-carta "vermelho" 0)
                   (cons (make-carta "amarelo" 0)
                         (cons (make-carta "verde" 9)
                               (cons (make-carta "amarelo" PULA_VEZ)
                                     (cons (make-carta "preto" CURINGA)
                                           (cons (make-carta "azul" 5)
                                                 (cons (make-carta "vermelho" 8)
                                                       (cons (make-carta "verde" 7)
                                                             (cons (make-carta "preto" CURINGA)
                                                                   (cons (make-carta "azul" INVERTE) empty)))))))))))

(define LC21 (cons (make-carta "preto" CURINGA)
                   (cons (make-carta "amarelo" 5)
                         (cons (make-carta "amarelo" 7)
                               (cons (make-carta "amarelo" 8)
                                     (cons (make-carta "amarelo" 9) empty))))))

(define LC22 (cons (make-carta "verde" INVERTE)
                  (cons (make-carta "verde" PULA_VEZ)
                        (cons (make-carta "verde" 5)
                              (cons (make-carta "vermelho" INVERTE)
                                    (cons (make-carta "verde" 0) empty))))))

(define LC23 (cons (make-carta "vermelho" COMPRA2)
                   (cons (make-carta "azul" COMPRA2)
                         (cons (make-carta "vermelho" PULA_VEZ)
                               (cons (make-carta "vermelho" 1) empty)))))

(define LC24 (cons (make-carta "preto" CURINGA_COMPRA4)
                   (cons (make-carta "amarelo" 5)
                         (cons (make-carta "amarelo" 7)
                               (cons (make-carta "amarelo" 8)
                                     (cons (make-carta "amarelo" 9) empty))))))

(define LC25 (cons (make-carta "amarelo" 5)
                  (cons (make-carta "azul" 0)
                        (cons (make-carta "amarelo" 7)
                              (cons (make-carta "amarelo" 8)
                                    (cons (make-carta "amarelo" 9) empty))))))

(define LC26 (cons (make-carta "amarelo" 5)
                  (cons (make-carta "amarelo" 7)
                        (cons (make-carta "vermelho" INVERTE)
                              (cons (make-carta "amarelo" 8)
                                    (cons (make-carta "amarelo" 9) empty))))))

(define LC27 (cons (make-carta "amarelo" 5)
                  (cons (make-carta "amarelo" 7)
                        (cons (make-carta "amarelo" 8)
                              (cons (make-carta "azul" COMPRA2)
                                    (cons (make-carta "amarelo" 9) empty))))))

(define LC28 (cons (make-carta "amarelo" 5)
                  (cons (make-carta "amarelo" 7)
                        (cons (make-carta "amarelo" 8)
                              (cons (make-carta "amarelo" 9)
                                    (cons (make-carta "verde" INVERTE) empty))))))

(define LC29 (cons (make-carta "azul" 7)
                   (cons (make-carta "azul" 5) empty)))

(define LC30 (cons (make-carta "amarelo" 9)
                  (cons (make-carta "amarelo" 8)
                        (cons (make-carta "amarelo" 5)
                              (cons (make-carta "amarelo" 7) empty)))))

(define LC31 (cons (make-carta "amarelo" 8)
                  (cons (make-carta "amarelo" 7)
                        (cons (make-carta "amarelo" 9)
                              (cons (make-carta "amarelo" 5) empty)))))

(define LC32 (cons (make-carta "amarelo" 7)
                  (cons (make-carta "amarelo" 8)
                        (cons (make-carta "amarelo" 9)
                              (cons (make-carta "amarelo" 5) empty)))))

;; =========================================================================
;;                                 QUESTÃO 1
;; =========================================================================

;; --------------------
;; TIPO LISTA DE NÚMEROS:
;; --------------------
;; Uma ListaDeNúmeros é
;; 1. vazia (empty), ou
;; 2. (cons n ln), onde:
;;        n : Número e
;;       ln : ListaDeNúmeros

;; Exemplos de elementos do conjunto ListaDeNúmeros:
(define LN1 (cons 5 (cons 7 (cons 8 (cons 9 empty)))))
(define LN2 (cons INVERTE (cons PULA_VEZ (cons 5 (cons 0 empty)))))
(define LN3 (cons COMPRA2 (cons PULA_VEZ (cons 1 empty))))
(define LN4 (cons 5 (cons 7 empty)))

;; cria-cartas : String ListaDeNúmeros -> ListaDeCartas
;; Objetivo: dada uma cor e uma lista de números, retorna uma lista de
;;           cartas de UNO.
;; Exemplos:
;;          (cria-cartas "vermelho" LN1) = LC11
;;          (cria-cartas "azul" LN2) = LC12

(define (cria-cartas C L)
  ;; Dada uma lista de números L
  (cond
    ;; Se a lista estiver vazia, retorna uma lista vazia
    [(empty? L) empty]

    ;; Se houver elementos na lista, cria uma lista de cartas com
    [else
     (cons
      ;; Uma carta com a cor C e o valor do primeiro elemento de L
      (make-carta C (first L))

      ;; Uma lista de cartas com a cor C e o resto de L
      (cria-cartas C (rest L))
      )]
    )
  )

;; Testes:
(check-expect (cria-cartas "amarelo" LN1) LC1)
(check-expect (cria-cartas "verde" LN2) LC2)
(check-expect (cria-cartas "vermelho" LN3) LC3)
(check-expect (cria-cartas "azul" LN4) LC4)

;; =========================================================================
;;                                 QUESTÃO 2
;; =========================================================================

;; seleciona-cartas : ListaDeCartas String -> ListaDeCartas
;; Objetivo: dada uma lista de cartas e uma cor, retorna uma nova lista de cartas
;;           somente com as cartas que tem a cor especificada.
;; Exemplos:
;;          (seleciona-cartas LC5 "verde") = LC6
;;          (seleciona-cartas LC7 "amarelo") = LC8

(define (seleciona-cartas L C)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se L estiver vazia, retorna uma lista vazia
    [(empty? L) empty]

    ;; Se o primeiro elemento de L for uma carta com a cor C, cria uma lista
    ;; de cartas com esse elemento e o resto das cartas de L que sejam da cor C
    [(string=? (carta-cor (first L)) C)
     (cons (first L) (seleciona-cartas (rest L) C))]

    ;; Caso contrário, seleciona as cartas do resto de L que sejam da cor C
    [else (seleciona-cartas (rest L) C)]
    )
  )

;; Testes:
(check-expect (seleciona-cartas LC5 "azul") empty)
(check-expect (seleciona-cartas LC5 "verde") LC6)
(check-expect (seleciona-cartas LC7 "amarelo") LC8)
(check-expect (seleciona-cartas LC9 "verde") LC10)
;; =========================================================================
;;                                 QUESTÃO 3
;; =========================================================================

;; carta=? : Carta Carta -> Boolean
;; Objetivo: compara se duas cartas são iguais.
;; Exemplos:
;;          (carta=? (make-carta "vermelho" 5)
;;                   (make-carta "amarelo" 0)) = false
;;
;;          (carta=? (make-carta "azul" 9)
;;                   (make-carta "azul" 9)) = true

(define (carta=? C1 C2)
  (cond
    ;; Se as cores e o valor de C1 e C2 forem iguais, retorna true
    [(and
      (string=? (carta-cor C1) (carta-cor C2))
      (= (carta-valor C1) (carta-valor C2))) true]

    ;; Caso contrário, retorna false
    [else false]
    )
  )

;; Testes:
(check-expect (carta=? (make-carta "vermelho" 5) (make-carta "amarelo" 0)) false)
(check-expect (carta=? (make-carta "azul" 6) (make-carta "amarelo" 1)) false)
(check-expect (carta=? (make-carta "verde" 0) (make-carta "amarelo" 0)) false)
(check-expect (carta=? (make-carta "azul" 9) (make-carta "azul" 9)) true)
(check-expect (carta=? (make-carta "verde" 8) (make-carta "verde" 8)) true)

;; carta-existe? : ListaDeCartas Carta -> Boolean
;; Objetivo: retorna se a carta informada está presente em uma lista de cartas.
;; Exemplos:
;;          (carta-existe? LC13 (make-carta "verde" 7)) = false
;;          (carta-existe? LC7 (make-carta "amarelo" 5)) = true

(define (carta-existe? L C)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se L estiver vazia, retorna false
    [(empty? L) false]

    ;; Se a primeira carta de L for igual a C, retorna true
    [(carta=? (first L) C) true]
    
    ;; Caso contrário, verifica se C existe no resto de L
    [else (carta-existe? (rest L) C)]
    )
  )

;; Testes:
(check-expect (carta-existe? LC1 (make-carta "vermelho" 7)) false)
(check-expect (carta-existe? LC2 (make-carta "azul" 5)) false)
(check-expect (carta-existe? LC13 (make-carta "verde" 7)) false)
(check-expect (carta-existe? LC7 (make-carta "amarelo" 5)) true)
(check-expect (carta-existe? LC8 (make-carta "amarelo" 7)) true)

;; remove-repetidas : ListaDeCartas -> ListaDeCartas
;; Objetivo: dada uma lista de cartas, retorna uma nova lista sem as cartas
;;           repetidas.
;; Exemplos:
;;          (remove-repetidas LC13) = LC13
;;          (remove-repetidas LC14) = LC15

(define (remove-repetidas L)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se L estiver vazia, retorna uma lista vazia
    [(empty? L) empty]

    ;; Se o primeiro elemento de L não existir no resto da lista, cria uma lista
    ;; com o primeiro elemento de L e os elementos do resto de L que não se repetem
    [(not (carta-existe? (rest L) (first L)))
     (cons
      (first L)
      (remove-repetidas (rest L)))]

    ;; Caso contrário, se a lista tiver elementos e o primeiro for repetido,
    ;; continua removendo as cartas repetidas do resto de L
    [else (remove-repetidas (rest L))]
    )
  )

;; Testes:
(check-expect (remove-repetidas LC13) LC13)
(check-expect (remove-repetidas LC14) LC15)
(check-expect (remove-repetidas LC16) LC17)
(check-expect (remove-repetidas LC18) LC19)

;; =========================================================================
;;                                 QUESTÃO 4
;; =========================================================================

;; Documentação da função random, que já existe no Racket, que deve ser usada
;; na resolução do exercício:

;; random: Número -> Número
;; obj: Dado um número inteiro k, retorna um número inteiro aleatório, 
;; no intervalo [0, k-1]
;; ex:
;;   (random 1) -> 0
;;   (random 2) -> 0
;;   (random 2) -> 1
;;   (random 3) -> 2
;;   (random 1000) -> 42

;; quantas-cartas : ListaDeCartas -> Número
;; Objetivo: dada uma lista de cartas, retorna quantas cartas existem
;;           nessa lista.
;; Exemplos:
;;          (quantas-cartas empty) = 0
;;          (quantas-cartas LC5) = 4
;;          (quantas-cartas LC7) = 3
;;          (quantas-cartas LC13) = 4
;;          (quantas-cartas LC19) = 2

(define (quantas-cartas L)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se L estiver vazia, retorna 0
    [(empty? L) 0]

    ;; Caso contrário, se houver elementos em L, soma 1 com o número de
    ;; cartas do resto de L
    [else (+ 1 (quantas-cartas (rest L)))]))

;; Testes:
(check-expect (quantas-cartas empty) 0)
(check-expect (quantas-cartas LC1) 4)
(check-expect (quantas-cartas LC3) 3)
(check-expect (quantas-cartas LC4) 2)
(check-expect (quantas-cartas LC10) 1)
(check-expect (quantas-cartas LC20) 10)

;; insere-carta : ListaDeCartas Carta Número -> ListaDeCartas
;; Objetivo: dada uma lista de cartas, uma carta e uma posição, insere a carta
;;           na posição indicada da lista (a posição começa a contar em zero).
;;           Se a posição informada não existir na lista de saída, a carta será 
;;           inserida no começo dela, caso o número da posição seja menor que 0, 
;;           ou no final dela, caso o número da posição seja maior que a quantidade 
;;           de posições da lista de entrada + 1.
;; Exemplos:
;;          (insere-carta LC1 (make-carta "preto" CURINGA) 0) = LC21
;;          (insere-carta LC2 (make-carta "vermelho" INVERTE) 3) = LC22
;;          (insere-carta LC3 (make-carta "azul" COMPRA2) 1) = LC23

(define (insere-carta L C P)
  ;; Dada uma lista de cartas L
  (cond

    ;; Considerando Q como o número de posições da nova lista
    ;; Q = 1 + (quantas-cartas L) - 1 = (quantas-cartas L)

    ;; Se P for menor que 0, insere a carta C na primeira posição de L
    [(< P 0) (insere-carta L C 0)]

    ;; Se P for maior que Q, insere a carta C na última posição de L
    [(> P (quantas-cartas L))
     (insere-carta L C (quantas-cartas L))]
    
    ;; Se P for igual a 0, cria uma lista com a C e a lista L
    [(= P 0) (cons C L)]

    ;; Caso contrário, cria uma lista com a primeira carta de L e insere
    ;; a carta C na posição P-1 do resto de L
    [else
     (cons (first L)
           (insere-carta (rest L) C (- P 1)))]
    )
  )

;; Testes:
(check-expect (insere-carta LC1 (make-carta "preto" CURINGA) 0) LC21)
(check-expect (insere-carta LC2 (make-carta "vermelho" INVERTE) 3) LC22)
(check-expect (insere-carta LC3 (make-carta "azul" COMPRA2) 1) LC23)
(check-expect (insere-carta LC1 (make-carta "preto" CURINGA_COMPRA4) -2) LC24)
(check-expect (insere-carta LC1 (make-carta "preto" CURINGA_COMPRA4) -1) LC24)
(check-expect (insere-carta LC1 (make-carta "preto" CURINGA_COMPRA4) 0) LC24)
(check-expect (insere-carta LC1 (make-carta "azul" 0) 1) LC25)
(check-expect (insere-carta LC1 (make-carta "vermelho" INVERTE) 2) LC26)
(check-expect (insere-carta LC1 (make-carta "azul" COMPRA2) 3) LC27)
(check-expect (insere-carta LC1 (make-carta "verde" INVERTE) 4) LC28)
(check-expect (insere-carta LC1 (make-carta "verde" INVERTE) 5) LC28)
(check-expect (insere-carta LC1 (make-carta "verde" INVERTE) 6) LC28)

;; embaralha : ListaDeCartas -> ListaDeCartas
;; Objetivo: dada uma lista de cartas, retorna uma lista com as mesmas cartas,
;;           mas com suas posições embaralhadas.
;; Exemplos:
;;          (embaralha LC1) = LC30
;;          (embaralha LC1) = LC31
;;          (embaralha LC1) = LC32
;;          (embaralha LC4) = LC4
;;          (embaralha LC4) = L29

(define (embaralha L)
  ;; Dada uma lista de cartas L
  (cond
    ;; Se o resto de L estiver vazio, retorna uma lista vazia
    [(empty? L) empty]

    ;; ...
    [else
     (insere-carta
      (embaralha (rest L))
      (first L)
      (random (quantas-cartas L))
      )
      ]
    )
  )