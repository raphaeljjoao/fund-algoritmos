;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista11-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 11

;; Funções pré-definidas:

;; flip-horizontal : Imagem -> Imagem
;; Dada uma imagem, gera uma nova imagem espelhando a imagem original horizontalmente.

;; line: Número Número String -> Imagem
;; Dados as as coordenadas x e y de um ponto e uma cor, desenha uma reta desta cor
;; ligando este ponto ao ponto (0,0).

;; ========================================================================== 
;; TIPO FIGURA:
;; ===========
(define-struct figura (x y altura cor))
;; Um elemento do conjunto Figura tem o formato
;; (make-figura x y a c), onde:
;;   x: Número, é a coordenada x do centro da figura
;;   y: Número, é a coordenada y do centro da figura
;;   a : Número, é a altura da figura
;;   c : Número, número que representa a cor da figura, de acordo com a função gera-cor 

;; Exemplos de elementos do conjunto Figura:
(define F1 (make-figura 1 2 50 45))
(define F2 (make-figura 30 0 10 0))
(define F3 (make-figura 34 75 50 56))
(define F4 (make-figura 220 100 200 3))

;; ========================
;; FUNÇÃO GERA-COR:
;; ========================
;; gera-cor : Número -> String
;; Objetivo: Dado um número positivo, devolve uma de 5 cores: "red", "blue", "green", "yellow"
;;           ou "cyan".
;; Exemplos:
;;      (gera-cor 3) = "yellow"
;;      (gera-cor 55) = "red"
(define (gera-cor n)
  (cond
    [(= (remainder n 5) 0) "red"]
    [(= (remainder n 5) 1) "blue"]  
    [(= (remainder n 5) 2) "green"]
    [(= (remainder n 5) 3) "yellow"]
    [(= (remainder n 5) 4) "cyan"]))

;; ========================
;; FUNÇÃO DESENHA-TRIANGULO:
;; ========================
;; desenha-triangulo : Número String ->  Imagem
;; Objetivo: Dados um tamanho de lado e uma cor, desenha um triângulo.

(define (desenha-triangulo lado cor)
  (triangle lado "outline" cor))


;; ==========================================================================
;; Função sierpinski
;; ==========================================================================

;; sierpinski : Número String -> Imagem
;; Objetivo: Dados o tamanho do lado e uma cor, desenha um triângulo de Sierpinski
;; desta cor cujo lado do triângulo externo é o lado passado como argumento. 
;; Exemplos:
;;    (sierpinksi 40 "red") => desenha um triângulo de Sierpinski de lado externo 40 na cor vermelha
;;    (sierpinksi 100 "blue") => desenha um triângulo de Sierpinski de lado externo 100 na cor azul

(define (sierpinski lado cor)
  (cond
       ;; caso trivial: se o lado for muito pequeno, desenhar um triângulo com o lado dado
       [(<= lado 5)  (desenha-triangulo lado cor)]
       ;; senão 
       [else (local
               ( ;; desenha um triângulo de sierpinksi com a metade do tamanho do lado
                 ;; e dá o nome de TRIANGULO para este desenho:
                (define TRIANGULO (sierpinski (/ lado 2) cor))
               )
                ;; e monta a imagem do triângulo de sierpinski colocando um TRIANGULO
                ;; acima de dois outros TRIANGULOs:
               (above TRIANGULO
                      (beside TRIANGULO TRIANGULO)))]))
;; Argumentação de terminação:
;; Este programa sempre termina porque:
;; (a) Existe um caso base (sem recursão) que é quando o tamanho do lado é menor ou igual a 5.
;;     Neste caso, o programa simplesmente desenha um triângulo com este lado, na cor selecionada.
;; (b) Cada chamada recursiva é realizada tendo como argumento a metade do lado,
;;     que é um número estritamente menor que o lado, e portanto mais próximo
;;     de se tornar menor que 5 (lembre que a chamada recursiva só ocorre se o lado for >=5).
;; (c) As funções <=, above, beside e cond terminam, pois são pré-definidas da linguagem.
;;     A função desenha-triangulo termina, pois somente usa funções pré-definidas e não tem laços.

;; ==========================================================================
;;                                 QUESTÕES
;; ==========================================================================

;; ==========================================================================
;; 1: Função sierpinski-carpet
;; ==========================================================================
;; sierpinski-carpet : Número String -> Imagem
;; Objetivo: Dados o tamanho do lado e uma cor, desenha um carpete de Sierpinski desta cor
;; cujo lado do carpete/quadrado mais externo é o lado passado como argumento.
;; Exemplos:
;;          (sierpinski-carpet 40 "red") desenha um carpete de Sierpinski de lado externo
;;                                       40 na cor vermelha
;;          (sierpinski-carpet 100 "purple") desenha um carpete de Sierpinski de lado externo
;;                                           100 na cor roxa
;;          (sierpinski-carpet 250 "orange") desenha um carpete de Sierpinski de lado externo
;;                                           250 na cor laranja

(define (sierpinski-carpet lado cor)
  (cond
    ;; Se o lado for muito pequeno, desenha somente um quadrado com o lado dado
    [(<= lado 5) (square lado "solid" cor)]
    ;; Caso contrário
    [else
     (local
       (
        ;; Desenha um carpete de Sierpinksi chamado QUAD com 1/3 do tamanho do lado
        ;; na cor dada
        (define QUAD (sierpinski-carpet (/ lado 3) cor))
        ;; Desenha um quadrado chamado VAZIO com 1/3 do tamanho do lado na cor branca
        (define VAZIO (square (/ lado 3) "solid" "white"))
        )
       ;; Empilhar em uma única imagem
       (above
        ;; Três carpetes QUAD, um ao lado do outro
        (beside QUAD QUAD QUAD)
        ;; Dois carpetes QUAD, um ao lado do outro, com um quadrado VAZIO entre eles
        (beside QUAD VAZIO QUAD)
        ;; Três carpetes QUAD, um ao lado do outro
        (beside QUAD QUAD QUAD)))]))

;; Argumentação de terminação:
;; Este programa sempre termina porque:
;; (a) Existe um caso base que é quando o tamanho do lado é menor ou igual a 5. Neste caso,
;;     o programa simplesmente desenha um quadrado com este lado, na cor selecionada.
;; (b) Cada chamada recursiva é realizada tendo como argumento 1/3 do lado, que é um número
;;     estritamente menor que o lado, e portanto mais próximo de se tornar menor que 5.
;; (c) As funções <=, above, beside e cond terminam, pois são pré-definidas da linguagem.

;; Exemplos de chamadas da função sierpinski-carpet:
"sierpinski-carpet:"
(sierpinski-carpet 5 "red")
(sierpinski-carpet 15 "gold")
(sierpinski-carpet 30 "yellow")
(sierpinski-carpet 60 "forestgreen")
(sierpinski-carpet 120 "blue")
(sierpinski-carpet 240 "purple")

;; ==========================================================================
;; 2: Função desenha-sierpinski-carpet
;; ==========================================================================

;; desenha-sierpinski-carpet : Figura -> Imagem
;; Objetivo: Dada uma figura, desenha um carpete de Sierpinski com a cor da figura traduzida pela
;; função gera-cor e cujo lado do carpete/quadrado mais externo é a altura da figura passada como
;; argumento.
;; Exemplos:
;;          (desenha-sierpinski-carpet F1) desenha um carpete de Sierpinski de lado externo
;;                                         50 na cor vermelha
;;          (desenha-sierpinski-carpet F2) desenha um carpete de Sierpinski de lado 10
;;                                         100 na cor vermelha
;;          (desenha-sierpinski-carpet F3) desenha um carpete de Sierpinski de lado externo
;;                                         50 na cor azul

(define (desenha-sierpinski-carpet fig)
  ;; Desenha um carpete de Dierpinski com a altura e cor de fig
  (sierpinski-carpet (figura-altura fig) (gera-cor (figura-cor fig)))
  )

;; Exemplos de chamadas da função desenha-sierpinski-carpet:
"desenha-sierpinski-carpet:"
(desenha-sierpinski-carpet F1)
(desenha-sierpinski-carpet F2)
(desenha-sierpinski-carpet F3)
(desenha-sierpinski-carpet (make-figura 0 0 150 533))
(desenha-sierpinski-carpet (make-figura 0 0 175 0))
(desenha-sierpinski-carpet (make-figura 0 0 200 4))

;; ==========================================================================
;; 3: Função desenha-boneco
;; ==========================================================================

;; desenha-boneco : Figura -> Imagem
;; Objetivo: Dada uma figura, desenha um boneco de palito com a cor (traduzida pela função gera-cor)
;; e altura da figura.
;; Exemplos:
;;          (desenha-boneco F1) desenha um boneco de palito com altura 50 na cor vermelha
;;          (desenha-boneco F2) desenha um boneco de palito com altura 10 na cor vermelha
;;          (desenha-boneco F3) desenha um boneco de palito com altura 50 na cor azul
;;          (desenha-boneco F4) desenha um boneco de palito com altura 200 na cor amarelo

(define (desenha-boneco fig)
  (local
    (
     (define tam (figura-altura fig)) ;; Tamanho de fig
     (define tamParte (/ tam 4)) ;; 1/4 do tamanho
     (define cor (gera-cor (figura-cor fig))) ;; Cor de fig traduzida pela função gera-cor
     
     (define cabeca (circle (/ tamParte 2) "solid" cor)) ;; Cabeça do boneco
     (define corpo (line 0 (* 2 tamParte) cor)) ;; Corpo do boneco

     ;; Braços do boneco
     (define bracoDireito (line tamParte (* 0.75 tamParte) cor))
     (define bracoEsquerdo (rotate 76 bracoDireito))

     ;; Pernas do boneco
     (define pernaDireita (line tamParte (* 1 tamParte) cor))
     (define pernaEsquerda (rotate 90 pernaDireita))
     )
    ;; Empilha em uma única imagem a cabeça
    (above cabeca
           ;; Desenha da esquerda para a direita (lado a lado)
           (beside bracoEsquerdo corpo bracoDireito)
           ;; Desenha lado a lado
           (beside pernaEsquerda pernaDireita))))

;; Exemplos de chamadas da função desenha-boneco:
"desenha-boneco:"
(desenha-boneco F1)
(desenha-boneco F2)
(desenha-boneco F3)
(desenha-boneco F4)

;; ==========================================================================
;; 4: Função desenha-figuras
;; ==========================================================================

;; Dimensões de uma cena padrão:
(define LARG 400) ;; Largura
(define ALT 400) ;; Altura

;; desenha-figuras : (Figura -> Imagem) Figura -> Cena
;; Objetivo: Dadas uma função e uma figura, desenha em uma cena várias dessas figuras utilizando 
;; a função dada, variando sua posição e cor até que sua posição horizontal ultrapasse a largura
;; da cena.
;; Exemplos:
;;          (desenha-figuras desenha-sierpinski-carpet F1) retorna uma cena com vários carpetes
;;          de Sierpinski com lado externo 50 e cor aleatória distribuídos diagonalmente partindo do
;;          canto superior esquerdo até atingir o canto inferior direito, a distância do centro de um
;;          carpete até outro é √1250
;;          (desenha-figuras desenha-boneco F2) retorna uma cena com vários bonecos de palito
;;          com altura 10 e cor aleatória distribuídos diagonalmente partindo do canto superior
;;          esquerdo até atingir o canto inferior direito, a distância do centro de um boneco até
;;          outro é √200

(define (desenha-figuras func fig)
  ;; Dadas uma função chamada func que recebe uma figura e desenha uma imagem e uma figura chamada fig
  (cond
    ;; Se a posição horizontal de fig ultrapassar a largura da cena, desenha fig em uma
    ;; cena vazia com largura LARG e altura ALT e a posiciona no local definido pelos seus atributos
    [(> (figura-x fig) LARG) (place-image (func fig)
                                          (figura-x fig)
                                          (figura-y fig)
                                          (empty-scene LARG ALT))]
    
    ;; Caso contrário
    [else (place-image
           ;; Desenha fig
           (func fig)
           ;; Posicionada no local definido
           (figura-x fig)
           (figura-y fig)
           ;; Em uma cena com várias figs
           (desenha-figuras func (make-figura
                                  ;; Variando sua posição horizontal para direita em metade
                                  ;; da altura de fig
                                  (+ (figura-x fig) (/ (figura-altura fig) 2))
                                  ;; Variando sua posição vertical para baixo em metade
                                  ;; da altura de fig
                                  (+ (figura-y fig) (/ (figura-altura fig) 2))
                                  ;; Mantendo sua altura original
                                  (figura-altura fig)
                                  ;; Aleatorizando sua cor
                                  (random 5))))]))

;; Exemplos de chamadas da função desenha-figuras:
"desenha-figuras:"
(desenha-figuras desenha-sierpinski-carpet F1)
(desenha-figuras desenha-boneco F1)
(desenha-figuras desenha-sierpinski-carpet F2)
(desenha-figuras desenha-boneco F2)
(desenha-figuras desenha-sierpinski-carpet F3)
(desenha-figuras desenha-boneco F3)
(desenha-figuras desenha-sierpinski-carpet F4)
(desenha-figuras desenha-boneco F4)

;; ==========================================================================
;; 5: Função desenha-figuras-gen
;; ==========================================================================

;; (a) Critérios de fim e funções de movimentação:

;; pequena? : Figura -> Booleano
;; Objetivo: Dada uma figura, retorna se sua altura é menor que 1.
;; Exemplos:
;;          (pequena? F1) retorna false
;;          (pequena? (make-figura 1 2 0.75 3)) retorna true

(define (pequena? fig)
  ;; Retorno: Altura de fig < 1
  (< (figura-altura fig) 1))

;; Testes:
(check-expect (pequena? F1) false)
(check-expect (pequena? F2) false)
(check-expect (pequena? F3) false)
(check-expect (pequena? F4) false)
(check-expect (pequena? (make-figura 0 0 (* (/ 1 3) 3) 4)) false)
(check-expect (pequena? (make-figura 0 0 0.5 4)) true)
(check-expect (pequena? (make-figura 0 0 0.25 4)) true)
(check-expect (pequena? (make-figura 0 0 0.00023 4)) true)
(check-expect (pequena? (make-figura 0 0 0.9999 4)) true)

;; foraLimites? : Figura -> Booleano
;; Objetivo: Dada uma figura, retorna se ela está fora dos limites de dimensões de uma cena padrão.
;; Exemplos:
;;          (foraLimites? F2) retorna false
;;          (foraLimites? (make-figura 401 401 1 0)) retorna true
;;          (foraLimites? (make-figura 401 0 1 0)) retorna true
;;          (foraLimites? (make-figura 0 401 1 0)) retorna true

(define (foraLimites? fig)
  (not ;; Contrário de
   (and ;; Se as duas condições a seguir forem satisfeitas
    ;; 0 <= Altura de fig <= Largura da cena
    (<= 0 (figura-x fig) LARG)
    ;; 0 <= Altura de fig <= Altura da cena
    (<= 0 (figura-y fig) ALT))))

;; Testes:
(check-expect (foraLimites? F1) false)
(check-expect (foraLimites? F2) false)
(check-expect (foraLimites? F3) false)
(check-expect (foraLimites? F4) false)
(check-expect (foraLimites? (make-figura 500 0 100 9)) true)
(check-expect (foraLimites? (make-figura 0 401 100 7)) true)
(check-expect (foraLimites? (make-figura 500 401 100 45)) true)

;; grande? : Figura -> Booleano
;; Objetivo: Dada uma figura, retorna se sua altura é maior que alguma das dimensões de
;; uma cena padrão.
;; Exemplos:
;;          (grande? F3) retorna false
;;          (grande? (grande? (make-figura 0 0 401 0))) retorna true

(define (grande? fig)
  (or ;; Se algum das condições a seguir forem satisfeitas
   ;; Altura de fig > Largura da cena
   (> (figura-altura fig) LARG)
   ;; Altura de fig > Altura da cena
   (> (figura-altura fig) ALT)))

;; Testes:
(check-expect (grande? F1) false)
(check-expect (grande? F2) false)
(check-expect (grande? F3) false)
(check-expect (grande? F4) false)
(check-expect (grande? (make-figura 0 0 401 0)) true)
(check-expect (grande? (make-figura 0 0 (expt 10 69) 0)) true)

;; figAltera1 : Figura -> Figura
;; Objetivo: Dada uma figura, retorna uma figura com os mesmos atributos, mas com sua posição
;; vertical e altura alteradas.
;; Exemplos:
;;          (figAltera1 F1) retorna (make-figura 1 10.33333333 45 45)
;;          (figAltera1 F2) retorna (make-figura 30 1.66666666 9 0)
;;          (figAltera1 F3) retorna (make-figura 34 83.33333333 45 56)
;;          (figAltera1 F4) retorna (make-figura 220 133.3333333333 180 3)


(define (figAltera1 fig)
  ;; Retorna uma figura
  (make-figura
   ;; Mantendo sua posição horizontal
   (figura-x fig)
   ;; Somando à sua posição vertical 1/6 da altura de fig
   (+ (figura-y fig) (/ (figura-altura fig) 6))
   ;; Diminuindo 1/10 da sua altura
   (* (figura-altura fig) 0.9)
   ;; Mantendo sua cor
   (figura-cor fig)))

;; Testes:
(check-expect (figAltera1 F1) (make-figura 1 10.33333333 45 45))
(check-expect (figAltera1 F2) (make-figura 30 1.66666666 9 0))
(check-expect (figAltera1 F3) (make-figura 34 83.33333333 45 56))
(check-expect (figAltera1 F4) (make-figura 220 133.3333333333 180 3))
(check-expect (figAltera1 (make-figura 0 0 60 5)) (make-figura 0 10 54 5))
(check-expect (figAltera1 (make-figura 0 0 216 256)) (make-figura 0 36 194.4 256))

;; figAltera2 : Figura -> Figura
;; Objetivo: Dada uma figura, retorna uma figura com posição diagonal, altura e cor alteradas.
;; Exemplos:
;;          (figAltera2 F1) retorna (make-figura 6 7 75 46)
;;          (figAltera2 F2) retorna (make-figura 35 5 15 1)
;;          (figAltera2 F3) retorna (make-figura 39 80 75 57)
;;          (figAltera2 F4) retorna (make-figura 225 105 300 4)

(define (figAltera2 fig)
  ;; Retorna uma figura
  (make-figura
   ;; Somando 5 à sua posição horizontal
   (+ (figura-x fig) 5)
   ;; Somando 5 à sua posição vertical
   (+ (figura-y fig) 5)
   ;; Aumentando sua altura 1.5 vezes
   (* (figura-altura fig) 1.5)
   ;; Somando 1 unidade ao número que representa sua cor
   (+ (figura-cor fig) 1)))

;; Testes:
(check-expect (figAltera2 F1) (make-figura 6 7 75 46))
(check-expect (figAltera2 F2) (make-figura 35 5 15 1))
(check-expect (figAltera2 F3) (make-figura 39 80 75 57))
(check-expect (figAltera2 F4) (make-figura 225 105 300 4))
(check-expect (figAltera2 (make-figura 1 2 3 4)) (make-figura 6 7 4.5 5))
(check-expect (figAltera2 (make-figura 0 0 25 2)) (make-figura 5 5 37.5 3))
(check-expect (figAltera2 (make-figura 100 100 500 99)) (make-figura 105 105 750 100))

;; figAltera3 : Figura -> Figura
;; Objetivo: Dada uma figura, retorna uma figura com todos os seus atributos alterados de forma
;; pseudoaleatória.
;; Exemplos:
;;          (figAltera3 F1) retorna (make-figura [56,76] [58,78] [5,150] [0,5])
;;          (figAltera3 F2) retorna (make-figura [41, 61] [1,21] [1,30] [0,5])
;;          (figAltera3 F3) retorna (make-figura [89,109] [130,150] [5,150] [0,5])
;;          (figAltera3 F3) retorna (make-figura [440,460] [320,340] [20,600] [0,5])

(define (figAltera3 fig)
  (local
    (
     (define x (figura-x fig)) ;; Posição horizontal de fig
     (define y (figura-y fig)) ;; Posição vertical de fig
     (define altura (figura-altura fig)) ;; Altura de fig
     (define cor (figura-cor fig)) ;; Número que representa a cor de fig
     (define pLarg (+ 1 (/ LARG 20))) ;; Parte da largura da cena (1/20) + 1
     (define pAlt (+ 1 (/ ALT 20))) ;; Parte da altura da cena (1/20) + 1
     )
    ;; Retorna uma figura
    (make-figura
     ;; Somando altura e um número aleatório no intervalo [0, pLarg] à sua posição horizontal
     (+ x altura (random pLarg))
     ;; Somando altura e um número aleatório no intervalo [0, pAlt] à sua posição vertical
     (+ y altura (random pAlt))
     ;; Aumentando sua altura n vezes, onde n é um número no intervalo [0.1,3]
     (* altura (/ (+ 1(random 30)) 10))
     ;; Definindo o número que representa sua cor como um número aleatório no intervalo [0,5]
     (random 6))))

;; Testes não foram incluídos porque essa função utiliza números pseudoaleatórios na definição
;; de todos os seus atributos. Os comentários do código indicam o intervalo de possibilidades
;; na geração dos números. Além disso, nos exemplos dessa função foram incluídos os conjuntos
;; numéricos que representam os números mínimos e máximos que podem ser gerados em cada caso.

;; (b) função desenha-figuras-gen:

;; desenha-figuras-gen : (Figura -> Imagem) Figura (Figura -> Booleano) (Figura -> Figura) -> Cena
;; Objetivo: Dadas uma função para desenhar figuras, uma figura, um critério de fim e uma função de
;; alteração de figuras, gera uma cena com várias figuras desenhadas pela primeira função, variando
;; seus atribudos através da terceira função até que o critério da segunda função seja satisfeito.
;; Exemplos:
;;          (desenha-figuras-gen desenha-sierpinski-carpet F4 pequena? figAltera1) retorna uma cena
;;          com vários carpetes de Sierpinski na cor amarela que perdem 1/10 do seu tamanho e se
;;          movimentam verticalmente 1/6 do seu tamanho a cada novo carpete, até que a altura de
;;          um carpete seja menor que 1
;;          (desenha-figuras-gen desenha-boneco F3 grande? figAltera2) retorna uma cena
;;          com vários bonecos de palito que se movimentam diagonalmente √50, aumentam
;;          1.5x de altura e incrementam 1 unidade no número que representa sua cor a cada
;;          novo boneco, até que a altura do boneco seja maior que alguma das dimensões da cena

(define (desenha-figuras-gen desenha fig criterio altera)
  (cond
    ;; Se criterio for verdadeiro, utiliza a função desenha para desenhar fig em uma cena vazia
    ;; com largura LARG e altura ALT e a posiciona no local definido pelos seus atributos
    [(criterio fig) (place-image (desenha fig)
                                 (figura-x fig)
                                 (figura-y fig)
                                 (empty-scene LARG ALT))]

    ;; Caso contrário
    [else (place-image
           ;; Utiliza a função desenha para desenhar fig
           (desenha fig)
           ;; No local definido pelos seus atributos
           (figura-x fig)
           (figura-y fig)
           ;; Em uma cena com várias figs
           (desenha-figuras-gen
            ;; Que são desenhadas pela função desenha
            desenha
            ;; A alteração de fig
            (altera fig)
            ;; Tendo como critério de fim a função critério
            criterio
            ;; Tendo como padrão de alteração a função altera
            altera))]))

;; Exemplos de chamadas da função desenha-figuras-gen:
"desenha-figuras-gen:"
;; Chamada 1 (C1):
(desenha-figuras-gen desenha-sierpinski-carpet F4 pequena? figAltera1)
;; Chamada 2 (C2):
(desenha-figuras-gen desenha-boneco F3 grande? figAltera2)
;; Chamada 3 (C3):
(desenha-figuras-gen desenha-sierpinski-carpet F1 pequena? figAltera3)
;; Chamada 4 (C4):
(desenha-figuras-gen desenha-boneco F2 foraLimites? figAltera3)

;; (c) Argumentação da terminação das chamadas de exemplo:

;; C1
;; Este programa sempre termina porque:
;; (a) Um caso base foi definido para ele, caso esse que é verificado utilizando função pequena?,
;;     que retorna se a altura da figura é menor que 1. Quando esse caso é atingido, o programa
;;     utiliza a função desenha-sierpinski-carpet para desenhar a figura na tela, em uma cena vazia.
;;     A função desenha-sierpinski-carpet sempre termina, pois ela usa a função sierpinski-carpet
;;     no seu funcionamento. A função sierpinski-carpet teve sua terminação argumentada.
;; (b) Cada chamada recursiva é realizada tendo como argumento 9/10 da altura original da figura,
;;     que é um número estritamente menor que a altura original e mais próximo de se tornar menor que
;;     1, atingindo o caso base.
;; (c) A função place-image termina, pois é pré-definida na linguagem.

;; C2
;; Este programa sempre termina porque:
;; (a) Um caso base foi definido para ele, caso esse que é verificado utilizando a função grande?, 
;;     que retorna se a altura da figura é maior que alguma das dimensões de uma cena padrão. 
;;     Quando esse caso é atingido, o programa utiliza a função desenha-boneco para desenhar a figura
;;     na tela, em uma cena vazia. A função desenha-boneco sempre termina, pois usa somentes funções
;;     pré-definidas dada linguagem (line, circle, above e beside, além da função remainder que
;;     é a base da função gera-cor) e não tem recursão em nenhum momento.
;; (b) Cada chamada recursiva é realizada tendo como argumento 1.5x da altura original da figura,
;;     que é um número estritamente maior que a altura original e mais próximo de ser tornar maior
;;     que alguma das dimensões de uma cena padrão, atingindo o caso base.
;; (c) A função place-image termina, pois é pré-definida na linguagem.

;; C3
;; Este programa nem sempre termina porque:
;; (a) Um caso base foi definido para ele, caso esse que é verificado utilizando função pequena?,
;;     que retorna se a altura da figura é menor que 1. Quando esse caso é atingido, o programa
;;     utiliza a função desenha-sierpinski-carpet para desenhar a figura na tela, em uma cena vazia.
;;     A função desenha-sierpinski-carpet sempre termina, pois ela usa a função sierpinski-carpet
;;     no seu funcionamento. A função sierpinski-carpet teve sua terminação argumentada.
;; (b) A função place-image termina, pois é pré-definida na linguagem.
;; (c) Apesar de um caso base ter sido definido, cada chamada recursiva é realizada tendo como
;;     argumento uma altura aleatória, que pode variar entre 0.1x e 3x sobre a altura original. Não
;;     há garantia que a multiplicação resultará em um altura menor que 1, pois a altura aumenta
;;     e diminui de forma aleatória.
;; (d) Logo, o programa somente acaba quando as sucessivas multiplicações sobre a altura resultam
;;     em uma altura menor que 1, o que não pode ser previsto, por se tratar de uma geração aleatória.

;; C4
;; Este programa sempre termina porque:
;; (a) Um caso base foi definido para ele, caso esse que é verificado utilizando foraLimites?, que
;;     retorna se ambas posições (horizontal e vertical) estão fora dos limites de uma cena padrão.
;;     Quando esse caso é atingido, o programa utiliza a função desenha-boneco para desenhar a figura
;;     na tela, em uma cena vazia. A função desenha-boneco sempre termina, pois usa somentes funções
;;     pré-definidas da da linguagem (line, circle, above e beside, além da função remainder que é a
;;     base da função gera-cor) e não tem recursão em nenhum momento.
;; (b) Cada chamada recursiva é realizada tendo como argumento duas posições aleatórias (horizontal e
;;     vertical), que são sempre estritamente maiores que as posições originais e mais próximas de
;;     se tornarem maiores que os limites de uma cena padrão, atingindo o caso base.
;; (c) Apesar da definição da posição depender da altura, que também é aleatória, a altura nunca será
;;     multiplicada por 0, o que garante que as posições nunca regredirão.
;; (d) A função place-image termina, pois é pré-definida na linguagem.


;; C3: pode ser que a multiplicação nunca resulte em um altura menor que 1

;; ==========================================================================
;; 6: Função sierpinski-carpet-color
;; ==========================================================================

;; sierpinski-carpet-color : Número Número -> Imagem
;; Objetivo: Dados o tamanho do lado e um número que representa uma cor, desenha um carpete de
;; Sierpinski colorido cujo lado do carpete/quadrado mais externo é o lado passado por argumentado.
;; A cor mais predominante do carpete sempre será o preto, e a segunda mais predominante será a gerada
;; pela função gera-cor através da cor dada.
;; Exemplos:
;;          (sierpinski-carpet-color 15 1) desenha um carpete de Sierpinski de lado externo 15,
;;          em 2 cores, sendo elas preto e azul, respectivamente em sua ordem de predominância
;;          (sierpinski-carpet-color 45 1) desenha um carpete de Sierpinski de lado externo 45,
;;          em 3 cores, sendo elas preto, azul e verde, respectivamente em sua ordem de predominância
;;          (sierpinski-carpet-color 135 1) desenha um carpete de Sierpinski de lado externo 135,
;;          em 4 cores, sendo elas preto, azul, verde e amarelo, respectivamente em sua ordem de
;;          predominância
;;          (sierpinski-carpet-color 405 1) desenha um carpete de Sierpinski de lado externo 405,
;;          em 5 cores, sendo elas preto, azul, verde, amarelo e ciano respectivamente em sua ordem
;;          de predominância
;;          (sierpinski-carpet-color 1215 1) desenha um carpete de Sierpinski de lado externo 1215,
;;          em 6 cores, sendo elas preto, azul, verde, amarelo, ciano e vermelho respectivamente em 
;;          sua ordem de predominância
;;          (sierpinski-carpet-color 3645 1) desenha um carpete de Sierpinski de lado externo 3645,
;;          em 7 cores, sendo elas preto, azul, verde, amarelo, ciano, vermelho e azul (2),   
;;          respectivamente em sua ordem de predominância

(define (sierpinski-carpet-color lado cor)
  (cond
    ;; Se o lado for muito pequeno, desenha um quadrado preto com lado lado
    [(<= lado 5) (square lado "solid" "black")]
    ;; Caso contrário
    [else
     (local
       (
        ;; Desenha um carpete de Sierpinski chamado QUAD com 1/3 do tamanho do lado
        ;; na cor seguinte a dada
        (define QUAD (sierpinski-carpet-color (/ lado 3) (+ cor 1)))
        ;; Desenha um quadrado chamado VAZIO com 1/3 do tamanho do lado na cor dada
        (define VAZIO (square (/ lado 3) "solid" (gera-cor cor)))
        )
       ;; Empilhar em uma única imagem
       (above
        ;; Três carpetes QUAD, um ao lado do outro
        (beside QUAD QUAD QUAD)
        ;; Dois carpetes QUAD, um ao lado do outro, com um quadrado VAZIO entre eles
        (beside QUAD VAZIO QUAD)
        ;; Três carpetes QUAD, um ao lado do outro
        (beside QUAD QUAD QUAD)))]
    )
  )

;; Exemplos de chamadas da função sierpinski-carpet-color:
"sierpinski-carpet-color:"
(sierpinski-carpet-color 5 0)
(sierpinski-carpet-color 15 1)
(sierpinski-carpet-color 45 2)
(sierpinski-carpet-color 135 3)
(sierpinski-carpet-color 405 4)
(sierpinski-carpet-color 1215 5)

(text "FIM" 50 "black")