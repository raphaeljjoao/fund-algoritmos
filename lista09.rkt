;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista9-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 9

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================

;; Um elemento do conjunto Data é um elemento do conjunto NumerosInteiros.

;; d1<=d2? : Data Data -> Booleano
;; Objetivo: dadas 2 datas (apenas o ano), verifica se a primeira é menor ou igual a segunda.
;; Exemplos:
;;          (d1<=d2? 2014 2013) = false
;:          (d1<=d2? 2014 2014) = true
;:          (d1<=d2? 2012 2016) = true

(define (d1<=d2? d1 d2) (<= d1 d2))

;; Testes:
(check-expect (d1<=d2? 2014 2013) false)
(check-expect (d1<=d2? 2023 2021) false)
(check-expect (d1<=d2? 2017 2016) false)
(check-expect (d1<=d2? 2014 2014) true)
(check-expect (d1<=d2? 2012 2016) true)
(check-expect (d1<=d2? 2006 2012) true)

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

(define-struct filho (pai mãe nome data olhos))
;; Um elemento nó de um conjunto Nó (de uma árvore genealógica) é:
;; 1. empty, representando a falta de informação, ou
;; 2. (make-filho p m n d o), onde:
;;   p : Nó, representa o pai do filho;
;;   m : Nó, representa a mãe do filho;
;;   n : String, representa o nome do filho;
;;   d : Data, representa o ano de nascimento do filho;
;;   o : String, representa a cor dos olhos do filho.

;; Exemplos de elementos do conjunto Filho:
(define Althea (make-filho empty empty "Althea" 1915 "brown"))
(define Jack (make-filho empty empty "Jack" 1948 "brown"))
(define Judy (make-filho empty Althea "Judy" 1945 "green"))
(define Monica (make-filho Jack Judy "Monica" 1968 "blue"))
(define Ross (make-filho Jack Judy "Ross" 1966 "brown"))
(define Sandra (make-filho empty empty "Sandra" 1947 "brown"))
(define Leonard (make-filho empty empty "Leonard" 1947 "brown"))
(define Rachel (make-filho Leonard Sandra "Rachel" 1969 "blue"))
(define Nora (make-filho empty empty "Nora" 1948 "blue"))
(define Charles (make-filho empty empty "Charles" 1948 "blue"))
(define Chandler (make-filho Charles Nora "Chandler" 1966 "blue"))
(define GloriaTribbiani (make-filho empty empty "GloriaTribbiani" 1950 "brown"))
(define MrTribbiani (make-filho empty empty "MrTribbiani" 1949 "brown"))
(define Joey (make-filho MrTribbiani GloriaTribbiani "Joey" 1969 "brown"))
(define Frank (make-filho empty empty "Frank" 1940 "brown"))
(define LilyBuffay (make-filho empty empty "LilyBuffay" 1940 "blue"))
(define Phoebe (make-filho Frank LilyBuffay "Phoebe" 1965 "blue"))
(define Carol (make-filho empty empty "Carol" 1965 "blue"))
(define Ben (make-filho Ross Carol "Ben" 1994 "blue"))
(define Emma (make-filho Ross Rachel "Emma" 2002 "blue"))

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================

;; maisIdoso : Filho -> Filho
;; Objetivo: dado um nó, retorna seu ancestral mais velho, que pode ser ele próprio.
;;           No caso de existirem mais de um ancestral com o mesmo ano de nascimento, retorna o pai ou seu ancestral
;;           mais velho (se houver).
;; Exemplos:
;;          (maisIdoso empty) = empty
;;          (maisIdoso Althea) = Althea
;;          (maisIdoso Nora) = Nora
;;          (maisIdoso Joey) = MrTribbiani

(define (maisIdoso filho)
  ;; Nos comentários desse código "empty" e "vazio" significam "falta de informação"
  ;; ou "desconhecido".
  (cond
    ;; Se o filho for desconhecido, retorna empty
    [(empty? filho) empty]

    ;; Se o pai e a mãe do filho (conhecido) forem desconhecidos, retorna o próprio filho
    [(and (empty? (filho-pai filho)) (empty? (filho-mãe filho))) filho]

    ;; Se o pai for desconhecido, retorna o ancestral mais velho da mãe
    [(empty? (filho-pai filho)) (maisIdoso (filho-mãe filho))]

    ;; Se a mãe for desconhecida, retorna o ancestral mais velho do pai
    [(empty? (filho-mãe filho)) (maisIdoso (filho-pai filho))]

    ;; Caso contrário, se filho, pai e mãe forem conhecidos
    [else
     (cond
       ;; Se o pai for mais velho ou tiver a mesma idade que a mãe, retorna
       ;; o ancestral mais velho do pai
       [(d1<=d2? (filho-data (filho-pai filho)) (filho-data (filho-mãe filho)))
        (maisIdoso (filho-pai filho))]

       ;; Caso contrário, se a mãe for mais velha, retorna o ancestral mais velho da mãe
       [else (maisIdoso (filho-mãe filho))]
       )]))

;; Testes:
(check-expect (maisIdoso empty) empty)
(check-expect (maisIdoso Althea) Althea)
(check-expect (maisIdoso Jack) Jack)
(check-expect (maisIdoso Judy) Althea)
(check-expect (maisIdoso Monica) Althea)
(check-expect (maisIdoso Ross) Althea)
(check-expect (maisIdoso Sandra) Sandra)
(check-expect (maisIdoso Leonard) Leonard)
(check-expect (maisIdoso Rachel) Leonard)
(check-expect (maisIdoso Nora) Nora)
(check-expect (maisIdoso Charles) Charles)
(check-expect (maisIdoso Chandler) Charles)
(check-expect (maisIdoso GloriaTribbiani) GloriaTribbiani)
(check-expect (maisIdoso MrTribbiani) MrTribbiani)
(check-expect (maisIdoso Joey) MrTribbiani)
(check-expect (maisIdoso Frank) Frank)
(check-expect (maisIdoso LilyBuffay) LilyBuffay)
(check-expect (maisIdoso Phoebe) Frank)
(check-expect (maisIdoso Carol) Carol)
(check-expect (maisIdoso Ben) Carol)
(check-expect (maisIdoso Emma) Althea)

;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;; ==============================================================

(define-struct nó (id conteúdo esq dir))
;; Um elemento nó de um conjunto AB (Arvore Binária) é:
;; 1. empty, representando a falta de informação, ou
;; 2. (make-nó i c e d), onde:
;;   i : Número, representa o identificador do nó;
;;   c : String, representa o conteúdo do nó;
;;   e : AB, representa a sub-árvore da esquerda;
;;   d : AB, representa a sub-árvore da direita.

;; Um elemento nó de um conjunto ABP (Árvore Binária de Pesquisa)
;; tem as mesmas propriedades de um elemento do conjunto AB, mas
;; com duas alterações:
;; 1. A sub-árvore da esquerda contém apenas nós com
;;    identificadores menores que id;
;; 2. A sub-árvore da direita contém apenas nós com
;;    identificadores maiores que id.

;; Exemplos de elementos do conjunto AB:
(define N (make-nó 2 "N" empty empty))
(define M (make-nó 60 "M" empty empty))
(define L (make-nó 52 "L" empty M))
(define K (make-nó 50 "K" empty L))
(define J (make-nó 5 "J" N empty))
(define I (make-nó 23 "I" J K))
(define H (make-nó 12 "H" empty empty))
(define G (make-nó 13 "G" H I))

(define F (make-nó 1 "F" empty empty))
(define E (make-nó 20 "E" empty empty))
(define D (make-nó 15 "D" F empty))
(define C (make-nó 3 "C" D E))
(define B (make-nó 12 "B" empty empty))
(define A (make-nó 10 "A" B C))

;; é-abp? : AB -> Booleano
;; Objetivo: dada uma árvore binária, retorna se ela é uma árvore binária de pesquisa.
;; Exemplos:
;;          (é-abp? A) = false
;;          (é-abp? F) = true
;;          (é-abp? G) = true
;;          (é-abp? empty) = true

(define (é-abp? ab)
  ;; Dada uma árvore binária ab
  (cond
    ;; Se ela for desconhecida, retorna false
    [(empty? ab) true]
    
    ;; Se ab for uma folha, retorna true
    [(and (empty? (nó-esq ab)) (empty? (nó-dir ab))) true]

    ;; Se o nó da esquerda de ab for desconhecido, retorna se o nó da direita é de pesquisa
    [(empty? (nó-esq ab)) (é-abp? (nó-dir ab))]

    ;; Se o nó da direita de ab for desconhecido, retorna se o nó da esquerda é de pesquisa
    [(empty? (nó-dir ab)) (é-abp? (nó-esq ab))]

    ;; Se o id do nó da direita de ab for menor ou igual que que o id de ab, ou
    ;; o id do nó da esquerda de ab for maior ou igual que o id de ab, retorna false
    [(or (<= (nó-id (nó-dir ab)) (nó-id ab))
         (>= (nó-id (nó-esq ab)) (nó-id ab))) false]

    ;; Caso contrário, retorna se o nó da esquerda e da direita são árvores de pesquisa
    [else (and (é-abp? (nó-esq ab))
               (é-abp? (nó-dir ab)))]))

;; Testes:
(check-expect (é-abp? empty) true)
(check-expect (é-abp? A) false)
(check-expect (é-abp? C) false)
(check-expect (é-abp? F) true)
(check-expect (é-abp? G) true)
(check-expect (é-abp? K) true)
