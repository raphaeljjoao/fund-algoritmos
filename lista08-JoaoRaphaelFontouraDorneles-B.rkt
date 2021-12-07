;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista8-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles (333315)
;;;;;;;;;;;; 8

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================
;; ============================
;; DEFINIÇÕES DE TIPOS DE DADOS
;; ============================
;;   TIPO RETANGULO:
(define-struct retangulo (lado1 lado2 cor nome))
;; Um elemento de Retangulo tem o formato
;;    (make-retangulo  l1 l2 c n), onde:
;;     l1 : Número, é a largura do retângulo;
;;     l2 : Número, é a altura do retângulo;
;;     c  : String, é a cor do retângulo;
;;     n  : String, é o nome do retângulo.
(define R1 (make-retangulo 30 60 "red" "R1"))
(define R2 (make-retangulo 40 40 "green" "R2"))

;;   TIPO TRIANGULO:
(define-struct triangulo (lado cor nome))
;; Um elemento de Triangulo tem o formato
;;    (make-triangulo  l c n), onde:
;;     l : Número, é o lado do triângulo;
;;     c : String, é a cor do triângulo;
;;     n : String, é o nome do triângulo.
(define T1 (make-triangulo 30  "blue" "T1"))
(define T2 (make-triangulo 40  "red" "T2"))

;;   TIPO CIRCULO:
(define-struct circulo (raio cor nome))
;; Um elemento de Circulo tem o formato
;;    (make-circulo  r c n), onde:
;;     r : Número, é o raio do circulo;
;;     c : String, é a cor do circulo;
;;     n : String, é o nome do circulo.
(define C1 (make-circulo 20  "pink" "C1"))
(define C2 (make-circulo 30  "red" "C2"))

;; Uma Forma pode ser
  ;; 1. um Retangulo, ou
  ;; 2. um Triangulo, ou
  ;; 3. um Circulo

;; Uma ListaDeFormas é
;; 1. vazia (empty), ou
;; 2. (cons f lf), onde
;;   f : Forma
;;  lf : ListaDeFormas

;; Uma ListaDeStrings é
;; 1. vazia (empty), ou
;; 2. (cons s ls), onde
;;   s : String
;;   ls : ListaDeStrings

;; Uma ListaDeNúmeros é
;; 1. vazia (empty), ou
;; 2. (cons n ln), onde
;;   n : Número
;;   ln : ListaDeNúmeros

;; Constantes do tipo ListaDeFormas:
  (define L1 (cons C1 empty))
  (define L2 (cons C1 (cons C2 (cons T1 (cons T2 (cons R1 (cons R2 empty)))))))
  (define L3 (cons T1 (cons T1 (cons R2 (cons T1 (cons R1 (cons T2 (cons R1 empty))))))))
  (define L4 (cons C2 (cons C2 (cons C2 (cons T2 empty)))))

;; =====================
;; DEFINIÇÕES DE FUNÇÕES
;; =====================

;; desenha : Forma -> Imagem
;; Dada uma forma, gera uma imagem desta forma.
;; Exemplos:

  (define (desenha f)
    (cond
      [(retangulo? f)
       (rectangle (retangulo-lado1 f) (retangulo-lado2 f) "solid" (retangulo-cor f))]
      [(triangulo? f)
       (triangle  (triangulo-lado f)  "solid" (triangulo-cor f))]
      [(circulo? f)
       (circle    (circulo-raio f)    "solid" (circulo-cor f))]))
;; Testes:
(check-expect (desenha C1) (circle   20 "solid" "pink"))
(check-expect (desenha T2) (triangle 40 "solid" "red"))

;; ==============================================================
;; A A A A A A A A A ==> lista-retangulos
;; ==============================================================
;; lista-retangulos : ListaDeFormas -> ListaDeFormas
;; Objetivo: Dada uma lista de formas, devolve uma lista com todos os 
;;           retângulos da lista original.
;; Exemplos:
;;          (lista-retangulos L4) =  empty
;;          (lista-retangulos L2) = (cons R1 (cons R2 empty))
(define (lista-retangulos lf)
  ;; Filtro: se um elemento de lf for do conjunto Retangulo, ele permanece na lista
  (filter retangulo? lf))

;; Testes:
(check-expect (lista-retangulos L1) empty)
(check-expect (lista-retangulos L2) (cons R1 (cons R2 empty)))
(check-expect (lista-retangulos L3) (cons R2 (cons R1 (cons R1 empty))))
(check-expect (lista-retangulos L4) empty)


;; ==============================================================
;; B B B B B B B B  ==> lista-nomes-retangulos
;; ==============================================================
;; lista-nomes-retangulos : ListaDeFormas -> ListaDeStrings
;; Objetivo: Dada uma lista de formas, devolve uma lista de strings com os nomes de
;;           todos os retângulos da lista original.
;; Exemplos:
;;          (lista-nomes-retangulos L3) =  (cons "R2" (cons "R1" (cons "R1" empty)))
;;          (lista-nomes-retangulos L2) = (cons "R1" (cons "R2" empty))
(define (lista-nomes-retangulos lf)
  ;; Cria uma ListaDeStrings com o nome dos retângulos da lista de retângulos de lf
  (map retangulo-nome (lista-retangulos lf)))

;; Testes:
(check-expect (lista-nomes-retangulos L1) empty)
(check-expect (lista-nomes-retangulos L2) (cons "R1" (cons "R2" empty)))
(check-expect (lista-nomes-retangulos L3) (cons "R2" (cons "R1" (cons "R1" empty))))
(check-expect (lista-nomes-retangulos L4) empty)

;; ==============================================================
;; C C C C C C C C  ==> lista-retangulos-img
;; ==============================================================
;; lista-retangulos-img : ListaDeFormas -> Imagem
;; Objetivo: Dada uma lista de formas, devolve uma imagem com todos
;;           os retângulos da lista original lado a lado, na ordem inversa 
;;           na qual eles aparecem na lista.

(define (lista-retangulos-img lf)
  ;; Junta lado a lado cada imagem, a ordem é do fim para o início
  (foldl beside empty-image
         ;; Desenha cada elemento da lista de retângulos de lf
         (map desenha (lista-retangulos lf))))

;; Testes:
(check-expect (lista-retangulos-img L1) empty-image)
(check-expect (lista-retangulos-img L2) (beside
                                         (desenha R2)
                                         (desenha R1)
                                         empty-image))
(check-expect (lista-retangulos-img L3) (beside
                                         (desenha R1)
                                         (desenha R1)
                                         (desenha R2)
                                         empty-image))
(check-expect (lista-retangulos-img L4) empty-image)

;; ==============================================================
;; D D D D D D D D ==> soma-raios-circulos
;; ==============================================================
;; soma-raios-circulos : ListaDeFormas -> Número
;; Objetivo: Dada uma lista de formas, devolve a soma dos tamanhos dos raios
;;           dos círculos dessa lista.
;; Exemplo: (soma-raios-circulos L2) = 50
(define (soma-raios-circulos lf)
  ;; Soma 0 + todos os elementos da lista de números criada, a ordem é do fim para o início
  (foldl + 0
         ;; Cria uma lista de números com o raio dos círculos da lista filtrada
         (map circulo-raio
              ;; Filtro: se um elemento de lf for do conjunto Circulo, ele permanece na lista
              (filter circulo? lf))))

;; Testes:
(check-expect (soma-raios-circulos L1) 20)
(check-expect (soma-raios-circulos L2) 50)
(check-expect (soma-raios-circulos L3) 0)
(check-expect (soma-raios-circulos L4) 90)

;; ==============================================================
;; E E E E E E E E ==> desenha-lista-formas
;; ==============================================================
;; desenha-lista-formas: ListaDeFormas -> Imagem
;; Objetivo: Dada uma lista de formas, monta uma imagem com todas as
;;           formas lado a lado, na mesma ordem na qual elas aparecem na lista.

(define (desenha-lista-formas lf)
  ;; Junta lado a lado cada imagem, a ordem é do fim para o início
  (foldl beside empty-image
         ;; Inverte a lista
         (foldl cons empty
                ;; Desenha cada elemento de lf
                (map desenha lf))))

;; Testes:
(check-expect (desenha-lista-formas L1) (desenha C1))
(check-expect (desenha-lista-formas L2) (beside
                                         (desenha C1) (desenha C2)
                                         (desenha T1) (desenha T2)
                                         (desenha R1) (desenha R2)))
(check-expect (desenha-lista-formas L3) (beside
                                         (desenha T1) (desenha T1)
                                         (desenha R2) (desenha T1)
                                         (desenha R1) (desenha T2) (desenha R1)))
(check-expect (desenha-lista-formas L4) (beside
                                         (desenha C2) (desenha C2)
                                         (desenha C2) (desenha T2)))


;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================


(define-struct filme (nome ano categoria estrelas disponivel?))
;; Um elemento do conjunto Filme tem o formato
;;    (make-filme n a c e d) onde
;;     n : String, é o nome do filme
;;     a : ano, é o ano de lançamento do filme
;;     c : String, é a categoria do filme, que pode ser "Aventura", "Ficção" ou "Comédia"
;;     e : Número, é o número médio de estrelas que o filme recebeu dos espectadores
;;     d : Booleano, diz se o filme está disponível ou não no momento

;; Constantes do tipo Filme:
(define AVANGERS (make-filme "Os Vingadores" 2012 "Aventura" 4.6 true))
(define JV (make-filme "Jogos Vorazes" 2012 "Aventura" 4.6 true))
(define JV2 (make-filme "Jogos Vorazes - Em Chamas" 2013 "Aventura" 4.7 true))
(define JV3 (make-filme "Jogos Vorazes: A Esperança – Parte 1" 2014 "Aventura" 4.4 true))
(define JV4 (make-filme "Jogos Vorazes: A Esperança - O Final" 2015 "Aventura" 4.2 true))

(define TW01 (make-filme "2001 - Uma Odisseia no Espaço" 1968 "Ficção" 4.2 false))
(define TW12 (make-filme "2012" 2009 "Ficção" 4.1 true))
(define INTERS (make-filme "Interestelar" 2014 "Ficção" 4.9 true))

(define SALAF (make-filme "Os Salafrários" 2021 "Comédia" 2.9 true))
(define MMP (make-filme "Minha Mãe É uma Peça" 2013 "Comédia" 4.2 true))
(define MMP2 (make-filme "Minha Mãe É uma Peça 2" 2016 "Comédia" 4.1 true))
(define MMP3 (make-filme "Minha Mãe É uma Peça 3" 2019 "Comédia" 4.7 true))

;; Uma ListaDeFilmes é
;; 1. vazia (empty), ou
;; 2. (cons f lf), onde
;;   f : Filme
;;   lf : ListaDeFilmes

;; Constantes do tipo ListaDeFilmes:
(define AVENTURA (cons AVANGERS (cons JV (cons JV2 (cons JV2 (cons JV4 empty))))))
(define FICÇÃO (cons TW01 (cons TW12 (cons INTERS empty))))
(define COMÉDIA (cons SALAF (cons MMP (cons MMP2 (cons MMP3 empty)))))
(define TODOS (cons AVANGERS (cons JV (cons JV2 (cons JV2 (cons JV4
              (cons TW01 (cons TW12 (cons INTERS
              (cons SALAF (cons MMP (cons MMP2 (cons MMP3 empty)))))))))))))
(define LF1 (cons SALAF (cons INTERS (cons AVANGERS empty))))
(define LF2 (cons SALAF (cons INTERS (cons AVANGERS (cons MMP3
                                                          (cons TW01 (cons JV empty)))))))

;; =========================================================
;; A A A A A A A A A A A A A A A A A A A A A A A A A A A A A
;; =========================================================

;; comédia? : Filme -> Booleano
;; Objetivo: Dado um filme, retorna se ele é da categoria comédia.
;; Exemplos:
;;          (comédia? JV2) = false
;;          (comédia? SALAF) = true
(define (comédia? f)
  (string=? "Comédia" (filme-categoria f)))

;; Testes:
(check-expect (comédia? JV2) false)
(check-expect (comédia? TW01) false)
(check-expect (comédia? TW12) false)
(check-expect (comédia? SALAF) true)
(check-expect (comédia? MMP2) true)

;; ficção? : Filme -> Booleano
;; Objetivo: Dado um filme, retorna se ele é da categoria ficção.
;; Exemplos:
;;          (ficção? JV2) = false
;;          (ficção? INTERS) = true
(define (ficção? f)
  (string=? "Ficção" (filme-categoria f)))

;; Testes:
(check-expect (ficção? JV2) false)
(check-expect (ficção? SALAF) false)
(check-expect (ficção? TW01) true)
(check-expect (ficção? TW12) true)

;; estrelas>=4 : Filme -> Booleano
;; Objetivo: Dado um filme, retorna se ele obteve uma avaliação maior ou igual a 4 estrelas.
;; Exemplos:
;;          (estrelas>=4 SALAF) = false
;;          (estrelas>=4 INTERS) = true

(define (estrelas>=4 f)
  (>= (filme-estrelas f) 4))

;; Testes:
(check-expect (estrelas>=4 SALAF) false)
(check-expect (estrelas>=4 JV2) true)
(check-expect (estrelas>=4 TW01) true)
(check-expect (estrelas>=4 TW12) true)
(check-expect (estrelas>=4 MMP2) true)

;; disponível? : Filme -> Booleano
;; Objetivo: Dado um filme, retorna se ele está disponível no momento.
;;          (disponível? TW01) = false
;;          (disponível? JV) = true

(define (disponível? f)
  (filme-disponivel? f))

;; Testes:
(check-expect (disponível? TW01) false)
(check-expect (disponível? JV) true)
(check-expect (disponível? JV2) true)
(check-expect (disponível? TW12) true)
(check-expect (disponível? MMP2) true)

;; =========================================================
;; B B B B B B B B B B B B B B B B B B B B B B B B B B B B B 
;; =========================================================

;; filtra-ficção : ListaDeFilmes -> ListaDeStrings
;; Objetivo: Dada uma lista de filmes, retorna uma lista de strings com o nome dos
;;           filmes de ficção.
;; Exemplos:
;;          (filtra-ficção AVENTURA) = empty
;;          (filtra-ficção LF1) = (cons "Interestelar" empty)
;;          (filtra-ficção LF2) =
;;          (cons "Interestelar" (cons "2001 - Uma Odisseia no Espaço" empty))

(define (filtra-ficção lf)
  ;; Cria uma ListaDeStrings com o nome dos filmes da lista filtrada
  (map filme-nome
       ;; Filtro: se um elemento de lf for da categoria ficção, ele permance na lista
       (filter ficção? lf)))

;; Testes:
(check-expect (filtra-ficção AVENTURA) empty)
(check-expect (filtra-ficção COMÉDIA) empty)
(check-expect (filtra-ficção FICÇÃO) (cons "2001 - Uma Odisseia no Espaço"
                                           (cons "2012"
                                                 (cons "Interestelar" empty))))
(check-expect (filtra-ficção LF1) (cons "Interestelar" empty))
(check-expect (filtra-ficção LF2) (cons "Interestelar"
                                        (cons "2001 - Uma Odisseia no Espaço" empty)))

;; =========================================================
;; C C C C C C C C C C C C C C C C C C C C C C C C C C C C C 
;; =========================================================

;; filtra-ficção-disp : ListaDeFilmes -> ListaDeStrings
;; Objetivo: Dada uma lista de filmes, retorna uma lista de strings com o nome dos
;;           filmes de ficção que estão disponíveis no momento.
;; Exemplos:
;;          (filtra-ficção AVENTURA) = empty
;;          (filtra-ficção LF1) = (cons "Interestelar" empty)
;;          (filtra-ficção TODOS) = (cons "2012" (cons "Interestelar" empty))

(define (filtra-ficção-disp lf)
  ;; Cria uma lista de strings com o nome dos filmes da lista duplamente filtrada
  (map filme-nome
       ;; Filtro: se um elemento de lf estiver disponível no momento, ele permance na lista
       (filter disponível?
               ;; Filtro: se um elemento de lf for da categoria ficção, ele permance na lista
               (filter ficção? lf))))

;; Testes:
(check-expect (filtra-ficção-disp AVENTURA) empty)
(check-expect (filtra-ficção-disp COMÉDIA) empty)
(check-expect (filtra-ficção-disp FICÇÃO) (cons "2012" (cons "Interestelar" empty)))
(check-expect (filtra-ficção-disp LF1) (cons "Interestelar" empty))
(check-expect (filtra-ficção-disp LF2) (cons "Interestelar" empty))
(check-expect (filtra-ficção-disp TODOS) (cons "2012" (cons "Interestelar" empty)))

;; =========================================================
;; D D D D D D D D D D D D D D D D D D D D D D D D D D D D D
;; =========================================================

;; média-estrelas : ListaDeFilmes -> Número
;; Objetivo: Dado uma lista de filmes não vazia, retorna a média de estrelas dela.
;; Exemplos:
;;          (média-estrelas AVENTURA) = 4.56
;;          (média-estrelas FICÇÃO) = 4.4
;;          (média-estrelas COMÉDIA) = 3.975

(define (média-estrelas lf)
  ;; Média = soma dos elementos / número de elementos
  (/
   ;; Soma 0 + todos os elementos da lista de números criada, a ordem é do fim para o início
   (foldl + 0
          ;; Cria uma lista de números com as estrelas dos filmes de lf
          (map filme-estrelas lf))
   (length lf)))

;; Testes:
(check-expect (média-estrelas AVENTURA) 4.56)
(check-expect (média-estrelas FICÇÃO) 4.4)
(check-expect (média-estrelas COMÉDIA) 3.975)
(check-expect (média-estrelas TODOS) 4.325)
(check-within (média-estrelas LF1) 4.13 0.01)
(check-within (média-estrelas LF2) 4.316 0.01)

;; =========================================================
;; E E E E E E E E E E E E E E E E E E E E E E E E E E E E E
;; =========================================================

;; filtra-lista-filmes : ListaDeFilmes (Filme -> Booleano) -> ListaDeFilmes
;; Objetivo: Dada uma lista de filmes e uma função, retorna uma lista dos filmes
;;           que passam no critério estabelecido pela função.
;; Exemplos:
;;          (filtra-lista-filmes TODOS (λ (f) (not (disponível? f)))) = (cons TW01 empty)
;;          (filtra-lista-filmes TODOS (λ (f) (<= (filme-estrelas f) 3))) = (cons SALAF empty)

(define (filtra-lista-filmes lf criterio)
  ;; Filtro: se um elemento de lf passar no criterio, ele permance na lista
  (filter criterio lf))

;; Testes:
(check-expect (filtra-lista-filmes TODOS (λ (f) (not (disponível? f)))) (cons TW01 empty))
(check-expect (filtra-lista-filmes TODOS (λ (f) (<= (filme-estrelas f) 3))) (cons SALAF empty))
(check-expect (filtra-lista-filmes AVENTURA ficção?) empty)
(check-expect (filtra-lista-filmes COMÉDIA comédia?) COMÉDIA)
(check-expect (filtra-lista-filmes LF1 comédia?) (cons SALAF empty))
(check-expect (filtra-lista-filmes LF1 estrelas>=4) (cons INTERS (cons AVANGERS empty)))
(check-expect (filtra-lista-filmes LF2 disponível?)
              (cons SALAF (cons INTERS (cons AVANGERS (cons MMP3 (cons JV empty))))))
(check-expect (filtra-lista-filmes LF2 (λ (f) (and (disponível? f) (estrelas>=4 f))))
              (cons INTERS (cons AVANGERS (cons MMP3 (cons JV empty)))))

;; =========================================================
;; F F F F F F F F F F F F F F F F F F F F F F F F F F F F F
;; =========================================================

;; filtra-filmes-disp-ano : ListaDeFilmes Número -> ListaDeFilmes
;; Objetivo: Dada uma lista de filmes e um ano, retorna uma lista com os filmes
;;           disponíveis lançados a partir desse ano.
;; Exemplos:
;;          (filtra-filmes-disp-ano FICÇÃO 2010) = (cons INTERS empty)
;;          (filtra-filmes-disp-ano COMÉDIA 2019) = (cons SALAF (cons MMP3 empty))

(define (filtra-filmes-disp-ano lf ano)
  ;; Filtro: se um elemento de lf estiver disponível no momento e tiver sido
  ;; lançado a partir do ano, ele permance na lista
  (filter (λ (f) (and (disponível? f)
                      (>= (filme-ano f) ano))) lf))

;; Testes:
(check-expect (filtra-filmes-disp-ano FICÇÃO 2010) (cons INTERS empty))
(check-expect (filtra-filmes-disp-ano COMÉDIA 2019) (cons SALAF (cons MMP3 empty)))
(check-expect (filtra-filmes-disp-ano LF1 2014) (cons SALAF (cons INTERS empty)))
(check-expect (filtra-filmes-disp-ano LF2 2014) (cons SALAF (cons INTERS (cons MMP3 empty))))