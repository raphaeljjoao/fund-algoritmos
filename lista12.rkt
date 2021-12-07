;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista12-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 12

;; ========================================================================== 
;; ========================== Definição de Dados ============================
;; ==========================================================================

(define-struct cidade (nome vizinhas))
;; Um elemento do conjunto Cidade é um par (make-cidade n v), onde:
;; n : String, representa o nome da cidade;
;; v : ListaDeString, representa as (nomes das) cidades vizinhas.

;; Um Mapa é:
;; 1. empty, ou
;; 2. (cons n g), onde:
;;   n : Cidade
;;   g : Mapa

;; Um elemento do conjunto ListaDeString é:
;; 1. empty, ou
;; 2. (cons s ls), onde:
;;   s: String
;;   ls: ListaDeString

;; Um elemento do conjunto ListaDeStringOuFalse:
;; 1. ListaDeString, ou
;; 2. Booleano (false)

;; ========================================================================== 
;; ============================== Questão 1 =================================
;; ==========================================================================

(define MAPA (list (make-cidade "Toronto" (list "SaultSteMarie" "Duluth" "Chicago" "Pittsburgh"))
                   (make-cidade "Duluth" (list "Toronto" "Winnipeg" "SaultSteMarie"
                                             "Helena" "Omaha" "Chicago"))
                   (make-cidade "Chicago" (list "Toronto" "SaintLouis" "Omaha" "Duluth"
                                              "Pittsburgh"))
                   (make-cidade "Pittsburgh" (list "Toronto" "Chicago" "Nashville" "SaintLouis"))
                   (make-cidade "SaultSteMarie" (list "Toronto" "Duluth" "Winnipeg"))
                   (make-cidade "Winnipeg" (list "Duluth" "Helena" "SaultSteMarie"))
                   (make-cidade "Helena" (list "Winnipeg" "Denver" "Duluth" "Omaha"))
                   (make-cidade "Denver" (list "Helena" "SantaFe" "OklahomaCity" "Omaha"
                                             "KansasCity"))
                   (make-cidade "SantaFe" (list "Denver" "OklahomaCity"))
                   (make-cidade "OklahomaCity" (list "Denver" "SantaFe" "KansasCity"
                                                   "LittleRock"))
                   (make-cidade "Omaha" (list "Helena" "Denver" "KansasCity" "Duluth" "Chicago"))
                   (make-cidade "SaintLouis" (list "Pittsburgh" "KansasCity" "Nashville" "Chicago"
                                                 "LittleRock"))
                   (make-cidade "Nashville" (list "Pittsburgh" "SaintLouis" "Atlanta" "Little Rock"))
                   (make-cidade "LittleRock" (list "OklahomaCity" "SaintLouis" "Nashville"))
                   (make-cidade "KansasCity" (list "OklahomaCity" "Denver" "Omaha" "SaintLouis"))
                   (make-cidade "Atlanta" (list "Nashville"))))

;; ========================================================================== 
;; ============================== Questão 2 =================================
;; ==========================================================================

;; está-na-lista? : String ListaDeString -> Booleano
;; Objetivos: Dadas uma string e uma lista de string, retorna se a string dada está na lista.
;; Exemplos:
;;          (está-na-lista? "Porto Alegre" (list "Canoas" "Viamão" "Novo Hamburgo")) retorna false
;;          (está-na-lista? "Paris" (list "Marselha" "Lyon" "Paris")) retorna true

(define (está-na-lista? s ls)
  ;; Dadas uma string s e uma lista de string ls
  (cond
    ;; Se ls estiver vazia, retorna false
    [(empty? ls) false]

    ;; Se a primeira string de ls for igual a s, retorna true
    [(string=? s (first ls)) true]

    ;; Caso contrário, retorna se s está no resto de ls
    [else (está-na-lista? s (rest ls))]
    )
  )

;; Testes:
(check-expect (está-na-lista? "abc" empty) false)
(check-expect (está-na-lista? "Porto Alegre" (list "Canoas" "Viamão" "Novo Hamburgo")) false)
(check-expect (está-na-lista? "João" (list "Davi" "Nicolas" "Bruno")) false)
(check-expect (está-na-lista? "Nicolas" (list "Davi" "Nicolas" "Bruno")) true)
(check-expect (está-na-lista? "Paris" (list "Marselha" "Lyon" "Paris")) true)

;; subtrai-lista : ListaDeString ListaDeString -> ListaDeString
;; Objetivo: Dadas duas listas de string, devolve todos os elementos da primeira lista
;; que não estão na segunda.
;;          (subtrai-lista (list "Porto Alegre" "Canoas")
;;                         (list "Canoas" "Viamão" "Novo Hamburgo")) retorna
;;          (list "Porto Alegre")
;;          (subtrai-lista (list "Rio Branco" "Salvador") empty) retorna
;;          (list "Rio Branco" "Salvador")

(define (subtrai-lista ls1 ls2)
  (cond
    ;; Se ls1 ou ls2 estiverem vazias, retorna ls1
    [(or (empty? ls1)
         (empty? ls2)) ls1]

    ;; Caso contrário, junta as seguintes listas de strings
    [else
     (append
      (cond
        ;; Se a primeira string de ls1 não aparecer em ls2, uma lista com somente essa string
        [(not (está-na-lista? (first ls1) ls2)) (list (first ls1))]
        ;; Caso contrário, uma lista vazia
        [else empty])
      ;; Uma lista com os elementos do resto de ls1 que não aparecem em ls2
      (subtrai-lista (rest ls1) ls2))]
    )
  )

(check-expect (subtrai-lista empty empty) empty)
(check-expect (subtrai-lista (list "Rio Branco" "Salvador") empty) (list "Rio Branco" "Salvador"))
(check-expect (subtrai-lista (list "Porto Alegre" "Canoas")
                             (list "Canoas" "Viamão" "Novo Hamburgo")) (list "Porto Alegre"))
(check-expect (subtrai-lista (list "Marcelo" "Isadora") empty) (list "Marcelo" "Isadora"))
(check-expect (subtrai-lista (list "Davi" "João") (list "Davi" "Nicolas" "Bruno")) (list "João"))
(check-expect (subtrai-lista (list "Davi" "João") empty) (list "Davi" "João"))

;; ========================================================================== 
;; ============================== Questão 3 =================================
;; ==========================================================================

;; vizinhos : String Mapa ListaDeString -> ListaDeString
;; Objetivo: Dados o nome de uma cidade, um mapa e uma lista com nomes de cidades visitadas,
;; retorna o nome das cidades vizinhas do nome da cidade dado que não constam na lista de cidades
;; visitadas.
;; Exemplos:
;;          (vizinhos "Toronto" MAPA (list "Toronto" "SaintLouis" "Omaha" "Duluth" "Pittsburgh"))
;;          retorna (list "SaultSteMarie" "Chicago")
;;          (vizinhos "OklahomaCity" MAPA (list "Denver" "OklahomaCity")) retorna
;;          (list "SantaFe" "KansasCity" "LittleRock")

(define (vizinhos cidade mapa visitadas)
  (cond
    ;; Se o mapa estiver vazio, retorna uma lista vazia
    [(empty? mapa) empty]

    ;; Se a primeiro cidade do mapa tiver o nome cidade, retorna o nome das cidades vizinhas dessa
    ;; cidade que não constam em visitadas
    [(string=? cidade (cidade-nome (first mapa)))
     (subtrai-lista (cidade-vizinhas (first mapa)) visitadas)]

    ;; Caso contrário, retorna o nome das cidades vizinhas de que não constam na lista de cidades
    ;; visitadas do resto de mapa
    [else (vizinhos cidade (rest mapa) visitadas)]
    )
  )

;; Testes:
(check-expect (vizinhos "Toronto" empty (list "Omaha" "Duluth")) empty)
(check-expect (vizinhos "Nashville" MAPA (list "Pittsburgh" "SaintLouis" "Atlanta" "Little Rock"))
              empty)
(check-expect (vizinhos "Toronto" MAPA (list "Toronto" "SaintLouis" "Omaha" "Duluth" "Pittsburgh"))
              (list "SaultSteMarie" "Chicago"))
(check-expect (vizinhos "OklahomaCity" MAPA (list "Denver" "OklahomaCity"))
              (list "SantaFe" "KansasCity" "LittleRock"))

;; ========================================================================== 
;; ============================== Questão 4 =================================
;; ==========================================================================

;; encontra-caminho : String String Mapa ListaDeString -> ListaDeStringOuFalse
;; Objetivo: Dados os nomes das cidades origem e destino, um mapa e uma lista de cidades já
;; visitadas, encontra um caminho entre a origem e o destino. Se não existir caminho, retorna
;; false.
;; Observação: As cidades dadas devem fazer parte do mapa.
;; Exemplos:
;;          (encontra-caminho "Nashville" "Desconhecida" MAPA empty) retorna false
;;          (encontra-caminho "Duluth" "Toronto" MAPA empty) retorna (list "Duluth" "Toronto")
;;          (encontra-caminho "SantaFe" "Atlanta" MAPA empty) retorna
;;          (list "SantaFe" "Denver" "Helena" "Winnipeg" "Duluth" "Toronto" "Chicago"
;;                "SaintLouis" "Pittsburgh" "Nashville" "Atlanta")

(define (encontra-caminho origem destino mapa visitadas)
  (cond
    ;; Se a origem for igual ao destino, retorna uma lista com o destino
    [(string=? origem destino) (list destino)]

    ;; Caso contrário
    [else
     ;; Define localmente
     (local
       (
        ;; Os visitados, incluindo a origem (que agora é uma cidade já visitada)
        (define visitadasNovo (append (list origem) visitadas))
        ;; Vizinhos não visitados do mapa origem e o destino, se existir caminho
        (define caminho (encontra-caminho-vizinhos (vizinhos origem mapa visitadasNovo)
                                                   destino mapa visitadasNovo))
        )
       
       (cond
         ;;
         [(list? caminho) (append (list origem) caminho)]

         ;; Caso contrário, se caminho for um booleano (não há caminho), retorna false
         [else false])
       )]
    )
  )

;; Testes:
(check-expect (encontra-caminho "Toronto" "Desconhecida" MAPA empty) false)
(check-expect (encontra-caminho "Toronto" "Toronto" MAPA empty) (list "Toronto"))
(check-expect (encontra-caminho "Toronto" "Nashville" MAPA empty)
              (list "Toronto" "SaultSteMarie" "Duluth" "Winnipeg" "Helena"
                    "Denver" "SantaFe" "OklahomaCity" "KansasCity" "Omaha" "Chicago"
                    "SaintLouis" "Pittsburgh" "Nashville"))
(check-expect (encontra-caminho "Nashville" "Toronto" MAPA empty)
              (list "Nashville" "Pittsburgh" "Toronto"))
(check-expect (encontra-caminho "Helena" "SantaFe" MAPA empty)
              (list "Helena" "Winnipeg" "Duluth" "Toronto" "Chicago" "SaintLouis" "KansasCity"
                    "OklahomaCity" "Denver" "SantaFe"))
(check-expect (encontra-caminho "KansasCity" "Omaha" MAPA empty)
              (list "KansasCity" "OklahomaCity" "Denver" "Helena" "Winnipeg"
                    "Duluth" "Toronto" "Chicago" "Omaha"))

;; encontra-caminho-vizinhos : ListaDeString String Mapa ListaDeString -> ListaDeStringOuFalse
;; Objetivo: Dados uma lista de cidades origem, um destino, um mapa e uma lista de cidades já
;; visitadas, encontra um caminho entre alguma das origens e o destino. Se não existir caminho,
;; devolve false.
;; Observação: As cidades dadas devem fazer parte do mapa.
;; Exemplos:
;;          (encontra-caminho-vizinhos empty "Atlanta" MAPA empty) retorna false
;;          (encontra-caminho-vizinhos (list "Toronto" "Duluth") "Winnipeg" MAPA empty) retorna
;;          (list "Toronto" "SaultSteMarie" "Duluth" "Winnipeg")

(define (encontra-caminho-vizinhos origem destino mapa visitadas)
  (cond
    ;; Se a lista de origens for vazia, retornar false
    [(empty? origem) false]

    ;; Caso contrário
    [else
     ;; Definir localmente
     (local
       (
        ;; Caminho entre a primeira cidade da lista de origens e o destino,
        ;; não passando pelas cidades já visitadss, se não existir caminho, false é armazenado
        (define caminho (encontra-caminho  (first origem) destino mapa visitadas))
        )
       
       (cond
         ;; Se caminho for uma lista (foi encontrado um caminho), retorna o caminho
         [(list? caminho) caminho]

         ;; Caso contrário, procura um caminho no mapa do resto da lista de origens até o destino
         [else (encontra-caminho-vizinhos (rest origem) destino mapa visitadas)])
       )]
    )
  )

;; Testes:
(check-expect (encontra-caminho-vizinhos empty "Atlanta" MAPA empty) false)
(check-expect (encontra-caminho-vizinhos (list "Desconhecida 1" "Desconhecida 2") "Toronto"
                                         MAPA empty)
              false)
(check-expect (encontra-caminho-vizinhos (list "Omaha" "Toronto") "Nashville" MAPA empty)
              (list "Omaha" "Helena" "Winnipeg" "Duluth" "Toronto" "Chicago"
                    "SaintLouis" "Pittsburgh" "Nashville"))

(check-expect (encontra-caminho-vizinhos (list "Toronto" "Duluth") "Winnipeg" MAPA empty)
              (list "Toronto" "SaultSteMarie" "Duluth" "Winnipeg"))

;; ========================================================================== 
;; ============================== Questão 5 =================================
;; ==========================================================================

;; Um elemento do conjunto ListaDeListaDeString é:
;; 1. empty, ou
;; 2. (cons ls lls), onde:
;;   ls: ListaDeString
;;   lls: ListaDeListaDeString

;; monta-caminhos: String ListaDeListaDeString -> ListaDeListaDeString
;; Objetivo: Dados um nome e uma lista de listas de strings, retorna uma lista de listas,
;; onde cada uma das listas tem como primeiro elemento o nome dado na entrada.
;; Exemplos:
;;          (monta-caminhos "A" empty) retorna empty
;;          (monta-caminhos "A" (list (list "B" "C") (list "D))) retorna
;;          (list (list "A" "B" "C") (list  "A" "D"))

(define (monta-caminhos nome lista)
  (cond
    ;; Se a lista estiver vazia, retornar uma lista vazia
    [(empty? lista) empty]

    ;; Caso contrário
    [else
     ;; Monta uma lista com
     (cons
      ;; A lista gerada colocando nome na frente da primeira lista de lista
      (cons nome (first lista))
      ;; E com as listas geradas colocando nome na frente das outras listas de lista
      (monta-caminhos nome (rest lista))
      )]
    )
  )

;; Testes:
(check-expect (monta-caminhos "A" empty) empty)
(check-expect (monta-caminhos "A" (list (list "B" "C") (list "D")))
              (list (list "A" "B" "C") (list  "A" "D")))

;; encontra-todos-caminhos : String String Mapa -> ListaDeListaDeString
;; Objetivo: Dados os nomes das cidades origem e destino e um mapa, retorna todos os caminhos
;; possíveis entre a origem e o destino.
;; Observação: As cidades dadas devem fazer parte do mapa.

;; ========================================================================== 
;; ============================== Questão 6 =================================
;; ==========================================================================

;; mostra-caminhos : String String Mapa -> String
;; Objetivo: Dados os nomes das cidades origem e destino e um mapa, gera uma string contendo
;; na primeira linha o número de caminhos existentes da cidade origem até o destino e depois
;; a lista de todos os caminhos.

;; ========================================================================== 
;; ============================== Questão 7 =================================
;; ==========================================================================