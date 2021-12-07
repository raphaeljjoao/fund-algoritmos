;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista10-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 10

;; ============================================
;; DEFINIÇÕES DE TIPOS DE DADOS
;; ============================================

;; ------------------
;; TIPO ARQUIVO:
;; ------------------
(define-struct arquivo (nome tamanho)) 
;; Um elemento do conjunto Arquivo tem o formato
;;  (make-arquivo n t), onde
;;    n : String, é o nome do arquivo
;;    t : Número, é o tamanho do arquivo, em Kb.

;; ------------------
;; TIPO CONTEUDO:
;; ------------------
;; Um Conteudo é
;; 1. empty,
;; 2. (cons a lc), onde:
;;   a : Arquivo
;;   lc: Conteudo
;; 3. (cons d lc), onde:
;;   d: Diretorio
;;   lc: Conteudo

;; ------------------
;; TIPO DIRETORIO:
;; ------------------
(define-struct diretorio (nome conteudo))
;; Um elemento do conjunto Diretorio tem o formato
;;  (make-diretorio n c), onde
;;    n : String, é o nome do diretório
;;    c : Conteudo, é o conteúdo do diretório

;; =========================================================================
;;                                 QUESTÃO 1
;; =========================================================================

;; Tamanhos de arquivos:
(define PNG 100)
(define TXT 50)
(define RKT 50)
(define MP3 2000)
(define DIR 10)

;; Constante solicitada no enunciado da questão:


(define JOÃO-DOCUMENTOS (make-diretorio "Documentos" (list
                                                      (make-arquivo "CPF.png" PNG)
                                                      (make-arquivo "RG.png" PNG))))

(define FACULDADE-FUNDAMENTOS (make-diretorio
                               "Fundamentos"
                               (list
                                (make-arquivo "lista1-JoãoMarcosFlach-X.rkt" RKT)
                                (make-arquivo "lista2-JoãoMarcosFlach-X.rkt" RKT)
                                (make-arquivo "lista3-JoãoMarcosFlach-X.rkt" RKT))))

(define FACULDADE-PROGRAMAÇÃO (make-diretorio "Programação" empty))

(define TCC-IMAGENS (make-diretorio "Imagens" (list
                                               (make-arquivo "algoritmo.png" PNG)
                                               (make-arquivo "resultados.png" PNG)
                                               (make-arquivo "testes.png" PNG))))

(define FACULDADE-TCC (make-diretorio "TCC" (list TCC-IMAGENS
                                                  (make-arquivo "tcc.txt" TXT))))

(define JOÃO-FACULDADE (make-diretorio "Faculdade" (list FACULDADE-FUNDAMENTOS
                                                         FACULDADE-PROGRAMAÇÃO
                                                         FACULDADE-TCC)))

(define JOÃO-IMAGENS (make-diretorio "Imagens" (list (make-arquivo "profile.png" PNG))))

(define JOÃO-MÚSICAS (make-diretorio "Músicas" (list
                                                (make-arquivo "unknown-artist-1.mp3" MP3)
                                                (make-arquivo "unknown-artist-2.mp3" MP3)
                                                (make-arquivo "unknown-artist-3.mp3" MP3)
                                                (make-arquivo "unknown-artist-4.mp3" MP3)
                                                (make-arquivo "unknown-artist-5.mp3" MP3))))

(define PASTA-JOÃO (make-diretorio "João" (list JOÃO-DOCUMENTOS
                                                JOÃO-FACULDADE
                                                JOÃO-IMAGENS
                                                JOÃO-MÚSICAS)))

;; =========================================================================
;;                                 QUESTÃO 2
;; =========================================================================

;; arquivo-no-dir? : Conteudo String -> Booleano
;; Objetivo: Dados o conteúdo de um diretório e um nome de arquivo, retorna se existe um
;; arquivo com esse nome no conteúdo específicado. Importante: Esta função não considera
;; subdiretórios ao realizar a consulta.
;; Exemplos:
;;          (arquivo-no-dir? empty "arquivo.txt") retorna false
;;          (arquivo-no-dir? (diretorio-conteudo TCC-IMAGENS) "profile.png") retorna false
;;          (arquivo-no-dir? (diretorio-conteudo JOÃO-FACULDADE) "tcc.txt") retorna false
;;          (arquivo-no-dir? (diretorio-conteudo FACULDADE-TCC) "tcc.txt") retorna true

(define (arquivo-no-dir? cont n)
  ;; Dados um conteúdo cont e um nome de arquivo n
  (cond
    ;; Se cont estiver vazio, retorna false
    [(empty? cont) false]

    ;; Se o primeiro elemento de cont for do tipo arquivo e seu nome
    ;; for igual a n, retorna true
    [(and (arquivo? (first cont))
          (string=? n (arquivo-nome (first cont)))) true]

    ;; Caso contrário, retorna se existe um arquivo de nome n no resto de cont
    [else (arquivo-no-dir? (rest cont) n)]
    ))

;; Testes:
(check-expect (arquivo-no-dir? empty "arquivo.txt") false)
(check-expect (arquivo-no-dir? (diretorio-conteudo TCC-IMAGENS) "profile.png") false)
(check-expect (arquivo-no-dir? (diretorio-conteudo JOÃO-FACULDADE) "tcc.txt") false)
(check-expect (arquivo-no-dir? (diretorio-conteudo JOÃO-MÚSICAS) "algoritmo.png") false)
(check-expect (arquivo-no-dir? (diretorio-conteudo PASTA-JOÃO) "lista1-JoãoMarcosFlach-X.rkt") false)
(check-expect (arquivo-no-dir? (diretorio-conteudo FACULDADE-TCC) "tcc.txt") true)
(check-expect (arquivo-no-dir? (diretorio-conteudo JOÃO-MÚSICAS) "unknown-artist-5.mp3") true)

;; =========================================================================
;;                                 QUESTÃO 3
;; =========================================================================

;; arquivo-encontrado? : Conteudo String -> Booleano
;; Objetivo: Dados o conteúdo de um diretório e um nome de arquivo, retorna se existe um
;; arquivo com esse nome no conteúdo específicado. Importante: Esta função considera 
;; subdiretórios ao realizar a consulta.
;; Exemplos:
;;          (arquivo-encontrado? (diretorio-conteudo PASTA-JOÃO) "lista11.rkt") retorna false
;;          (arquivo-encontrado? (diretorio-conteudo JOÃO-FACULDADE) "tcc.txt") retorna true
;;          (arquivo-encontrado? (diretorio-conteudo FACULDADE-TCC) "tcc.txt") retorna true

(define (arquivo-encontrado? cont n)
  ;; Dados um conteúdo cont e um nome de arquivo n
  (cond

    ;; Se cont estiver vazio, retorna false
    [(empty? cont) false]
    
    ;; Se existir um arquivo com o nome n em cont, retorna true
    [(arquivo-no-dir? cont n) true]

    ;; Se o primeiro elemento de cont for um diretório e existir um arquivo com
    ;; o nome n no conteúdo do primeiro elemento de cont, retorna true
    [(and (diretorio? (first cont))
          (arquivo-no-dir? (diretorio-conteudo (first cont)) n)) true]

    ;; Caso contrário, retorna se existe um arquivo de nome n no resto de cont
    [else (arquivo-encontrado? (rest cont) n)]
    ))

;; Testes:
(check-expect (arquivo-encontrado? (diretorio-conteudo PASTA-JOÃO) "lista11.rkt") false)
(check-expect (arquivo-encontrado? (diretorio-conteudo JOÃO-FACULDADE) "lista2-JoãoMarcosFlach-X.rkt")
              true)
(check-expect (arquivo-encontrado? (diretorio-conteudo FACULDADE-TCC) "tcc.txt") true)
(check-expect (arquivo-encontrado? (diretorio-conteudo FACULDADE-TCC) "algoritmo.png") true)
(check-expect (arquivo-encontrado? (diretorio-conteudo FACULDADE-TCC) "testes.png") true)

;; =========================================================================
;;                                 QUESTÃO 4
;; =========================================================================

;; calcula-tamanho : Diretorio -> Número
;; Objetivo: Dado um diretório, retorna o tamanho necessário em disco para armazená-lo.
;; No cálculo do tamanho é somado 10Kb por diretório mais o tamanho de cada arquivo.
;; Importante: Esta função considera subdiretórios ao realizar o cálculo.
;; Exemplos:
;;          (calcula-tamanho FACULDADE-PROGRAMAÇÃO) retorna 10
;;          (calcula-tamanho FACULDADE-TCC) retorna 370
;;          (calcula-tamanho JOÃO-MÚSICAS) retorna 10010

(define (calcula-tamanho d)
  ;; Dado um diretório d
  ;; Nessa função, cont refere-se ao conteúdo de d
  (cond
    ;; Se d for empty, retorna 0
    ;;[(empty? d) 0]

    ;; Se cont for empty, retorna o tamanho de um diretório
    [(empty? (diretorio-conteudo d)) DIR]

    ;; Se o primeiro elemento de cont for um arquivo, retorna a soma
    [(arquivo? (first (diretorio-conteudo d)))
     (+
      ;; Do tamanho desse arquivo
      (arquivo-tamanho (first (diretorio-conteudo d)))
      ;; Com o tamanho de um diretório com o conteúdo do resto de cont
      (calcula-tamanho (make-diretorio (diretorio-nome d)
                                       (rest (diretorio-conteudo d)))))]

    ;; Se o primeiro elemento de cont for um diretório, retorna a soma
    [(diretorio? (first (diretorio-conteudo d)))
     (+
      ;; Do tamanho desse diretório
      (calcula-tamanho (first (diretorio-conteudo d)))
      ;; Com o tamanho de um diretório com o conteúdo do resto de cont
      (calcula-tamanho (make-diretorio (diretorio-nome d)
                                       (rest (diretorio-conteudo d)))))]
    ))

;; Testes:
(check-expect (calcula-tamanho FACULDADE-PROGRAMAÇÃO) 10)
(check-expect (calcula-tamanho FACULDADE-TCC) 370)
(check-expect (calcula-tamanho JOÃO-MÚSICAS) 10010)
(check-expect (calcula-tamanho PASTA-JOÃO) 10890)

(check-expect (calcula-tamanho FACULDADE-PROGRAMAÇÃO) DIR)
(check-expect (calcula-tamanho FACULDADE-TCC) (+ (* 2 DIR) TXT (* 3 PNG)))
(check-expect (calcula-tamanho JOÃO-MÚSICAS) (+ DIR (* 5 MP3)))
(check-expect (calcula-tamanho PASTA-JOÃO) (+ (* 9 DIR) (* 6 PNG) (* 3 RKT) TXT (* 5 MP3)))


;; =========================================================================
;;                                 QUESTÃO 5
;; =========================================================================

;; mostra-caminho-auxiliar : String Conteudo -> String
;; Objetivo: Dados um nome de arquivo e o conteúdo de um diretório, mostra o caminho até esse arquivo.

(define (mostra-caminho-auxiliar n cont)
  ;; Dados um nome de arquivo n e o conteúdo de um diretório cont
  (cond
    ;; Se o primeiro elemento de cont for um diretório que contém um arquivo de nome n,
    ;; retorna o caminho para n
    [(and (diretorio? (first cont))
          (arquivo-no-dir? (diretorio-conteudo (first cont)) n))
     (string-append (diretorio-nome (first cont))
                    " -> "
                    n)]

    ;; Se o primeiro elemento de cont for um diretório que contém um arquivo de nome n em algum
    ;; dos seus subdiretórios, retorna o começo do caminho o arquivo e mostra o caminho até n
    ;; partindo do primeiro elemento de cont
    [(and (diretorio? (first cont))
          (arquivo-encontrado? (diretorio-conteudo (first cont)) n))
     (string-append (diretorio-nome (first cont))
                    " -> "
                    (mostra-caminho-auxiliar n (diretorio-conteudo (first cont))))]

    ;; Caso contrário, mostra o caminho de n partindo do resto de cont
    [else (mostra-caminho-auxiliar n (rest cont))]
    ))


;; mostra-caminho : String Diretorio -> String
;; Objetivo: Dados um nome de arquivo e um diretório, retorna o caminho até esse arquivo.
;; Caso o arquivo não exista, é mostrada a mensagem "Arquivo não encontrado".
;; Exemplos:
;;          (mostra-caminho "CNPJ.png" PASTA-JOÃO) retorna "Arquivo não encontrado"
;;          (mostra-caminho "lista2-JoãoMarcosFlach-X.rkt" FACULDADE-FUNDAMENTOS) retorna
;;          "Fundamentos -> lista2-JoãoMarcosFlach-X.rkt"
;;          (mostra-caminho "CPF.png" PASTA-JOÃO) retorna "João -> Documentos -> CPF.png"


(define (mostra-caminho n d)
  ;; Dados um nome de arquivo n e um diretório d
  ;; Nessa função, cont refere-se ao conteúdo de d
  (cond
    ;; Se não existir um arquivo de nome n em cont, retorna uma string vazia
    [(not (arquivo-encontrado? (diretorio-conteudo d) n)) "Arquivo não encontrado"]
    
    ;; Se um arquivo de nome n estiver em cont (n está no diretório d), retorna o caminho para n
    [(arquivo-no-dir? (diretorio-conteudo d) n) (string-append (diretorio-nome d) " -> " n)]

    ;; Se um arquivo de nome n existir em algum subdiretório de d, retorna o caminho para n
    ;; através de uma função auxiliar
    [(arquivo-encontrado? (diretorio-conteudo d) n)
     (string-append (diretorio-nome d)
                    " -> "
                    (mostra-caminho-auxiliar (diretorio-conteudo d) n))]
    ))

;; Testes:
(check-expect (mostra-caminho "CNPJ.png" PASTA-JOÃO) "Arquivo não encontrado")
(check-expect (mostra-caminho "lista2-JoãoMarcosFlach-X.rkt" FACULDADE-FUNDAMENTOS)
              "Fundamentos -> lista2-JoãoMarcosFlach-X.rkt")
(check-expect (mostra-caminho "CPF.png" PASTA-JOÃO) "João -> Documentos -> CPF.png")

;; Mais testes...

;;(mostra-caminho "algoritmo.png" PASTA-JOÃO)