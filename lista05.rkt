;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista5-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 1 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Um elemento do conjunto falseOuString é:
;; (i) um booleano false, ou
;; (ii) uma String

;; Um elemento do conjunto NumeroOuString é:
;; (i) um Número, ou
;; (ii) uma String

(define-struct cidadão (nome CPF idade sexo saude? covid? comorb gravidez?))

;; Um elemento do conjunto Cidadão  é uma estrutura
;; (make-cidadão nome CPF idade sexo saude? covid? comorb gravidez?), onde:
;; nome : String, o nome do cidadão;
;; CPF : Número, é CPF do cidadão;
;; idade : Número, é a idade do cidadão;
;; sexo : Booleano, é o sexo do cidadão (feminino=true / masculino=false);
;; saude? : Booleano, indica se o cidadão trabalha na área da saúde;
;; covid? Booleano, indica se o cidadão já foi infectado pelo COVID-19;
;; comorb : falseOuString, indica se o cidadão tem alguma comorbidade e, se sim,
;;          qual;
;; gravidez? : indica se o cidadão está em estado de gravidez.


(define-struct imunizante (nome fabricante quantidade umaDose?))
;; Um elemento conjunto Imunizante é uma estrutura
;; (make-imunizante nome fabricante quantidade umaDose?), onde:
;; nome : String, é o nome do imunizante;
;; fabricante : String, é a fabricante do imunizante;
;; quantidade : Número, é a quantidade do imunizante em estoque;
;; umaDose? : Booleano, indica se o imunizante é de apenas uma dose.

;; Exemplos de instâncias:

;; Homem, 18 anos
(define JOÃO (make-cidadão "João Raphael" 12345678901 18 false
                           false false false false))
;; Mulher, 48 anos
(define ADRIANA (make-cidadão "Adriana Porto" 12345678902 48 true
                              false false false false))
;; Mulher, 77 anos
(define NELI (make-cidadão "Neli Betat" 12345678903 77 true
                              false false false false))
;; Mulher, 33 anos, estado de gravidez
(define ANA (make-cidadão "Ana da Silva" 12345678904 33 true
                              false false false true))
;; Homem, 25 anos, estado de gravidez
(define FRAUDADOR (make-cidadão "José Alves" 12345678905 25 false
                                false false false true))
;; Homem, 27 anos, profissional da saúde, já teve COVID-19
(define MÉDICO (make-cidadão "Gustavo" 12345678906 27 false
                                true true false false))
;; Homem, 20 anos, teve COVID-19, comorbidade
(define COMÓRBIDO (make-cidadão "Ismael" 12345678907 20 false
                                  false true "Diabetes" false))
;; Mulher, 80 anos
(define C80 (make-cidadão "Isolda" 12345678908 80 true
                              false false false false))
;; Homem, 70 anos
(define C70 (make-cidadão "Salomão" 12345678909 70 false
                              false false false false))
;; Mulher, 60 anos
(define C60 (make-cidadão "Salomé" 12345678910 60 true
                              false false false false))
;; Homem, 50 anos
(define C50 (make-cidadão "Pedro" 12345678911 50 false
                              false false false false))
;; Mulher, 40 anos
(define C40 (make-cidadão "Cristina" 12345678912 40 true
                              false false false false))
;; Homem, 18 anos
(define C18 (make-cidadão "Nicolas" 12345678913 18 false
                              false false false false))
;; Mulher, 15 anos
(define C15 (make-cidadão "Caroline" 12345678914 15 true
                              false false false false))
;; Homem, 14 anos
(define C14 (make-cidadão "Benhur" 12345678915 14 false
                              false false false false))

(define U (make-imunizante "U" "fabricante U" 5432 true))
(define A (make-imunizante "A" "fabricante A" 9878 false))
(define B (make-imunizante "B" "fabricante B" 67546 false))

(define U-ACABANDO (make-imunizante "U" "fabricante U" 542 true))
(define A-ACABANDO (make-imunizante "A" "fabricante A" 988 false))
(define B-ACABANDO (make-imunizante "B" "fabricante B" 676 false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 2 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; checa-erro : Cidadão -> NumeroOuString
;; Objetivo: dado um elemento do conjunto Cidadão, verifica se ele é do sexo
;;           masculino e está em estado de gravidez, caso afirmativo, retorna
;;           o CPF desse cidadão indicando que há algum erro, senão, retorna
;;           "CPF OK".
;; Exemplos:
;;          (checa-erro JOÃO) = "CPF OK"
;;          (checa-erro ANA) = "CPF OK"
;;          (checa-erro FRAUDADOR) = 12345678905

;; Corpo da função:
(define (checa-erro c)
  (cond
    ;; se o cidadão for do sexo masculino e estiver em estado de gravidez
    [(and (not (cidadão-sexo c)) (cidadão-gravidez? c))
     (cidadão-CPF c)]
    ;; caso contrário
    [else "CPF OK"]))

;; Testes:
(check-expect (checa-erro ADRIANA) "CPF OK")
(check-expect (checa-erro NELI) "CPF OK")
(check-expect (checa-erro (make-cidadão "Gabriela" 123 18
                                        true true true false true)) "CPF OK")
(check-expect (checa-erro (make-cidadão "Gabriel" 456 19
                                        false true true false true)) 456)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 3 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prioridade : Cidadão -> String
;; Objetivo: dado um elemento do conjunto Cidadão, retorna em que mês ele
;;           poderá se vacinar.
;; Exemplos:
;;          (prioridade C14) = "Aguarde novo calendário"
;;          (prioridade FRAUDADOR) = "Dados deste CPF devem ser verificados!"
;;          (prioridade MÉDICO) = "jan"
;;          (prioridade C80) = "fev"
;;          (prioridade C70) = "mar"
;;          (prioridade C60) = "abr"
;;          (prioridade COMÓRBIDO) = "mai"
;;          (proridade ANA) = "mai"
;;          (prioridade C50) = "jun"
;;          (prioridade C40) = "jul"
;;          (prioridade C18) = "ago"
;;          (prioridade C15) = "set"

(define (prioridade c)
  (cond
    ;; se o cidadão tiver 14 anos ou menos
    [(<= (cidadão-idade c) 14) "Aguarde novo calendário"]
    
    ;; se houver algum erro ou fraude no cadastro do cidadão
    [(number? (checa-erro c)) "Dados deste CPF devem ser verificados!"]
    
    ;; se o cidadão for um profissional da saúde
    [(cidadão-saude? c) "jan"]
    
    ;; se o cidadão tiver 80 anos ou mais
    [(>= (cidadão-idade c) 80) "fev"]
    
    ;; se o cidadão tiver 70 anos ou mais
    [(>= (cidadão-idade c) 70) "mar"]

    ;; se o cidadão tiver 60 anos ou mais
    [(>= (cidadão-idade c) 60) "abr"]

    ;; se o cidadão tiver comorbidade ou estiver em estado de gravidez
    [(or (string? (cidadão-comorb c)) (cidadão-gravidez? c)) "mai"]

    ;; se o cidadão tiver 50 anos ou mais
    [(>= (cidadão-idade c) 50) "jun"]

    ;; se o cidadão tiver 40 anos ou mais
    [(>= (cidadão-idade c) 40) "jul"]

    ;; se o cidadão tiver 18 anos ou mais
    [(>= (cidadão-idade c) 18) "ago"]

    ;; se o cidadão tiver 15 anos ou mais
    [(>= (cidadão-idade c) 15) "set"]
    )
  )

; Testes:
(check-expect (prioridade JOÃO) "ago")
(check-expect (prioridade ADRIANA) "jul")
(check-expect (prioridade NELI) "mar")
(check-expect (prioridade C14) "Aguarde novo calendário")
(check-expect (prioridade FRAUDADOR) "Dados deste CPF devem ser verificados!")
(check-expect (prioridade MÉDICO) "jan")
(check-expect (prioridade C80) "fev")
(check-expect (prioridade C70) "mar")
(check-expect (prioridade C60) "abr")
(check-expect (prioridade COMÓRBIDO) "mai")
(check-expect (prioridade ANA) "mai")
(check-expect (prioridade C50) "jun")
(check-expect (prioridade C40) "jul")
(check-expect (prioridade C18) "ago")
(check-expect (prioridade C15) "set")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 4 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; comorbidade? : Cidadão -> Booleano
;; Objetivo: dado um elemento do conjunto Cidadão, informa se ele tem comorbidade.
;; Exemplos:
;;          (comorbidade? JOÃO) = false
;;          (comorbidade? COMÓRBIDO) = true
(define (comorbidade? c) (string? (cidadão-comorb c)))

;; Testes:
(check-expect (comorbidade? JOÃO) false)
(check-expect (comorbidade? COMÓRBIDO) true)

;; gravidez? : Cidadão -> Booleano
;; Objetivo: dado um elemento do conjunto Cidadão, informa se ele está
;;           em estado de gravidez.
;; Exemplos:
;;          (gravidez? JOÃO) = false
;;          (gravidez? COMÓRBIDO) = false
;;          (gravidez? ANA) = true
(define (gravidez? c) (cidadão-gravidez? c))

;; Testes:
(check-expect (gravidez? JOÃO) false)
(check-expect (gravidez? COMÓRBIDO) false)
(check-expect (gravidez? ANA) true)

;; requisito-idade? : Cidadão -> Booleano
;; Objetivo: dado um elemento do conjunto Cidadão, informa se ele está dentro
;;           do intervalo [15, 18), que também pode ser lido como "jovens
;;           abaixo dos 18 (mas acima de 15)".
(define (requisito-idade? c) (<= 15 (cidadão-idade c) 17))

;; Testes:
(check-expect (requisito-idade? JOÃO) false)
(check-expect (requisito-idade? C14) false)
(check-expect (requisito-idade? C15) true)
 
;; imunizante-valido? : Cidadão Imunizante -> Booleano
;; Objetivo: dado um elemento do conjunto Cidadão e um elemento do
;;           conjunto Imunizante, informa se o cidadão pode receber o
;;           esse imunizante.
;; Exemplos:
;;          (imunizante-valido? COMÓRBIDO U) = true
;;          (imunizante-valido? COMÓRBIDO A) = false
;;          (imunizante-valido? COMÓRBIDO U-ACABANDO) = false
;;          (imunizante-valido? ANA A) = true
;;          (imunizante-valido? ANA U) = false
;;          (imunizante-valido? C15 B) = true
;;          (imunizante-valido? C15 U) = false


(define (imunizante-valido? c i)
  (cond

    ;; cidadão >= 15
    ;; cidadão sem erro
    ;; estoque acima de 1000

    ;; se o cidadão tiver 15 anos ou mais, não apresentar erros no cadastro e o
    ;; estoque do imunizante for maior que 1000
    [(and (>= (cidadão-idade c) 15)
          (string? (checa-erro c))
          (> (imunizante-quantidade i) 1000))
     (cond
       ;; se o cidadão tiver comorbidade e for aplicada o imunizante U
       [(and (comorbidade? c)
             (string=? "U" (imunizante-nome i))) true]

       ;; se o cidadão estiver em estado de gravidez e for aplicada o imunizante A
       [(and (gravidez? c)
             (string=? "A" (imunizante-nome i))) true]

       ;; se o cidadão for um jovem abaixo dos 18 anos (mas acima de 15) e for
       ;; aplicada o imunizante B
       [(and (requisito-idade? c)
             (string=? "B" (imunizante-nome i))) true]

       ;; se o cidadão não tiver comorbidade, não estiver em estado de gravidez
       ;; e nem for um jovem abaixo dos 18 anos (mas acima de 15), pode ser
       ;; aplicado qualquer imunizante
       [(and (not (gravidez? c))
             (not (comorbidade? c))
             (not (requisito-idade? c))) true]
       
       ;; caso contrário
       [else false]
       )]
    ;; caso contrário
    [else false]
    )
  )

;; Testes:
(check-expect (imunizante-valido? COMÓRBIDO U) true)
(check-expect (imunizante-valido? ANA A) true)
(check-expect (imunizante-valido? C15 B) true)
(check-expect (imunizante-valido? JOÃO U) true)
(check-expect (imunizante-valido? JOÃO A) true)
(check-expect (imunizante-valido? JOÃO B) true)
(check-expect (imunizante-valido? ADRIANA U) true)
(check-expect (imunizante-valido? ADRIANA A) true)
(check-expect (imunizante-valido? ADRIANA B) true)
(check-expect (imunizante-valido? COMÓRBIDO U-ACABANDO) false)
(check-expect (imunizante-valido? COMÓRBIDO A) false)
(check-expect (imunizante-valido? COMÓRBIDO B) false)
(check-expect (imunizante-valido? ANA A-ACABANDO) false)
(check-expect (imunizante-valido? ANA U) false)
(check-expect (imunizante-valido? ANA B) false)
(check-expect (imunizante-valido? C15 B-ACABANDO) false)
(check-expect (imunizante-valido? C15 U) false)
(check-expect (imunizante-valido? C15 A) false)
(check-expect (imunizante-valido? C14 U) false)
(check-expect (imunizante-valido? C14 A) false)
(check-expect (imunizante-valido? C14 B) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 5 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; 5a
(define-struct entrada (nome 14d 30d 60d 90d covid_sim covid_nao etaria1 etaria2))
;; Um elemento do conjunto Entrada é uma estrutura
;; (make-entrada nome 14d 30d 60d 90d covid_sim covid_nao etaria1 etaria2), onde:
;; nome : String, é o nome do imunizante;
;; 14d : NumeroOuString, é a eficácia do imunizante após 14 dias da sua aplicação;
;; 30d : NumeroOuString, é a eficácia do imunizante após 30 dias da sua aplicação;
;; 60d : NumeroOuString, é a eficácia do imunizante após 60 dias da sua aplicação;
;; 90d : NumeroOuString, é a eficácia do imunizante após 90 dias da sua aplicação;
;; covid_sim : Número, é a eficácia do imunizante para quem teve COVID-19;
;; covid_nao : Número, é a eficácia do imunizante para quem não teve COVID-19;
;; etaria1 : Número, é a eficácia do imunizante para a primeira faixa
;;           etária (de 18 a 34 anos);
;; etaria2 : Número, é a eficácia do imunizante para a segunda faixa
;;           etária (de 35 a 54 anos).

; Instâncias de Entrada:
(define linhaA (make-entrada "A" 94 90 88 78 93 85 90 77))
(define linhaB (make-entrada "B" 69  "--" "--" 61 88 68 73 54))

;;;;;;;; 5b

(define-struct resultado (dias infeccao etaria))

;; Um elemento do conjunto Resultado é uma estrutura
;; (make-resultado dias infeccao eteria), onde:
;; dias : NumeroOuString, valor de eficácia de um imunizante após 14, 30, 60 ou 90
;;        dias desde sua aplicação. É um número quando o valor for informado
;;        na tabela (colunas 2 a 5) e uma string quando esse dado está ausente;
;; infeccao : Número, valor de eficácia de um imunizante considerando a infecção
;;            prévia (colunas 6 ou 7 da tabela) por COVID-19 do imunizado;
;; etaria : Número, valor de eficácia de um imunizante considerando a faixa
;;          etária (colunas 8 ou 9 da tabela) do imunizado.

;;;;;;;;;; 5c

;; 3eficacias : Cidadão Número Entrada -> Resultado
;; Objetivo: dado um elemento do conjunto Cidadão, o número de dias decorridos
;;           após a aplicação do imunizante e um elemento do conjunto Entrada,
;;           retorna um elemento do conjunto Resultado com três eficácias:
;;           de dias decorridos, de infecção prévia e de faixa etária.
;; Exemplos:
;;          (3eficacias JOÃO 14 A) = (make-resultado 94 85 90)
;;          (3eficacias JOÃO 30 A) = (make-resultado 90 85 90)
;;          (3eficacias JOÃO 60 A) = (make-resultado 88 85 90)
;;          (3eficacias JOÃO 90 A) = (make-resultado 78 85 90)
;;          (3eficacias JOÃO 14 B) = (make-resultado 69 68 73)
;;          (3eficacias JOÃO 30 B) = (make-resultado "--" 68 73)
;;          (3eficacias JOÃO 60 B) = (make-resultado "--" 68 73)
;;          (3eficacias JOÃO 90 B) = (make-resultado 61 68 73)
;;          (3eficacias MÉDICO 14 A) = (make-resultado 94 93 90)
;;          (3eficacias MÉDICO 14 B) = (make-resultado 69 88 73)
;;          (3eficacias C40 30 A) = (make-resultado 90 85 77)
;;          (3eficacias C40 30 B) = (make-resultado "--" 68 54)

(define (3eficacias c dias e)
  (make-resultado
   ;; eficácia por dias decorridos
   (cond
     ;; 14 dias
     [(= dias 14)
      (entrada-14d e)]

     ;; 30 dias
     [(= dias 30)
      (entrada-30d e)]

     ;; 60 dias
     [(= dias 60)
      (entrada-60d e)]

     ;; 90 dias
     [(= dias 90)
      (entrada-90d e)]
     )

   ;; eficácia por infecção prévia
   (cond
     ;; teve COVID-19
     [(cidadão-covid? c)
      (entrada-covid_sim e)]

     ;; nunca teve COVID-19
     [(not (cidadão-covid? c))
      (entrada-covid_nao e)]
     )

   ;; eficácia por faixa etária
   (cond
     ;; de 18 a 34 anos
     [(<= 18 (cidadão-idade c) 34)
      (entrada-etaria1 e)]

     ;; de 35 a 54 anos
     [(<= 35 (cidadão-idade c) 54)
      (entrada-etaria2 e)]
    )
   )
  )

;; Testes:
(check-expect (3eficacias JOÃO 14 linhaA) (make-resultado 94 85 90))
(check-expect (3eficacias JOÃO 14 linhaA) (make-resultado 94 85 90))
(check-expect (3eficacias JOÃO 30 linhaA) (make-resultado 90 85 90))
(check-expect (3eficacias JOÃO 60 linhaA) (make-resultado 88 85 90))
(check-expect (3eficacias JOÃO 90 linhaA) (make-resultado 78 85 90))
(check-expect (3eficacias JOÃO 14 linhaB) (make-resultado 69 68 73))
(check-expect (3eficacias JOÃO 30 linhaB) (make-resultado "--" 68 73))
(check-expect (3eficacias JOÃO 60 linhaB) (make-resultado "--" 68 73))
(check-expect (3eficacias JOÃO 90 linhaB) (make-resultado 61 68 73))

(check-expect (3eficacias MÉDICO 14 linhaA) (make-resultado 94 93 90))
(check-expect (3eficacias MÉDICO 14 linhaB) (make-resultado 69 88 73))

(check-expect (3eficacias ADRIANA 14 linhaA) (make-resultado 94 85 77))
(check-expect (3eficacias ADRIANA 30 linhaA) (make-resultado 90 85 77))
(check-expect (3eficacias ADRIANA 60 linhaA) (make-resultado 88 85 77))
(check-expect (3eficacias ADRIANA 90 linhaA) (make-resultado 78 85 77))
(check-expect (3eficacias ADRIANA 14 linhaB) (make-resultado 69 68 54))
(check-expect (3eficacias ADRIANA 30 linhaB) (make-resultado "--" 68 54))
(check-expect (3eficacias ADRIANA 60 linhaB) (make-resultado "--" 68 54))
(check-expect (3eficacias ADRIANA 90 linhaB) (make-resultado 61 68 54))

(check-expect (3eficacias ANA 14 linhaA) (make-resultado 94 85 90))
(check-expect (3eficacias ANA 30 linhaA) (make-resultado 90 85 90))
(check-expect (3eficacias ANA 60 linhaA) (make-resultado 88 85 90))
(check-expect (3eficacias ANA 90 linhaA) (make-resultado 78 85 90))
(check-expect (3eficacias ANA 14 linhaB) (make-resultado 69 68 73))
(check-expect (3eficacias ANA 30 linhaB) (make-resultado "--" 68 73))
(check-expect (3eficacias ANA 60 linhaB) (make-resultado "--" 68 73))
(check-expect (3eficacias ANA 90 linhaB) (make-resultado 61 68 73))

(check-expect (3eficacias COMÓRBIDO 14 linhaA) (make-resultado 94 93 90))
(check-expect (3eficacias COMÓRBIDO 30 linhaA) (make-resultado 90 93 90))
(check-expect (3eficacias COMÓRBIDO 60 linhaA) (make-resultado 88 93 90))
(check-expect (3eficacias COMÓRBIDO 90 linhaA) (make-resultado 78 93 90))
(check-expect (3eficacias COMÓRBIDO 14 linhaB) (make-resultado 69 88 73))
(check-expect (3eficacias COMÓRBIDO 30 linhaB) (make-resultado "--" 88 73))
(check-expect (3eficacias COMÓRBIDO 60 linhaB) (make-resultado "--" 88 73))
(check-expect (3eficacias COMÓRBIDO 90 linhaB) (make-resultado 61 88 73))

(check-expect (3eficacias C50 14 linhaA) (make-resultado 94 85 77))
(check-expect (3eficacias C50 30 linhaA) (make-resultado 90 85 77))
(check-expect (3eficacias C50 60 linhaA) (make-resultado 88 85 77))
(check-expect (3eficacias C50 90 linhaA) (make-resultado 78 85 77))
(check-expect (3eficacias C50 14 linhaB) (make-resultado 69 68 54))
(check-expect (3eficacias C50 30 linhaB) (make-resultado "--" 68 54))
(check-expect (3eficacias C50 60 linhaB) (make-resultado "--" 68 54))
(check-expect (3eficacias C50 90 linhaB) (make-resultado 61 68 54))

(check-expect (3eficacias C40 14 linhaA) (make-resultado 94 85 77))
(check-expect (3eficacias C40 30 linhaA) (make-resultado 90 85 77))
(check-expect (3eficacias C40 60 linhaA) (make-resultado 88 85 77))
(check-expect (3eficacias C40 90 linhaA) (make-resultado 78 85 77))
(check-expect (3eficacias C40 14 linhaB) (make-resultado 69 68 54))
(check-expect (3eficacias C40 30 linhaB) (make-resultado "--" 68 54))
(check-expect (3eficacias C40 60 linhaB) (make-resultado "--" 68 54))
(check-expect (3eficacias C40 90 linhaB) (make-resultado 61 68 54))

(check-expect (3eficacias C18 14 linhaA) (make-resultado 94 85 90))
(check-expect (3eficacias C18 30 linhaA) (make-resultado 90 85 90))
(check-expect (3eficacias C18 60 linhaA) (make-resultado 88 85 90))
(check-expect (3eficacias C18 90 linhaA) (make-resultado 78 85 90))
(check-expect (3eficacias C18 14 linhaB) (make-resultado 69 68 73))
(check-expect (3eficacias C18 30 linhaB) (make-resultado "--" 68 73))
(check-expect (3eficacias C18 60 linhaB) (make-resultado "--" 68 73))
(check-expect (3eficacias C18 90 linhaB) (make-resultado 61 68 73))