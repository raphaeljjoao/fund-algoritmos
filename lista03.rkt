;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista3-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; Lista 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 1 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pi: Número -> Número
;;; obj: dado um R0 (número básico de reprodução em epidemiologia), retorna
;        a proporção da população que precisa ser imunizada para se ter
;         imunidade coletiva.
;; exemplos:
; (Pi 2) = 0.5
; (Pi 9) = 0.88...

;; Corpo da função:

(define (Pi taxa-infeccao)
  (- 1 (/ 1 taxa-infeccao))
  )


;; Testes:
(check-expect (Pi 2) 0.5)
(check-expect (Pi 4) 0.75)
(check-expect (Pi 8) 0.875)

;;; arredondaP1Casa: Número -> Número
;;; obj: dado um número, arrendonda ela e o deixa com apenas 1 casa decimal.
;; exemplos:
; (arredondaP1Casa 76.4956) = 76.5
; (arredondaP1Casa 43.2705) = 43.3

;; Corpo da função:

(define (arredondaP1Casa numero)
  (/(round(* numero 10))10)
  )

;; Testes:

(check-expect  (arredondaP1Casa 56.35646) 56.4)
(check-expect (arredondaP1Casa 335.3896) 335.4)

;;; imunidade-coletiva: String -> Número
;;; obj: dado o nome abreviado de um vírus ou uma variante de vírus, retorna
;        a
;; exemplos:
; (imunidade-coletiva "orig") = 50
; (imunidade-coletiva "sarampo") = 92.9

;; Corpo da função:

(define (imunidade-coletiva virus)
  (cond
    ;; caso o vírus informado seja o orig, retorna a porcentagem mínima
    ;; que precisa estar imunizada, recebendo essa informação da função Pi
    ;; em proporção, a multiplicando por 100 para mostrá-la em porcentagem
    ;; e arrendondando para uma casa decimal com a função arredondaP1Casa
    [(string=? virus "orig") (arredondaP1Casa(*(Pi 2) 100))]
    ;; faz o mesmo processo da primeira condição, mas caso o vírus informado
    ;; seja o alfa
    [(string=? virus "alfa") (arredondaP1Casa(*(Pi 3) 100))]
    ;; faz o mesmo processo da primeira condição, mas caso o vírus informado
    ;; seja o beta
    [(string=? virus "beta") (arredondaP1Casa(*(Pi 4.5) 100))]
    ;; faz o mesmo processo da primeira condição, mas caso o vírus informado
    ;; seja o gama
    [(string=? virus "gama") (arredondaP1Casa(*(Pi 4.7) 100))]
    ;; faz o mesmo processo da primeira condição, mas caso o vírus informado
    ;; seja o delta
    [(string=? virus "delta") (arredondaP1Casa(*(Pi 6) 100))]
    ;; faz o mesmo processo da primeira condição, mas caso o vírus informado
    ;; seja o sarampo
    [(string=? virus "sarampo") (arredondaP1Casa(*(Pi 14) 100))]
    )
  )

;; Testes:

(check-expect (imunidade-coletiva "orig") 50)
(check-expect (imunidade-coletiva "delta") 83.3)
(check-expect (imunidade-coletiva "sarampo") 92.9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 2 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; eficacia: String Número -> Número
;; obj: dado o nome de uma vacina e o número de dias decorridos após a
;;      segunda dose, informa a eficácia dessa vacina.
;; exemplos:
;;    (eficacia "A" 90) = 78
;;    (eficacia "B" 14) = 69

;; Corpo da função:

(define (eficacia nome dias)
  (cond
    [(string=? nome "A")
     (cond
       [(= dias 14) 94]
       [(= dias 30) 90]
       [(= dias 60) 88]
       [(= dias 90) 78]
       )
     ]
    [(string=? nome "B")
     (cond
       [(= dias 14) 69]
       [(= dias 90) 61]
       )
     ]
    )
  )


;; Testes:

(check-expect (eficacia "A" 30) 90)
(check-expect (eficacia "A" 60) 88)
(check-expect (eficacia "B" 90) 61)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 3 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re: Número Número Número -> Número
;; obj: dado três números, sendo eles o R0, a proporção de pessoas que estão
;;      vacinadas e a eficácia da vacina, calcula o número de reprodução
;;      efetiva de um vírus.
;; Exemplos:
;;    (Re 6 1 0.85) = 0.9
;;    (Re 6 0.5 0.95) = 3.15

;; Corpo da função:

(define (Re taxa-infeccao vacinados eficacia)
  (* taxa-infeccao (- 1 (* vacinados eficacia)))
  )

;; Testes:

(check-expect (Re 2 1 0.95) 0.1)
(check-expect (Re 6 0.65 0.85) 2.685)

;; Instruções
(Re 2 1 0.85)
(Re 2 0.5 0.95)
(Re 6 1 0.85)
(Re 6 0.5 0.95)

"Em qualquer taxa de transmissão R0, é mais vantajoso vacinar mais pessoas com uma vacina menos eficaz, do que apenas metade das pessoas com uma vacina mais eficaz."

;; resposta: Número Número Número ->
;; obj: dado três números, sendo eles a proporção de pessoas que estão
;;      vacinadas, o R0 e a eficácia da vacina, calcula o Pi caso menos de
;;      50% (inclusive) da população tenha sido vacinada, caso contrário,
;;      calcula o Re.
;; Exemplos:
;;    (resposta 0.5 4 0.75) = (Pi) 0.75
;;    (resposta 0.6 4 0.75) = (Re) 2.2
;;    (resposta 0.4 8 0.85) = (Pi) 0.875
;;    (resposta 0.8 8 0.95) = (Re) 1.92

(define (resposta vacinados taxa-infeccao eficacia)
  (cond
    [(<= vacinados 0.5) (Pi taxa-infeccao)]
    [else (Re taxa-infeccao vacinados eficacia)]
    )
  )

;; Testes:

(check-expect (resposta 0.4 10 0.8) 0.9)
(check-expect (resposta 0.87 8 0.95) 1.388)
(check-expect (resposta 1 12 0.75) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 4 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calcula-TMB: Número Número Número Número Booleano -> Número
;; obj: dado o peso, a altura, a idade, o fator de atividade física e o sexo
;;      de uma pessoa, calcula sua taxa metabólica basal. Informações:
;;      (1) Valor verdadeiro = mulher
;;      (2) Valor falso = homem
;; Exemplos:
;;      (calcula-TMB 71 175 18 1.25 false) = 2244.1875
;;      (calcula-TMB 55 160 32 1.4) = 1848.84

;; Corpo da função:

(define (calcula-TMB peso altura idade atividade mulher?)
  (cond
    ;; caso seja mulher, calcula de acordo com a fórmula de Harris Benedict
    ;; para mulheres
    [mulher?
     (* (- (+ 655 (* 9.6 peso) (* 1.8 altura))(* 4.7 idade)) atividade)
     ]
    ;; caso seja homem, calcula de acordo com a fórmula de Harris Benedict
    ;; para homens
    [else
     (* (- (+ 66.5 (* 13.75 peso) (* 5 altura))(* 6.8 idade)) atividade)
     ]
    )
  )

;; Testes:

(check-expect (calcula-TMB 70 170 18 1.35 false) 2371.41)
(check-expect (calcula-TMB 89 192 22 1.25 false) 2625.8125)
(check-expect (calcula-TMB 77 175 25 1.3 true) 2069.21)
(check-expect (calcula-TMB 80 150 45 1.2 true) 1777.8)