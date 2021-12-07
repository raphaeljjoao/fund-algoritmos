;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista2-JoaoRaphaelFontouraDorneles-B) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Nome: João Raphael Fontoura Dorneles

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================
;; calcula-porcentagem: Número Número -> Número
;; obj: dados dois números, que correspondem a um valor e o total, calcula a
;;      porcentagem do valor sobre o total.
;; Exemplos:
;;      (calcula-porcentagem 20 80) = 25 
;;      (calcula-porcentagem 50 100) = 50

;; Código
(define (calcula-porcentagem valor total)
  (* 100
   (/ valor total)
  )
)

;; Testes:
(check-expect (calcula-porcentagem 5 125) 4)
(check-expect (calcula-porcentagem 6 60) 10)
(check-expect (calcula-porcentagem 25 100) 25)

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================
;; vacinados: String Número Número -> String
;; obj: dados o nome de um país, sua população e o número de pessoas
;;      vacinadas, calcula a porcentagem de vacinação e a informa através
;;      de uma mensagem.
;; Exemplos:
;;       (vacinados "Brasil" 211000000 168000000) = "No país
;;        Brasil foram vacinados 80% dos habitantes." 
;;       (vacinados "Estados Unidos da América" 328200000 169000000) =
;;        "No país Estados Unidos da América foram vacinados
;;        80% dos habitantes." 

;; Código

(define (vacinados nome pop vac)
  (string-append "No país "
                 nome
                 " foram vacinados "
                 (number->string (round(calcula-porcentagem vac pop)))
                 ;; um arredondamento foi feito pois a função
                 ;; number->string aceita apenas números inteiros
                 ;; discutido com a professora Ana no meet do dia 19/08
                 "% dos habitantes."
  )
)

;; Testes:
(check-expect (vacinados "de teste" 100 50)
              "No país de teste foram vacinados 50% dos habitantes.")
(check-expect (vacinados "Israel" 9053000 5430000)
              "No país Israel foram vacinados 60% dos habitantes.")
(check-expect (vacinados "Espanha" 46940000 30300000)
              "No país Espanha foram vacinados 65% dos habitantes.")


;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================
;; barra-porcentagens: Número Número String String -> Imagem
;; obj: dadas duas porcentagens P1 e P2 e duas cores C1 e C2, ilustra
;;      essas porcentagens através de um gráfico de barra,com
;;      largura 30 e altura 100 onde P1% da altura deve ser
;;      pintada na cor C1 (parte inferior da barra), P2% na cor C2
;;     (parte do meio) e o resto de cinza.
;; Exemplos:
;; (barra-porcentagens 30 20 "darkgreen" "green") =
;;              (above
;;                    (rectangle 30 50 "solid" "gray")
;;                    (rectangle 30 20 "solid" "green")
;;                    (rectangle 30 30 "solid" "darkgreen")))
;; (barra-porcentagens 70 30 "orange" "yellow") =
;;              (above
;;                     (rectangle 30 0 "solid" "gray")
;;                     (rectangle 30 30 "solid" "yellow")
;;                     (rectangle 30 70 "solid" "orange"))


;; Código

(define (barra-porcentagens p1 p2 c1 c2)
  (above
   (rectangle 30 (- 100 p1 p2) "solid" "gray") ;; parte cinza
   (rectangle 30 p2 "solid" c2) ;; barra central
   (rectangle 30 p1 "solid" c1) ;; barra inferior
  )
)

;; Testes:
(check-expect (barra-porcentagens 75 5 "forestgreen" "green")
              (above
               (rectangle 30 (- 100 75 5) "solid" "gray")
               (rectangle 30 5 "solid" "green")
               (rectangle 30 75 "solid" "forestgreen")
              )
)

(check-expect (barra-porcentagens 40 25 "gold" "yellow")
              (above
               (rectangle 30 (- 100 40 25) "solid" "gray")
               (rectangle 30 25 "solid" "yellow")
               (rectangle 30 40 "solid" "gold")
              )
)
              

;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================
;; exibe-estatisticas: String Número Número Número -> Imagem
;; obj: dados o nome de um país, sua população, o número de pessoas
;;      vacinadas parcialmente e o número de pessoas vacinadas
;;      completamente, ilustra através de um gráfico de barra a porcentagem
;;      de pessoas vacinadas, uma legenda e informações escritas sobre
;;      o país de qual se trata essas informações.

;; Código

"rodou"
(define (exibe-estatisticas nome pop vacparcial vactotal)
  (above
   (text
    (string-append
     "No país "
     nome
     " foram vacinados "
     (number->string (round(calcula-porcentagem vacparcial pop)))
     "% dos habitantes."
    )
    15
    "black"
   )
   (rectangle 1 5 "solid" "white") ;; espaço entre a barra e o texto
   (barra-porcentagens
    (calcula-porcentagem vactotal pop) ;; parte inferior da barra
    (- (calcula-porcentagem vacparcial pop) ;; parte central
     (calcula-porcentagem vactotal pop)
    )
    "gold" ;; cor da parte inferior da barra
    "yellow" ;; cor da parte central da barra
   )
   (rectangle 1 5 "solid" "white") ;; espaço entre a barra e o texto
   (beside
    (rectangle 30 10 "solid" "gold")
    (text " esquema vacinal completo " 13 "black")
    (rectangle 30 10 "solid" "yellow")
    (text " somente primeira dose" 13 "black")
   )
  )
  
)

;; Chamadas:

(exibe-estatisticas "X" 1000 500 250)
(exibe-estatisticas "Y" 1000 700 200)
