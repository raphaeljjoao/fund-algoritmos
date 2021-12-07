;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lista1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;  João Raphael Fontoura Dorneles
;;;;;;;;;;;; Lista de exercícios 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 1 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Item a
(/ (+ 5 3) 4)

;; Item b
(- (sqrt 9) 1)

;; Item c
(+ (sin (/ pi 2)) 1)

;; Item d
(* (- (sqrt(expt 5 4)) (log e)) (/ 1 12))

;; Item e
(/ (+ -4 (sqrt (- (expt 4 2) (* 4 2 -16)))) (* 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 2 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( define (area-circulo raio)
 (* pi (* raio raio))
)


(define (volume-cilindro r a)
 (* (area-circulo r) a)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 3 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bhaskara a b c)
 (/ (+ (* b -1) (sqrt (-(expt b 2) (* 4 a c)))) (* 2 a))
)

(bhaskara 2 10 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 4 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (TMB p a i f)
  (* (- (+ 66.5 (* 13.75 p) (* 5 a)) (* 6.8 i)) f)
)

(TMB 70 170 18 1.3)
