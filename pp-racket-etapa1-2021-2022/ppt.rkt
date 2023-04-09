#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define result 0)

(define (dot-product X Y)
  (if (or (null? X) (null? Y))
         result
  (+ result (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y))))))


; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.

(define (multiply-tail M V acc)
  (if (null? M)
         acc
        (multiply-tail (cdr M) V (append acc (list (dot-product (car M) V))))))

(define (multiply M V)
  (multiply-tail M V '()))


; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
(define (find-n n min max acc)
  (cond ((= min n)
         (append acc '(1)))
        ((= max n)
         (append acc '(3)))
        ((append acc '(2)))))

(define (get-triplets n max min count acc)
  (cond ((= count 0)
         (find-n n min max acc))
        ((>= (+ min count) n)
         (get-triplets n (+ min count) min
                    (quotient (- (+ min count) min) 3)
                    (append acc '(1))))
        ((>= (+ (+ min 1) (* 2 count)) n)
         (get-triplets n (+ (+ min 1) (* 2 count)) (- (+ (+ min 1) (* 2 count)) count)
                    (quotient (- (+ (+ min 1) (* 2 count)) (- (+ (+ min 1) (* 2 count)) count)) 3)
                    (append acc '(2))))
        ((>= (+ (+ min 2) (* 3 count)) n)
         (get-triplets n (+ (+ min 2) (* 3 count)) (- (+ (+ min 2) (* 3 count)) count)
                    (quotient (- (+ (+ min 2) (* 3 count)) (- (+ (+ min 2) (* 3 count)) count)) 3)
                    (append acc '(3))))))

(define (get-min-max n L min max)
  (if (= (length L) 1)
         '()
         (get-triplets n (+ max (car (reverse L)))
                       (+ min (cadr (reverse L)))
                       (quotient (- (+ max (car (reverse L))) (+ min (cadr (reverse L)))) 3)
                       '())))

(define (find-level n power last L)
  (if (>= last n)
  (get-min-max n (append L (list last)) 1 0)
  (find-level n (+ power 1) (+ last (expt 3 power)) (append L (list last)))))

(define (get-transformations n)
  (find-level n 1 1 '()))


; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.

(define (apply-matrix-transformations-tail Ts acc)
  (cond ((null? Ts)
         acc)
        ((= 1 (car Ts))
         (apply-matrix-transformations-tail (cdr Ts) (multiply T1 acc)))
        ((= 2 (car Ts))
         (apply-matrix-transformations-tail (cdr Ts) (multiply T2 acc)))
        ((= 3 (car Ts))
         (apply-matrix-transformations-tail (cdr Ts) (multiply T3 acc)))))

(define (apply-matrix-transformations Ts ppt)
  (apply-matrix-transformations-tail Ts ppt))


; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))
