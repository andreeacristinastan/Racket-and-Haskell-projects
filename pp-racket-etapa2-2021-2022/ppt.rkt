#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (apply + (map * X Y)))

; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
  (map (λ (x)
         (dot-product x V)) M))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
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

; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
   (foldl (λ(x acc)
            (apply x (list acc))) tuple Fs))

; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.
(define (FT1 x) (((curry multiply) T1) x))
(define (FT2 x) (((curry multiply) T2) x))
(define (FT3 x) (((curry multiply) T3) x))

(define (FQ1 x) (((curry apply) Q1) x))
(define (FQ2 x) (((curry apply) Q2) x))
(define (FQ3 x) (((curry apply) Q3) x))

(define (get-nth-tuple tuple Fs list)
   (foldl (λ(x acc)
            (cond ((= x 1)
                   ((car Fs) acc))
                  ((= x 2)
                   ((cadr Fs) acc))
                  (((caddr Fs) acc)))) tuple list))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define (get-nth-ppt-from-matrix-transformations n)
  (get-nth-tuple '(3 4 5) (list FT1 FT2 FT3) (get-transformations n)))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (get-nth-quadruple n)
  (get-nth-tuple '(1 1 2 3) (list FQ1 FQ2 FQ3) (get-transformations n)))


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-first-element L acc)
  (list (* (car L) (cadddr L))))

(define (get-second-element L acc)
  (list (* 2 (cadr L) (caddr L))))

(define (get-third-element L acc)
  (list (+ (*(cadr L) (cadr L)) (* (caddr L) (caddr L)))))

(define (get-nth-ppt-from-GH-quadruples-helper L)
  (append (get-first-element L '()) (get-second-element L '()) (get-third-element L '())))

(define (get-nth-ppt-from-GH-quadruples n) 
  (get-nth-ppt-from-GH-quadruples-helper (get-nth-quadruple n)))