#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.

(define (get-unstable-couples engagements mpref wpref)
  (let loop ((lista engagements)
             (eng1 engagements)
             (eng2 (let ((a engagements))
                     (map (lambda (x) (cons (cdr x) (car x))) a)))
             (rezultat '()))
    (if (null? lista)
        rezultat
        (if (or (better-match-exists? (cdar lista) (caar lista) (get-pref-list mpref (cdar lista)) wpref eng1)
                (better-match-exists? (caar lista) (cdar lista) (get-pref-list wpref (caar lista)) mpref eng2))
            (loop (cdr lista) eng1 eng2 (cons (car lista) rezultat))
            (loop (cdr lista) eng1 eng2 rezultat))))
  )

; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let loop1 ((men free-men)
              (eng engagements))
    (if (null? men)
        eng
        (let loop2 ((pref-men (get-pref-list mpref (car men))))
          (if (null? pref-men)
              (loop1 (cdr men) eng)
              (let* ((sotie (car pref-men))
                     (sot (car men))
                     (rival (let ((posibil-fost-sot (filter (lambda (x) (equal? (car pref-men) (car x))) eng)))
                              (if (null? posibil-fost-sot)
                                  '()
                                  (cdar posibil-fost-sot)))))
                (if (null? rival)
                    (loop1 (cdr men) (append (list (cons sotie sot)) eng))
                    (if (preferable? (get-pref-list wpref sotie) sot rival)
                        (loop1 (cons rival (cdr men)) (append (remove (cons sotie rival) eng) (list (cons sotie sot))))
                        (loop2 (cdr pref-men)))))))))
   )

;(engage '(abe bob col dan ed fred gav hal ian jon) '() men-preferences-1 women-preferences-1)
; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (let* ((free-men (get-men mpref))
         (engagements '()))
    (engage free-men engagements mpref wpref)))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (let loop ((rez '())
             (lis pair-list))
    (if (null? lis)
        rez
        (loop (cons (caar lis) (cons (cdar lis) rez)) (cdr lis)))))

