#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur

(define (match person engagements pref1 pref2 queue)
  (let loop ((p person)
             (eng engagements))
    (if (null? p)
        eng
        (let loop2 ((pref (get-pref-list pref1 p)))
          (if (null? pref)
              (loop '() (append eng (list (cons #f p))))
              (if (equal? #f (member (car pref) queue))
                  (let* ((part (car pref))
                         (verif (filter (lambda (x) (equal? (car x) part)) eng)))
                    (if (null? verif)
                        (loop2 (cdr pref))
                        (let* ((cuplu (car (filter (lambda (x) (equal? (car x) part)) eng)))
                               (rival (cdr cuplu)))
                          (if (equal? #f rival)
                              (loop '() (append (list (cons part p)) (remove (cons part rival) eng)))
                              (if (preferable? (get-pref-list pref2 part) p rival)
                                  (loop rival (append (list (cons part p)) (remove (cons part rival) eng)))
                                  (loop2 (cdr pref)))))))
                  (loop2 (cdr pref))))))))

; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)

(define (path-to-stability engagements mpref wpref queue)
 (let loop ((eng engagements)
            (q queue))
   (if (null? q)
       eng
       (if (member (car q) (get-men mpref))
           (loop (match (car q) eng mpref wpref (cdr q)) (cdr q))
           (let rev ((ok 1)
                     (ENG eng))
             (let ((ENG2 (let rev-eng ((rev-eng (let ((a ENG))
                                                  (map (lambda (x) (cons (cdr x) (car x))) a))))
                           rev-eng)))
               (if (equal? ok 1)
                   (rev 2 (match (car q) ENG2 wpref mpref (cdr q)))
                   (loop ENG2 (cdr q)))))))))

; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie

(define (update-stable-match engagements mpref wpref)
  (let* ((instabile (get-unstable-couples engagements mpref wpref))
         (eng (let loop ((ENG engagements)
                         (inst instabile))
                (if (null? inst)
                    ENG
                    (loop (remove (car inst) ENG) (cdr inst)))))
         (queue (foldr (lambda (x acc) (append (list (car x)) (list (cdr x)) acc)) '() instabile)))
    (path-to-stability eng mpref wpref queue)))

; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
(define (build-stable-matches-stream pref-stream)
  (let loop ((f pref-stream)
                   (ok 1)
                   (eng '()))
    (if (stream-empty? f)
        empty-stream
        (let* ((elm (stream-first f))
               (mpref (stream-first elm))
               (wpref (stream-rest elm)))
          (if (equal? ok 1)
              (let ((gs-eng (gale-shapley mpref wpref)))
                (stream-cons gs-eng (loop (stream-rest f) 2 gs-eng)))
              (let ((usm-eng (update-stable-match eng mpref wpref)))
                (stream-cons usm-eng (loop (stream-rest f) 1 usm-eng))))))))
