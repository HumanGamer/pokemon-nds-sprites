(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define args (flatten (command-line)))
;(write (list-ref args 2))
;(newline)

(define filename "Resources/Narcs/pokegra-w.narc")
(define outdir "./Out/animtedSprites")

(define ENOENT 2)
(define EEXIST 17)

(define (mkdir-if-not-exist dir)
  (catch 'system-error
         (lambda () (mkdir dir) #t)
         (lambda (key subr msg args rest)
           (define code (car rest))
           (if (eq? code EEXIST)
               #f
               (throw key subr msg args rest)))))

(mkdir-if-not-exist outdir)
(mkdir-if-not-exist (string-append outdir "/female"))
(mkdir-if-not-exist (string-append outdir "/shiny"))
(mkdir-if-not-exist (string-append outdir "/shiny/female"))
(mkdir-if-not-exist (string-append outdir "/back"))
(mkdir-if-not-exist (string-append outdir "/back/female"))
(mkdir-if-not-exist (string-append outdir "/back/shiny"))
(mkdir-if-not-exist (string-append outdir "/back/shiny/female"))

(define (save-animation filename size nclr period callback)
  (save-gif filename size nclr 10
            (lambda (frame)
              (let ((tick (floor (* frame 6/10))))
                (if (< tick period)
                    (callback tick)
                    #f)))))

(let* ((n (string->number(list-ref args 1))) ; n = pokemon #
       (base (* n 20))
       (narc (load-narc filename))
       (nclr (narc-load-file narc (+ base (string->number(list-ref args 2))) 'NCLR)) ; 18 = normal, 19 = shiny
       (ncgr (narc-load-file narc (+ base (+ (string->number(list-ref args 3)) (string->number(list-ref args 4)))) 'NCGR)) ; 2 = male, 3 = female
       (ncer (narc-load-file narc (+ base (+ 4 (string->number(list-ref args 4)))) 'NCER))
       (nanr (narc-load-file narc (+ base (+ 5 (string->number(list-ref args 4)))) 'NANR))
       (nmcr (narc-load-file narc (+ base (+ 6 (string->number(list-ref args 4)))) 'NMCR))
       (nmar (narc-load-file narc (+ base (+ 7 (string->number(list-ref args 4)))) 'NMAR))
       (cell 0)
       (period (nmar-period nmar cell))
       (size '(192 128)))
  (save-animation (format #f "~a/~a.gif" (string-append outdir (list-ref args 5)) n)
                  size nclr period
                  (lambda (tick)
                    (let ((image (make-image size)))
                      (nmar-draw nmar cell tick
                                 nmcr nanr ncer ncgr
                                 image '(96 112))
                      image))))
