(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define args (flatten (command-line)))
;(write (list-ref args 2))
;(newline)

(define filename "Resources/trbgra.narc")
(define outdir "./Out/animtedTrainerBack")

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

(define (save-animation filename size nclr period callback)
  (save-gif filename size nclr 10
            (lambda (frame)
              (let ((tick (floor (* frame 6/10))))
                (if (< tick period)
                    (callback tick)
                    #f)))))

(let* ((n (string->number(list-ref args 1))) ; n = pokemon #
       (base (* n 8))
       (narc (load-narc filename))
       (nclr (narc-load-file narc (+ base 7) 'NCLR)) ; 18 = normal, 19 = shiny
       (ncgr (narc-load-file narc (+ base 1) 'NCGR)) ; 2 = male, 3 = female
       (ncer (narc-load-file narc (+ base 2) 'NCER))
       (nanr (narc-load-file narc (+ base 3) 'NANR))
       (nmcr (narc-load-file narc (+ base 4) 'NMCR))
       (nmar (narc-load-file narc (+ base 5) 'NMAR))
       (cell 1)
       (period (nmar-period nmar cell))
       (size '(192 128)))
  (save-animation (format #f "~a/~a.gif" (string-append outdir (list-ref args 2)) n)
                  size nclr period
                  (lambda (tick)
                    (let ((image (make-image size)))
                      (nmar-draw nmar cell tick
                                 nmcr nanr ncer ncgr
                                 image '(96 128))
                      image))))
