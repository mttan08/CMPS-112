#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Matthew Tan
;; mxtan
;; cmps 112
;; asg1: sbi.scm

;; stderr: dumps to standard error
(define *stderr* (current-error-port))

;; run-file: finds file in root and reads it
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath)))

;; die: kills off any program with nonexistent file
;; and displays error message on screen
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

;; usage-exit: displays message to stderr
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

;; read-list-from-inputfile: reads input sbir files
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; function table: holds all the functions
;; and the variables
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key '(no such key in
                                         function-table)))
(define (function-put! key value)
        (hash-set! *function-table* key value))

;; label table: hold addresses of each line
;; and the labels for each program
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key '()))
(define (label-put! key value)
        (hash-set! *label-table* key value ))

;; variable table: holds all the value of
;; the variables
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key '(no such key in 
                                          variable-table)))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

;; length: returns the length of the list
(define len (lambda (l)
    (define length (lambda (l n)
        (if (null? l)n
          (length (cdr l) (+ n 1)))))
        (length l 0)))

;; value: get the value from the list
(define (value-of expr)
  ;(printf "value-of: cond number? expr ~a ~n" expr)
  (if (pair? expr)
      (apply (function-get (car expr)) 
          (map value-of (cdr expr)))
        (cond ((number? expr) expr)
            (else (variable-get expr)))))

;; display-list: for debugging purposes only
;;(define (display-list lst)
;;  (let loop ((lst lst))
;;    (when (pair? lst) 
;;      (printf "display-list: list is: ~a ~n" lst)
;;      (printf "display-list: car of list: ~a ~n" (car lst))
;;      (printf "display-list: cdr of list: ~a ~n" (cdr lst))
;;      (loop (cdr lst))))
;;  (newline))

;; interp-print: deals with print statments
(define (interp-print expr) 
   ;(printf"in interp-print: expr: ~a ~n" expr)
   (if (not (null? expr))
     (begin
          (if (string? (car expr))
            (display (car expr))
            (display (value-of (car expr))))         
            (interp-print (cdr expr)))
            (newline)))

;; interp-input: deals with input statements
(define (interp-input expr)
    ;(printf "interp-input: expr: ~a ~n" (display-list expr))
    (variable-put! 'inputcount 0)
        (define (get-input expr)
            (when (not (null? (car expr)))
                ;(printf "interp-input: case null? 
                ;(car expr) car expr: ~a ~n" (car expr))
            (variable-put! (car expr) (void))
            (let ((data (read)))
            ;(printf "interp-input: data: ~a ~n" data)
                (cond [(eof-object? data)(variable-put! 'inputcount -1)]
                    [(number? data)(variable-put! (car expr) data)
                        (variable-put! 'inputcount 
                            (+ (variable-get 'inputcount) 1))]
                [else (begin (printf "invalid number: ~a~n" data))]))
         (when (not (null? (cdr expr)))
             ;(printf "interp-input: case null? 
             ;(cdr expr) cdr expr: ~n" (cdr expr))
             (get-input (cdr expr)))))
  (get-input expr))

;; interp-dim: deals with dim statements
(define (interp-dim expr)
    ;(printf "interp-dim: expr: ~a ~n" expr)
    (variable-put! (caar expr) (make-vector (value-of (cadar expr))) )
    ;(printf "interp-dim: (caar expr): ~a, (cadar expr): ~a ~n"
    ;    (caar expr) (cadar expr))
    (function-put! (caar expr)
        (lambda(x) (vector-ref (variable-get (caar expr)) (- x 1)))))

;; interp-let: deals with let statments
(define (interp-let expr)
    ;(printf "interp-let: expr: ~a ~n" expr)
    (if (pair? (car expr))
        (begin
            ;(printf "interp-let: got in case if pair? (car expr) ~n")
            (vector-set! (variable-get
                (caar expr)) (- (value-of (cadar expr)) 1) 
                 (value-of (cadr expr))))
    (begin
        ;(printf "interp-let: in case else ~n")
        (let ((result (value-of (cadr expr))))
           (variable-put! (car expr) result)
           ;(printf "interp-let: result: ~a, 
           ;(car expr): ~a ~n" result (car expr))
           ;(printf "interp-let: result: ~a, 
           ;(cadr expr): ~a ~n" result (cadr expr))
        ))))

;; interp-if deals with if statments
(define (interp-if program expr)
    (interp-prog program (- (label-get (caddr expr)) 1)))

;; interp-goto: deals with goto statements
(define (interp-goto program expr)
    (interp-prog program (- (label-get (cadr expr)) 1)))

;; for-each for operator expressions, code to
;; evaluate function calls and arrays from
;; the symbol table
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(
        (+ ,+) 
        (- ,-) 
        (* ,*) 
        (/ ,(lambda (x y)  (/ x (if (equal? y 0) 0.0 y))))
        (% ,(lambda (x y) (- x (* (div x y) y))))
        (^ ,expt)
        (abs ,abs) 
        (ceil ,ceiling) 
        (floor ,floor) 
        (exp ,exp)
        (sqrt ,sqrt) 
        (sin ,sin)  
        (cos ,cos) 
        (tan ,tan)
        (asin ,asin) 
        (acos ,acos) 
        (atan ,atan)
        (round ,round)
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (<=      ,(lambda (x y) (<= x y)))
        (>=      ,(lambda (x y) (>= x y)))
        (<       ,(lambda (x y) (< x y)))
        (>       ,(lambda (x y) (> x y)))
        (=       ,(lambda (x y) (eqv? x y)))
        (<>      ,(lambda (x y) (not (equal? x y))))
        (log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
        (print   ,interp-print)
        (input   ,interp-input)
        (dim     ,interp-dim)
        (let     ,interp-let)
        (if      ,interp-if)
        (goto    ,interp-goto)
    ))

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (inputcount 0)
        (pi 3.141592653589793238462643383279502884197169399)
        (e 2.718281828459045235360287471352662497757247093)))

;; interp-stmt: deals with statements
(define (interp-stmt instr program line-nr)
  ;(printf "interp-stmt: instr is: ~a ~n" instr)
    (if (null? instr)
      (interp-prog program (+ line-nr 1))
      (begin
      (when (not (hash-has-key? *function-table* (car instr )))
          (print (car instr))
          (printf " is not valid ~n")
          (usage-exit))
      (cond
        ((eqv? (car instr) 'goto)
          ;(printf "got in case goto ~n")
          (interp-goto program instr))
        ((eqv? (car instr) 'if)
          (if (equal? #t (value-of (cadr instr)))
            (interp-if program instr)
            (interp-prog program (+ line-nr 1))))  
          (else
            ((function-get (car instr)) (cdr instr))
            (interp-prog program (+ line-nr 1)))))))

;; label: deals with labels
(define (build-label-table list)
    (when (not (null? list))
        (let ((first (caar list)))
        (when (number? first)
            (if (not (null? (cdar list)))
                (if (not (symbol? (cadar list)))
                    (void)
                    (begin
                        (label-put! (cadar list) (caar list))))
                            (void))))
                        ;(display-list list)
    (build-label-table (cdr list))))

;; interp-prog: deals with executed lines
(define (interp-prog program line-nr)
    ;(printf "interp-prog: program is: ~a ~n" program)
    (when (< line-nr (len program))
        ;(printf "interp-prog: len program: ~a ~n" (len program))
        (let ((line (list-ref program line-nr)))
            (cond
                ((= (len line) 3) 
                    ;(printf "interp-prog: case 
                    ;= (len line) 3, line is: ~a ~n"
                    ;    (len line))
                    ;(newline)
                    (set! line (cddr line))
                    (interp-stmt (car line) program line-nr))
                    ((and (= (len line) 2) (list? (cadr line)))
                        ;(printf "interp-prog: case 
                        ;= (len line) 2: len line is: ~a ~n"
                        ;    (len line))
                        ;(printf "interp-prog case 
                        ;= (len line) 2: line is: ~a ~n"
                        ;    line)
                        (set! line (cdr line))
                        ;(printf "interp-prog: line is: ~a ~n" line)
                        (interp-stmt 
                            (car line) program line-nr))           
                    (else
                        ;(printf "interp-prog: case else: 
                        ;len line: ~a ~n" (len line))
                        (interp-prog program (+ line-nr 1)))))))

;; main: calls readlist-from-input-file and then process
;; the sbir program
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               (begin (build-label-table program)
                      (interp-prog program 0)))))

(main (vector->list 
    (current-command-line-arguments))) ;; stores results to list
