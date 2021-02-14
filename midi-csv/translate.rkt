#lang racket

(require csv-reading csv-writing)
(provide encode-csv decode-csv)


(define EXAMPLE "very_complex/very_complex.csv")

;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions and constants

;; number stuff
(define ((gteq? i) x) (<= i x))
(define DEF-TRASH 1)
(define DEF-CHAN 0)
(define DEF-VEL 60)

;; set stuff
(define (set-cons x ls) (if (member x ls) ls (cons x ls)))
(define (to-set ls) (foldr set-cons '() ls))
(define (set-difference s1 s2) (foldr (λ (x a) (if (member x s2) a (cons x a))) '() s1))
(define (set-union s1 s2) (foldr set-cons s1 s2))


;;;;;;;;;;;;;;;;;;;;;;
;; formatting

(define (on/off-instr? x) (member x '("Note_on_c" "Note_off_c")))

(define (make-header max)
  '(("0" "0" "Header" "0" "1" "96")
    ("1" "0" "Start_track")
    ("1" "0" "Title_t" "1-MIDI 1\\000")
    ("1" "0" "Time_signature" "4" "2" "36" "8")
    ("1" "0" "Time_signature" "4" "2" "36" "8")))

(define (make-footer max)
  `((1 ,max "End_track")
    (1 0 "End_of_file")))

(define (format-instruction i)
  (match i
    [`(,_ ,time ,(? on/off-instr? s) ,chan ,pitch ,vel)
     (list (string->number time) s (string->number pitch))]
    [else #f]))

(define (format-MIDI i)
  (match i
    [`(,time ,(? on/off-instr? s) ,pitch)
     (list DEF-TRASH (number->string time) s DEF-CHAN (number->string pitch) DEF-VEL)]
    [else i]))

(define ((make-note-on time) n) `(,time "Note_on_c" ,n))
(define ((make-note-off time) n) `(,time "Note_off_c" ,n))

;;;;;;;;;;;;;;;;;;;;;;;
;; template reading/writing functions
(define reader
    (make-csv-reader-maker
     '((separator-chars            #\,)
       (strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t))))

(define (read-encoding filename)
  (let ((next-row (reader (open-input-file filename))))
    (csv-map (λ (x) (cons (string->number (car x)) (cdr x))) next-row)))

(define (read-MIDI filename)
  (let ((next-row (reader (open-input-file filename))))
    (filter (λ (x) x) (csv-map format-instruction next-row))))

(define ((write-file e) fname)
  (let ((p (open-output-file fname #:exists 'replace)))
    (display-table e p)
    (close-output-port p)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Translators

(define (notes-are-on letters pitches) (sort (to-set (append letters pitches)) <))
(define (notes-are-off letters pitch) (remove pitch letters))

(define ((csv->words-go f) p time counts)
  (map
   (λ (x) (if (= (car x) time) (cons (car x) (f (cdr x) p)) x))
   counts))

(define add-to-count (csv->words-go notes-are-on))
(define remove-from-count (csv->words-go notes-are-off))

(define (concatenate-words ln)
  (list (car ln) (list->string (map integer->char (cdr ln)))))

;; translates Racket-data CSV into text version
(define (csv->words csv)
  (let ((MAX (foldr max 0 (map first csv))))
    (let loop ((track csv)
               (on '())
               (i 0)
               (counts (build-list (add1 MAX) (λ (x) `(,x)))))
    (match track
      ['() (map concatenate-words counts)]
      [`((,(? (gteq? i) t) ,s ,p) . ,d)
       (loop track on (add1 i) (add-to-count on i counts))]
      [`((,time "Note_on_c" ,pitch) . ,d)
       (let ((on (cons pitch on)))
         (loop d on time (add-to-count on time counts)))]
      [`((,time "Note_off_c" ,pitch) . ,d)
       (let ((on (remove pitch on)))
         (loop d on time (remove-from-count pitch time counts)))]
      [else (error 'parse-csv (format "unknown instr ~s" track))]))))

;; translates translated MIDI back into CSV format

(define (words->csv-loop track on)
  (match track
    ['() '()]
    [`((,t ,word) . ,d)
     (let* ((word (string->list word))
            (turned-on (set-difference word on))
            (turned-off (set-difference on word)))
       (append
        (map (make-note-off t) (map char->integer turned-off))
        (map (make-note-on t) (map char->integer turned-on))
        (words->csv-loop d (set-union word (set-difference on turned-off)))))]))

(define (words->csv words)
  (let ((max (foldr max 0 (map car words))))
    (append (make-header max) (words->csv-loop words '()) (make-footer max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface functions to be used by y'all!!

(define (encode-csv in out)
  ((write-file (csv->words (read-MIDI in))) out))

(define (decode-csv in out)
  ((write-file (map format-MIDI (words->csv (read-encoding in)))) out))



;;; running from the command-line
(define args (current-command-line-arguments))
(if (= (vector-length args) 3)
    (let ((mode (vector-ref args 0))
          (input-file (vector-ref args 1))
          (output-file (vector-ref args 2)))
      (match mode
        ["encode" (begin
                  (encode-csv input-file output-file)
                  (displayln
                   (format
                    "successfully encoded the contents of ~s as text and saved in ~s"
                    input-file
                    output-file)))]
        ["decode" (begin
                  (decode-csv input-file output-file)
                  (displayln
                   (format
                    "successfully decoded the contents of ~s to midi and saved in ~s"
                    input-file
                    output-file)))]
        
        [else (displayln "expected either 'text' or 'midi' as first argument")]))
    (begin
      (displayln "3 arguments expected:")
      (displayln "1. program mode (encode / decode) which tells the type of the input")
      (displayln "2. source file")
      (displayln "3. destination file")))



#|

here are examples of how to run the file (from the directoy midi-csv).

racket translate.rkt tells the computer which file to run (obv)

and then the first argument must be either midi or text, and tells
the program to run either encode or decode


racket translate.rkt encode complex/complex.csv complex/complex.txt

racket translate.rkt decode complex/complex.txt complex/complex2.csv
|#