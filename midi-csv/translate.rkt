#lang racket

(require csv-reading csv-writing)


;; reads CSV file into Racket data
(define (read-MIDI filename)
  (define reader
    (make-csv-reader-maker
     '((separator-chars            #\,)
       (strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t))))
  (let ((next-row (reader (open-input-file filename))))
    (filter
     (λ (x) x)
     (csv-map
      (λ (x)
        (match x
          [`(,_ ,time "Header" . ,c) #f]
          [`(,_ ,time "Start_track" . ,c) #f]
          [`(,_ ,time "Title_t" . ,c) #f]
          [`(,_ ,time "Time_signature" . ,c) #f]
          [`(,_ ,time "End_track" . ,c) #f]
          [`(,_ ,time "End_of_file" . ,c) #f]
          ;;;;;;;
          [`(,_ ,time "Note_on_c" ,chan ,pitch ,vel)
           (list (string->number time) "on" (string->number pitch))]
          [`(,_ ,time "Note_off_c" ,chan ,pitch ,vel)
           (list (string->number time) "off" (string->number pitch))]))
      next-row))))

(define (read-encoding filename)
  (define reader
    (make-csv-reader-maker
     '((separator-chars            #\,)
       (strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t))))
  (let ((next-row (reader (open-input-file filename))))
    (csv-map (λ (x) x) next-row)))


(define (fix-state s)
  (match s
    ["on" "Note_on_c"]
    ["off" "Note_off_c"]))

(define DEFAULT-TRASH 0)
(define DEFAULT-CHANNEL 1)
(define DEFAULT-VELOCITY 60)

(define (write-MIDI csv)
  (map
   (λ (x)
     (match x
       [`(,time ,state ,pitch)
        (list DEFAULT-TRASH
              (number->string time)
              (fix-state state)
              DEFAULT-CHANNEL
              (number->string pitch)
              DEFAULT-VELOCITY)]))
   csv))

(define ((write-MIDI-file e) fname)
  (let ((t (write-MIDI e))
        (p (open-output-file fname #:exists 'replace)))
    (display-table t p)
    (close-output-port p)))


(define EXAMPLE "very_complex/very_complex.csv")

(define (set-cons x ls) (if (member x ls) ls (cons x ls)))
(define (to-set ls) (foldr set-cons '() ls))
(define (set-difference s1 s2)
  (foldr
   (λ (x a) (if (member x s2) a (cons x a)))
   '()
   s1))

(define (add-to-count pitches time counts)
  (map
   (λ (x)
     (if (= (car x) time)
         (cons (car x) (sort (to-set (append (cdr x) pitches)) <))
         x))
   counts))

(define (remove-from-count pitch time counts)
  (map
   (λ (x)
     (if (= (car x) time)
         (cons (car x) (remove pitch (cdr x)))
         x))
   counts))

;; translates Racket-data CSV into text version
(define (csv->words csv)
  (let ((MAX (foldr max 0 (map first csv))))
    (let loop ((track csv)
               (on '())
               (i 0)
               (counts (build-list (add1 MAX) (λ (x) `(,x)))))
    (match track
      ['() (map
            (λ (x)
              (let ((word (list->string (map integer->char (cdr x)))))
                (list (car x) word)))
            counts)]
      [`((,time ,state ,pitch) . ,d)
       (if (<= i time)
           (loop track on (add1 i) (add-to-count on i counts))
           (match state
             ["on" (let ((on (cons pitch on)))
                     (loop d on time (add-to-count on time counts)))]
             ["off"
              (let ((on (remove pitch on)))
                (loop d on time (remove-from-count pitch time counts)))]))]
      [else (error 'parse-csv (format "unknown instr ~s" track))]))))

(define ((make-note-on time) n) `(,time "on" ,n))
(define ((make-note-off time) n) `(,time "off" ,n))

(define (words->csv words)
  (let loop ((track words)
             (on '()))
    (match track
      ['() '()]
      [`((,t ,word) . ,d)
       (let ((word (string->list word)))
         (let ((turned-on (set-difference word on))
               (turned-off (set-difference on word)))
           (append
            (sort (append
                   (map (make-note-off t) (map char->integer turned-off))
                   (map (make-note-on t) (map char->integer turned-on)))
                  (λ (x y) (< (caddr x) (caddr y))))
            (loop d (to-set (append word (set-difference on turned-off)))))))])))

(define ((write-file e) fname)
  (let ((p (open-output-file fname #:exists 'replace)))
    (display-table e p)
    (close-output-port p)))




(define (encode-csv in out) ((write-file (csv->words (read-MIDI in))) out))

(define (decode-csv in out) ((write-file (words->csv (read-encoding in))) out))
