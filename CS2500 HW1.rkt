;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |CS2500 HW1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Letters is one of:
;; - (cons required_letter '())
;; - (cons String Letters)
;; Interpretation: The base case represents the center letter and
;; the rest is available letters

;; letters-template : Letters -> ?
(define (letters-template alist)
  (cond
    [(empty? (rest alist)) (... (first alist) ...)]
    [(cons? alist) (... (first alist) ... (letters-template (rest alist)) ...)]))

;; PreviousWords is one of:
;; - '()
;; - (cons String PreviousWords)
;; Interpretation: List of previous words that the user entered,
;; empty list means user hasn't entered any words yet.

;; previous-words-template : PreviousWords -> ?
(define (previous-words-template prev)
  (cond
    [(empty? prev) ...]
    [(cons? prev) (... (first prev) ... (previous-words-template (rest prev)) ...)]))

(define-struct world [l pw prev])

;; A World is a (make-world Letters String PreviousWords)
;; A (make-world [l pw prev]) represents a World with available
;; letters, the partial word the user has entered, and the
;; previous words the user entered before.

;; world-template : World -> ?
(define (world-template w)
  (... (letters-template (world-l w)) ...
       (world-pw w) ...
       (previous-words-template (world-prev w)) ...))

(define LETTERS-1 (list "Y" "P" "T" "E" "N" "H" "O"))
(define LETTERS-2 (list "A" "B" "C" "D" "F" "G" "I" "Y" "P" "T" "E" "N" "H" "O"))
(define WORLD-1 (make-world LETTERS-1 "POT" '()))
(define WORLD-2 (make-world LETTERS-2 "" '()))

;; play : World -> World
;; Uses big-bang to play a game of Spelling Bee
(define (play w)
  (big-bang w
    [to-draw world->image]
    [on-key key-pressed]))

;; world->image : World -> Image
;; Consumes a world and outputs an image of the world
(define (world->image w)
  (beside (overlay 
           (above (text (world-pw w) 40 "black")
                  (letters->image (world-l w)))
           (rectangle 400 500 "solid" "white"))
          (overlay/align/offset
           "middle" "top"
           (prev-list->image (world-prev w))
           0 -30
           (rectangle 400 500 "solid" "white"))))

(check-expect (world->image WORLD-1)
              (beside (overlay 
                       (above (text (world-pw WORLD-1) 40 "black")
                              (letters->image (world-l WORLD-1)))
                       (rectangle 400 500 "solid" "white"))
                      (overlay/align/offset
                       "middle" "top"
                       (prev-list->image (world-prev WORLD-1))
                       0 -30
                       (rectangle 400 500 "solid" "white"))))

;; letters->image : Letters -> Image
;; It consumes letters and produces an image of corresponding letter-status
(define (letters->image alist)
  (cond
    [(empty? (rest alist))
     (overlay (overlay (text (first alist) 20 "black") (circle 30 "solid" (color 245 218 73)))
              (square 350 "solid" "white"))]
    [(cons? alist) (overlay/offset (overlay (text (first alist) 20 "black") (circle 30 "solid" (color 230 230 230)))
                                   (if (positive? (- 7 (length (rest alist))))
                                       (* 65 (cos (* (/ (* 2 pi) 6) (length (rest alist)))))
                                       (* 130 (cos (* (/ (* 2 pi) 12) (length (rest alist))))))
                                   (if (positive? (- 7 (length (rest alist))))
                                       (* 65 (sin (* (/ (* 2 pi) 6) (length (rest alist)))))
                                       (* 130 (sin (* (/ (* 2 pi) 12) (length (rest alist))))))
                                   (letters->image (rest alist)))]))

(check-expect (letters->image (list "A"))
              (overlay (overlay (text "A" 20 "black")
                                (circle 30 "solid" (color 245 218 73)))
                       (square 350 "solid" "white")))

(check-expect (letters->image (list "A" "B" "C"))
              (overlay/offset (overlay (text "A" 20 "black") (circle 30 "solid" (color 230 230 230)))
                              (* 65 (cos (* (/ (* 2 pi) 6) 2)))
                              (* 65 (sin (* (/ (* 2 pi) 6) 2)))
                              (overlay/offset (overlay (text "B" 20 "black") (circle 30 "solid" (color 230 230 230)))
                                              (* 65 (cos (* (/ (* 2 pi) 6) 1)))
                                              (* 65 (sin (* (/ (* 2 pi) 6) 1)))
                                              (overlay (overlay (text "C" 20 "black")
                                                                (circle 30 "solid" (color 245 218 73)))
                                                       (square 350 "solid" "white")))))

(check-expect (letters->image (list "A" "B" "C" "D" "E" "F" "G" "H"))
              (overlay/offset (overlay (text "A" 20 "black") (circle 30 "solid" (color 230 230 230)))
                              (* 130 (cos (* (/ (* 2 pi) 12) 7)))
                              (* 130 (sin (* (/ (* 2 pi) 12) 7)))
                              (letters->image (list "B" "C" "D" "E" "F" "G" "H"))))

;; prev-list->image : PreviousWords -> Image
;; Displays image of previous words
(define (prev-list->image prev)
  (cond
    [(empty? prev) (rectangle 1 1 "solid" "white")]
    [(cons? prev) (above (text (first prev) 18 "black")
                         (prev-list->image (rest prev)))]))

(check-expect (prev-list->image '()) (rectangle 1 1 "solid" "white"))
(check-expect (prev-list->image (list "hi"))
              (above (text "hi" 18 "black") (rectangle 1 1 "solid" "white")))

;; key-pressed : World KeyEvent -> World
;; Responds to key press to update partial word according to letter availability;
;; Pressing [enter] adds the partial word to the previous word list and clears the word.
(define (key-pressed w k)
  (cond
    [(key-in-letters (world-l w) k)
     (make-world (world-l w) (string-append (world-pw w) (string-upcase k)) (world-prev w))]
    [(and (string=? k "\r") (string-contains? (first (reverse (world-l w))) (world-pw w)))
     (make-world (world-l w) "" (cons (world-pw w) (world-prev w)))]
    [else w]))

(check-expect (key-pressed WORLD-1 "y") (make-world LETTERS-1 "POTY" '()))
(check-expect (key-pressed WORLD-1 "z") (make-world LETTERS-1 "POT" '()))
(check-expect (key-pressed WORLD-1 "\r") (make-world LETTERS-1 "" (cons "POT" '())))

;; key-in-letters : Letters KeyEvent -> Boolean
;; Returns whether key in available letters
(define (key-in-letters l k)
  (cond
    [(empty? (rest l)) (string=? (first l) (string-upcase k))]
    [(cons? l) (or (string=? (first l) (string-upcase k))
                   (key-in-letters (rest l) k))]))

(check-expect (key-in-letters LETTERS-1 "o") #true)
(check-expect (key-in-letters LETTERS-1 "x") #false)

(play WORLD-1)