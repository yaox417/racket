;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname spaceship-improve) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
#|
THIS IS A RENDITION OF THE SPACE INVADER GAME THAT ALLOWS FOR CUSTOMIZATION!
This rendition features:
1) the ability to tweak the invader bullet speed
(with the speed of spaceship bullets fixed at 10),
2) invaders are easier to hit on a lower level
3) invaders fire less on a lower level

|#
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; ### DATA DEFINITIONS ###
;; An Invader is a Posn 
;; INTERP: represents the location of the invader

;; A Bullet is a Posn 
;; INTERP: represents the location of a bullet

;; A Location is a Posn 
;; INTERP: represents a location of a spaceship

;; fun-of-posn: Posn -> ???
#;(define (fun-of-posn a-posn)
    (...(posn-x a-posn) ... (posn-y a-posn) ... ))

;; A Direction is one of: 
;; - 'left 
;; - 'right 
;; INTERP: represent the direction of movement for the spaceship

;; fun-of-direction: Direction -> ???
#;(define (fun-of-direction a-direction)
    (cond
      [(symbol=? a-direction 'left) ...]
      [(symbol=? a-direction 'right) ...]))

(define-struct ship (dir loc))
;; A Ship is (make-ship Direction Location) 
;; INTERP: represent the spaceship with its current direction 
;;         and movement

;; fun-of-ship: Ship -> ???
#;(define (fun-of-ship a-ship)
    (... (fun-of-dir (ship-dir a-ship)) ...
         (fun-of-posn (ship-loc a-ship)) ... ))

;; A List of Invaders (LoI) is one of 
;; - empty 
;; - (cons Invader LoI)

;; fun-of-invaders: LoI -> ???
#;(define (fun-of-invaders an-invader)
    (cond
      [(empty? an-invader) ... ]
      [else (... (fun-of-posn (first an-invader)) ...
                 (fun-of-invaders (rest an-invader)) ... )]))

;; A List of Bullets (LoB) is one of 
;; - empty
;; - (cons Bullet LoB)

;; fun-of-bullets: LoB -> ???
#;(define (fun-of-bullets a-bullet)
    (cond
      [(empty? a-bullet) ...]
      [else (... (fun-of-posn (first a-bullet)) ...
                 (fun-of-bullets (rest a-bullet)) ... )]))

(define-struct world (ship invaders ship-bullets invader-bullets level))
;; A World is (make-world Ship LoI LoB LoB PosNum) 
;; INTERP: represent the ship, the current list of invaders,
;;         the inflight spaceship bullets
;;         the inflight invader bullets
;;         and the level of the game

;; fun-of-world: World -> ???
#;(define (fun-of-world a-world)
    ... (fun-of-ship (world-ship a-world))
    ... (fun-of-invaders (world-invaders a-world))
    ... (fun-of-bullets (world-ship-bullets a-world))
    ... (fun-of-bullets (world-invader-bullets a-world))
    ... (fun-of-level (world-level a-world)))


;; MORE GENERAL DATA DEFINITIONS:
;; A [List-of X] is one of:
;; - '()
;; - (cons X [List-of X])
;; WHERE X is a Number, String, Posn, Symbol, or any well-defined Racket Value

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; ### CONSTANTS ###
(define WIDTH 500) 
(define HEIGHT 500) 

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define TXT (text "Choose level: 1 = EASY; 2 = MEDIUM; 3 = HARD" 20 "black"))

(define MAX-SHIP-BULLETS 3)

(define SPACESHIP-BULLET-IMAGE (circle 2 'solid 'black))

(define SHIP-WIDTH 25)

(define SHIP-HEIGHT 15)

(define SPACESHIP-IMAGE (rectangle SHIP-WIDTH SHIP-HEIGHT 'solid 'black))

(define INVADER-SIDE 20)

(define INVADER-IMAGE (square INVADER-SIDE 'solid 'red))

(define INVADER-BULLET-IMAGE (circle 2 'solid 'red))

(define SHIP-SPEED 10)
;; this accounts for both the ship bullet speed and the ship speed

(define BULLET-SPEED-BASE 2)
;; this will be used in calculating the speed of the bullet on different levels

(define SHIP-INIT (make-ship 'left (make-posn 250 480)))

(define INVADERS-INIT 
  (list (make-posn 100 20) (make-posn 140 20) (make-posn 180 20) 
        (make-posn 220 20) (make-posn 260 20) (make-posn 300 20) 
        (make-posn 340 20) (make-posn 380 20) (make-posn 420 20)
        (make-posn 100 50) (make-posn 140 50) (make-posn 180 50) 
        (make-posn 220 50) (make-posn 260 50) (make-posn 300 50) 
        (make-posn 340 50) (make-posn 380 50) (make-posn 420 50)
        (make-posn 100 80) (make-posn 140 80) (make-posn 180 80) 
        (make-posn 220 80) (make-posn 260 80) (make-posn 300 80) 
        (make-posn 340 80) (make-posn 380 80) (make-posn 420 80)
        (make-posn 100 110) (make-posn 140 110) (make-posn 180 110) 
        (make-posn 220 110) (make-posn 260 110) (make-posn 300 110) 
        (make-posn 340 110) (make-posn 380 110) (make-posn 420 110)))

;; Level is one of:
;; - 1
;; - 2
;; - 3
;; INTERP: Level 1 is the lowest level - EASY;
;;         Level 2 is MEDIUM;
;;         Level 3 is HARD

;; fun-of-level: Level -> ??? 
#;(define (fun-of-level alevel)
    (cond
      [(= 1 alevel) ...]
      [(= 2 alevel) ...]
      [(= 3 alevel) ...]))

;; -----------------------------------------------------------------------------
;; GAMEWORLD-STATES
;; A Gameworld is one of:
;; - String
;; - World
(define world0 "Choose level: 1 = EASY; 2 = MEDIUM; 3 = HARD")

(define WORLD-INIT (make-world SHIP-INIT INVADERS-INIT empty empty 1))

(define world1 (make-world SHIP-INIT INVADERS-INIT
                           (list (make-posn 400 30) (make-posn 350 300))
                           (list (make-posn 30 100)) 1))

;; fun-of-gw: Gameworld -> ???
#;(define (fun-of-gw agw)
    (cond
      [(string? agw) ...]
      [(world? agw) (fun-of-world agw) ...]))
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

;; ### FUNCTIONS ###
;; -----------------------------------------------------------------------------
;; DRAW-WORLD

;; Posn Image Image -> Image
;; the general helper function for all the drawing
;; i can use this directly to draw the ship
(check-expect (draw-general-helper (make-posn 100 100) SPACESHIP-IMAGE
                                   (empty-scene 300 300))
              (place-image SPACESHIP-IMAGE 100 100 (empty-scene 300 300)))

(define (draw-general-helper aposn aimg abg)
  (place-image aimg (posn-x aposn) (posn-y aposn) abg))

;; GENERAL DRAW FUNCTION
;; draw-general: String [List-of Posn] Image -> Image
;; a general function that can draw all the images that involve a [List-of Posn]
(check-expect (draw-general INVADER-IMAGE
                            (list (make-posn 30 40) (make-posn 200 30))
                            BACKGROUND)
              (place-image INVADER-IMAGE 30 40
                           (place-image INVADER-IMAGE 200 30 BACKGROUND)))
(check-expect (draw-general SPACESHIP-BULLET-IMAGE
                            (list (make-posn 10 10) (make-posn 30 20))
                            BACKGROUND)
              (place-image SPACESHIP-BULLET-IMAGE 10 10
                           (place-image SPACESHIP-BULLET-IMAGE 30 20 BACKGROUND)))
(check-expect (draw-general INVADER-BULLET-IMAGE
                            (list (make-posn 30 40) (make-posn 200 30))
                            BACKGROUND)
              (place-image INVADER-BULLET-IMAGE 30 40
                           (place-image INVADER-BULLET-IMAGE 200 30 BACKGROUND)))

(define (draw-general aimg alop abg)
  (local [(define (draw-gen-helper aposn abg)
            (draw-general-helper aposn aimg abg))]
    (foldr draw-gen-helper abg alop)))

;; draw-world: World -> Image 
;; Draw the world on the canvas
(define (draw-world a-world)
  (draw-general SPACESHIP-IMAGE (list (ship-loc (world-ship a-world)))
                (draw-general INVADER-IMAGE
                              (world-invaders a-world)
                              (draw-general SPACESHIP-BULLET-IMAGE
                                            (world-ship-bullets a-world)
                                            (draw-general INVADER-BULLET-IMAGE
                                                          (world-invader-bullets a-world)
                                                          BACKGROUND)))))

;; draw-gameworld: Gameworld -> Image
;; draws the gameworld-state onto the canvas
(check-expect (draw-gameworld "d") (overlay TXT BACKGROUND))

(define (draw-gameworld agw)
  (cond
    [(string? agw) (overlay TXT BACKGROUND)]
    [(world? agw) (draw-world agw)]))

;; -----------------------------------------------------------------------------
;; # GENERAL HELPER FOR MOVE-X #

;; update-posn: Posn Number Number -> Posn
;; update a Posn by the given parameters
(check-expect (update-posn (make-posn 10 10) -5 5) (make-posn 5 15))

(define (update-posn a-posn d1 d2)
  (make-posn (+ (posn-x a-posn) d1) (+ (posn-y a-posn) d2)))

;; hit-range: Posn Posn Number -> Boolean
;; identify whether a posn hits another posn within a given range
(check-expect (hit-range (make-posn 100 100) (make-posn 70 70) 20) #false)
(check-expect (hit-range (make-posn 100 100) (make-posn 80 90) 20) #true)
(check-expect (hit-range (make-posn 100 100) (make-posn 100 100) 0) #true)
(check-expect (hit-range (make-posn 100 100) (make-posn 99 99) 0) #false)
(check-expect (hit-range (make-posn 350 110) (make-posn 300 105) 10) #false)

(define (hit-range posn1 posn2 n)
  (and (<= (- (posn-x posn1) n) (posn-x posn2) (+ (posn-x posn1) n))
       (<= (- (posn-y posn1) n) (posn-y posn2) (+ (posn-y posn1) n))))

;; # END OF GENERAL HELPER FOR MOVE-X #
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; MOVE-SPACESHIP

;; update-ship-loc: Ship -> Posn
;; update the location of a spaceship and return the new Posn
(check-expect (update-ship-loc SHIP-INIT) (make-posn 240 480))

(define (update-ship-loc a-ship)
  (local [;; Direction -> Number
          (define (convert-dir a-dir)
            (cond
              [(symbol=? a-dir 'left) -1]
              [(symbol=? a-dir 'right) 1]))]
    (update-posn (ship-loc a-ship)
                 (* (convert-dir (ship-dir a-ship)) SHIP-SPEED) 0)))

;; move-spaceship: Ship -> Ship
;; move the ship in the appropriate direction 
(check-expect (move-spaceship (make-ship 'left (make-posn 300 300)))
              (make-ship 'left (make-posn 290 300)))
(check-expect (move-spaceship (make-ship 'right (make-posn 300 300)))
              (make-ship 'right (make-posn 310 300)))
(check-expect (move-spaceship (make-ship 'left (make-posn 5 300)))
              (make-ship 'left (make-posn 5 300)))

(define (move-spaceship a-ship)
  (local [;; Ship -> Boolean
          (define (stop? a-ship)
            (cond
              [(symbol=? (ship-dir a-ship) 'left)
               (> 0 (posn-x (update-ship-loc a-ship)))] 
              [(symbol=? (ship-dir a-ship) 'right)
               (< WIDTH (posn-x (update-ship-loc a-ship)))]))]
    (if (stop? a-ship) a-ship
        (make-ship (ship-dir a-ship) (update-ship-loc a-ship)))))

;; -----------------------------------------------------------------------------
;; MOVE-BULLETS
;; BulletDir is one of:
;; - SPACESHIP-DIR
;; - INVADER-DIR

(define SPACESHIP-DIR -1)
(define INVADER-DIR 1)

;; move-bullets : LoB BulletDir PosNum -> LoB
;; move each bullet in the specified direction by SPEED units
(check-expect (move-bullets (list (make-posn 400 30) (make-posn 350 300))
                            SPACESHIP-DIR SHIP-SPEED)
              (list (make-posn 400 20) (make-posn 350 290)))
(check-expect (move-bullets (list (make-posn 350 300)) INVADER-DIR 4)
              (list (make-posn 350 (+ 4 300))))

(define (move-bullets alob a-bullet-dir a-bullet-speed)
  (local [;; Posn -> Posn
          (define (move-one-bullet aposn)
            (update-posn aposn 0 (* a-bullet-speed a-bullet-dir)))]
    ;; [Posn -> Posn] [List-of Posn] -> [List-of Posn]
    (map move-one-bullet alob)))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; # HELPER FOR REMOVE-HITS-AND-OUT-OF-BOUND #
;; -----------------------------------------------------------------------------
;; REMOVE-HITS

;; remove-hits: [List-of Posn] [List-of Posn] PosNum -> [List-of Posn]
;; remove hit Posns from the first [List-of Posn] given a level
(check-expect (remove-hits (list (make-posn 300 110)
                                 (make-posn 380 110)
                                 (make-posn 420 110))
                           (list (make-posn 300 110)) 1)
              (list (make-posn 380 110) (make-posn 420 110)))

(define (remove-hits alob1 alob2 alevel)
  (local [;; Level -> Number
          ;; get the hit criteria given the range
          (define (get-range alevel)
            (cond
              [(= 1 alevel) (round (/ INVADER-SIDE 2))]
              [(= 2 alevel) (round (/ INVADER-SIDE 3))]
              [(= 3 alevel) (round (/ INVADER-SIDE 4))]))
          ;; hit-n?: Posn -> Boolean
          (define (hit-n? aposn)
            (not (hit-abst? aposn alob2 (get-range alevel))))]
    ;; [Posn -> Boolean] [List-of Posn] -> [List-of Posn]
    (filter hit-n? alob1)))

;; hit-abst?: Posn [List-of Posn] Level -> Boolean
;; checks to see if a Posn is hit according to the level
(check-expect (hit-abst? (make-posn 300 110) 
                         (list (make-posn 308 110) (make-posn 200 110)) 10) #true)
(check-expect (hit-abst? (make-posn 300 110)
                         (list (make-posn 308 110) (make-posn 200 110)) 0) #false)

(define (hit-abst? aposn1 alop n)
  (local [;; hit-range-abst: Posn -> Boolean
          (define (hit-range-abst aposn2) 
            (hit-range aposn1 aposn2 n))]
    ;; [Posn -> Boolean] [List-of Posn] -> Boolean
    (ormap hit-range-abst alop)))

;; -----------------------------------------------------------------------------
;; OUT-OF-BOUND
(check-expect (out-of-bound (list (make-posn 400 -10) (make-posn 350 300)))
              (list (make-posn 350 300)))
(check-expect (out-of-bound (list (make-posn 400 10) (make-posn 350 300)))
              (list (make-posn 400 10) (make-posn 350 300)))

;; out-of-bound: [List-of Posn] -> [List-of Posn]
;; get rid of Posns that are out of bound
(define (out-of-bound alop)
  (local [;; Posn -> Boolean
          (define (out-of-bound-posn? aposn)
            (and (<= 0 (posn-x aposn) WIDTH) (<= 0 (posn-y aposn) HEIGHT)))]
    ;; [Posn -> Boolean] [List-of Posn] -> [List-of Posn]
    (filter out-of-bound-posn? alop)))

;; # END OF HELPER FOR REMOVE-HITS-AND-OUT-OF-BOUND #
;; -----------------------------------------------------------------------------
;; remove-hits-and-out-of-bounds: World -> World 
;; remove any invaders that have been hit by a spaceship bullet
;; remove any bullets that are out of bounds
(check-expect (remove-hits-and-out-of-bounds
               (make-world SHIP-INIT INVADERS-INIT
                           (list (make-posn 400 -10) (make-posn 350 300))
                           (list (make-posn 600 100)) 1))
              (make-world SHIP-INIT INVADERS-INIT
                          (list (make-posn 350 300)) '() 1))

(check-expect (remove-hits-and-out-of-bounds
               (make-world SHIP-INIT INVADERS-INIT
                           (list (make-posn 340 110) (make-posn 350 300))
                           (list (make-posn 300 100)) 1))
              (make-world SHIP-INIT
                          (list (make-posn 100 20) (make-posn 140 20)
                                (make-posn 180 20) (make-posn 220 20)
                                (make-posn 260 20) (make-posn 300 20) 
                                (make-posn 340 20) (make-posn 380 20)
                                (make-posn 420 20) (make-posn 100 50)
                                (make-posn 140 50) (make-posn 180 50) 
                                (make-posn 220 50) (make-posn 260 50)
                                (make-posn 300 50) (make-posn 340 50)
                                (make-posn 380 50) (make-posn 420 50)
                                (make-posn 100 80) (make-posn 140 80)
                                (make-posn 180 80) (make-posn 220 80)
                                (make-posn 260 80) (make-posn 300 80) 
                                (make-posn 340 80) (make-posn 380 80)
                                (make-posn 420 80) (make-posn 100 110)
                                (make-posn 140 110) (make-posn 180 110) 
                                (make-posn 220 110) (make-posn 260 110)
                                (make-posn 300 110) (make-posn 380 110)
                                (make-posn 420 110))
                          (list (make-posn 350 300))
                          (list (make-posn 300 100)) 1))

(define (remove-hits-and-out-of-bounds a-world)
  (make-world (world-ship a-world)
              (remove-hits (world-invaders a-world)
                           (world-ship-bullets a-world)
                           (world-level a-world))
              (out-of-bound (remove-hits (world-ship-bullets a-world)
                                         (world-invaders a-world)
                                         (world-level a-world)))
              (out-of-bound (world-invader-bullets a-world))
              (world-level a-world)))

;; -----------------------------------------------------------------------------
;; INVADER FIRE

;; invaders-fire-total: LoB LoI PosNum -> LoB
;; add bullets from invaders randomly
;; randomness in both which invader fires, and how many invaders fire in total
;; ensuring that the total is not above the designated level amount
;; check the count check works
(check-expect (invaders-fire-total
               (list (make-posn 300 100) (make-posn 305 105)
                     (make-posn 310 90) (make-posn 30 10)
                     (make-posn 200 100) (make-posn 300 200)
                     (make-posn 310 100) (make-posn 400 100)
                     (make-posn 10 100) (make-posn 30 100))
               (world-invaders WORLD-INIT) 10)
              (list (make-posn 300 100) (make-posn 305 105)
                    (make-posn 310 90) (make-posn 30 10)
                    (make-posn 200 100) (make-posn 300 200)
                    (make-posn 310 100) (make-posn 400 100)
                    (make-posn 10 100) (make-posn 30 100)))

;; the real check is actually in add-fire
(check-random (invaders-fire-total (list (make-posn 300 100))
                                   (world-invaders WORLD-INIT) 10)
              (add-fire (random 9) (list (make-posn 300 100))
                        (world-invaders WORLD-INIT)))
(check-expect (invaders-fire-total (list (make-posn 300 100)) '() 8)
              (list (make-posn 300 100)))

(define (invaders-fire-total a-lob a-loi invader-cap)
  (local [;; LoB LoI Number -> PosNum
          ;; determines how many invaders should fire
          ;; and avoid one fire too many situation at the end
          (define (how-many-fire a-lob a-loi invader-cap)
            (min (- invader-cap (length a-lob)) (length a-loi)))]
    (if (and (< (length a-lob) invader-cap) (< 0 (length a-loi)))
        (add-fire (random (how-many-fire a-lob a-loi invader-cap)) a-lob a-loi)
        a-lob)))

;; add-fire: NaturalNumber LoB LoI -> LoB
;; add bullets from invaders N times to the end of LoB
;; because the bullets in the front are more likely to be out of bound
(check-random (add-fire 1 (list (make-posn 30 10)) (world-invaders WORLD-INIT))
              (list (make-posn 30 10)
                    (list-ref (world-invaders WORLD-INIT) (random 36))))

(define (add-fire n a-lob a-loi)
  (local [;; LoI -> Posn
          ;; fire from a random invader
          (define (invaders-fire a-loi)
            (list-ref a-loi (random (length a-loi))))
          ;; [List-of X] X -> [List-of X]
          ;; add to the end of the list 
          (define (add-to-end a-list an-element)
            (foldr cons (list an-element) a-list))]
    (cond
      [(zero? n) a-lob]
      [else (add-to-end (add-fire (sub1 n) a-lob a-loi) (invaders-fire a-loi))])))

;; -----------------------------------------------------------------------------
;; MOVE-GAMEWORLD

;; # HELPER FOR MOVE-GAMEWORLD #
;; move-ship-bullets: LoB -> LoB
;; move the spaceship bullets
(check-expect (move-ship-bullets (list (make-posn 10 30) (make-posn 20 20)))
              (list (make-posn 10 20) (make-posn 20 10)))

(define (move-ship-bullets alob)
  (move-bullets alob SPACESHIP-DIR SHIP-SPEED))

;; move-and-fire: LoB LoI Level -> LoB
;; fire and move invader bullets according to the level
(check-expect (move-and-fire (list (make-posn 30 40)) '() 1)
              (list (make-posn 30 44)))

(define (move-and-fire alob aloi alevel)
  (local [;; Level -> Number
          (define (get-max-bullets alevel)
            (* 2 (+ 2 alevel)))
          ;; Level -> Number
          (define (get-bullet-speed alevel)
            (* (+ alevel 1) BULLET-SPEED-BASE))]
    (move-bullets (invaders-fire-total alob aloi (get-max-bullets alevel))
                  INVADER-DIR
                  (get-bullet-speed alevel))))

;; move-world: World -> World
;; 1. move the spaceship
;; 2. move any spaceship bullets
;; 3. fire any invaders bullets if you can
;; 4. move any invader bullets
;; 5. remove any invaders that were hit
;; and any (invader or spaceship) bullets that are out of bounds

(check-random (move-world WORLD-INIT)
              (make-world (make-ship 'left (make-posn 240 480))
                          INVADERS-INIT '()
                          (move-bullets
                           (invaders-fire-total '() INVADERS-INIT 6)
                           INVADER-DIR 4) 1))

(define (move-world aworld)
  (remove-hits-and-out-of-bounds
   (make-world (move-spaceship (world-ship aworld))
               (world-invaders aworld)
               (move-ship-bullets (world-ship-bullets aworld))
               (move-and-fire (world-invader-bullets aworld)
                              (world-invaders aworld)
                              (world-level aworld))
               (world-level aworld))))

;; move-gameworld: Gameworld -> Gameworld
;; move a gameworld
(check-expect (move-gameworld world0) world0)
(check-random (move-gameworld WORLD-INIT) (move-world WORLD-INIT))

(define (move-gameworld aworld)
  (cond
    [(string? aworld) aworld]
    [(world? aworld) (move-world aworld)]))

;; -----------------------------------------------------------------------------
;; ### KEY EVENTS ###

;; get-level: Gameworld Keyevent -> Gameworld
;; let player choose the level of game and start game
(check-expect (get-level world0 "1") WORLD-INIT)
(check-expect (get-level world0 " ") world0)
(check-expect (get-level world1 "1") world1)
(check-expect (get-level WORLD-INIT "d") WORLD-INIT)

(define (get-level agw akey)
  (cond
    [(string? agw) (obtain-level agw akey)]
    [(world? agw) agw]))

;; obtain-level: Gameworld Keyevent -> Gameworld
;; initate the gameworld given the level
(check-expect (obtain-level world0 "1") WORLD-INIT)
(check-expect (obtain-level world0 "2")
              (make-world SHIP-INIT INVADERS-INIT empty empty 2))
(check-expect (obtain-level world0 "3")
              (make-world SHIP-INIT INVADERS-INIT empty empty 3))
(check-expect (obtain-level world0 " ") world0)

(define (obtain-level agw akey)
  (cond
    [(string=? akey "1") WORLD-INIT]
    [(string=? akey "2")
     (make-world SHIP-INIT INVADERS-INIT empty empty 2)]
    [(string=? akey "3")
     (make-world SHIP-INIT INVADERS-INIT empty empty 3)]
    [else agw]))

;; change-dir: Ship KeyEvent -> Ship
;; change direction of the ship
(check-expect (change-dir (make-ship 'left (make-posn 40 250)) "right")
              (make-ship 'right (make-posn 40 250)))
(check-expect (change-dir (make-ship 'left (make-posn 40 250)) "left")
              (make-ship 'left (make-posn 40 250)))
(check-expect (change-dir (make-ship 'right (make-posn 40 250)) "left")
              (make-ship 'left (make-posn 40 250)))
(check-expect (change-dir (make-ship 'right (make-posn 40 250)) " ")
              (make-ship 'right (make-posn 40 250)))

(define (change-dir a-ship a-key)
  (cond
    [(or (string=? "left" a-key) (string=? "right" a-key))
     (make-ship (string->symbol a-key) (ship-loc a-ship))]
    [else a-ship]))

;; ship-fire: Ship LoB KeyEvent -> LoB
;; fire bullets from the ship
(check-expect (ship-fire (make-ship 'left (make-posn 40 480))
                         (list (make-posn 30 10)) " ")
              (list (make-posn 40 480) (make-posn 30 10)))
(check-expect (ship-fire (make-ship 'left (make-posn 40 480))
                         (list (make-posn 30 10)) "e")
              (list (make-posn 30 10)))

(define (ship-fire a-ship a-lob a-key)
  (if (string=? " " a-key) (fire-from-ship a-ship a-lob) a-lob))

;; fire-from-ship: Ship Lob -> LoB
;; add a bullet from a ship to the list of bullets
;; if there are less than 3 bullets from ship
(check-expect (fire-from-ship (make-ship 'left (make-posn 40 480))
                              (list (make-posn 30 10) (make-posn 300 10)))
              (list (make-posn 40 480) (make-posn 30 10) (make-posn 300 10)))
(check-expect (fire-from-ship (make-ship 'left (make-posn 40 480))
                              (list (make-posn 30 10) (make-posn 300 10)
                                    (make-posn 350 10)))
              (list (make-posn 30 10) (make-posn 300 10) (make-posn 350 10)))

(define (fire-from-ship a-ship a-lob)
  (if (>= (length a-lob) MAX-SHIP-BULLETS)
      a-lob
      (cons (ship-loc a-ship) a-lob)))

;; update-world: World KeyEvent -> World
;; 1. change direction of the ship
;; 2. fire bullets from the ship
(check-expect (update-world WORLD-INIT " ")
              (make-world SHIP-INIT INVADERS-INIT
                          (list (make-posn 250 480)) '() 1))
(check-expect (update-world WORLD-INIT "right")
              (make-world (make-ship 'right (make-posn 250 480))
                          INVADERS-INIT '() '() 1))

(define (update-world a-world a-key)
  (make-world (change-dir (world-ship a-world) a-key) 
              (world-invaders a-world)
              (ship-fire (world-ship a-world)
                         (world-ship-bullets a-world) a-key)
              (world-invader-bullets a-world)
              (world-level a-world)))

;; update-gameworld: Gameworld Keyevent -> Gameworld
;; update the gameworld:
;; if it's a string, then start the game accordingly
;; if it's a world then update the world accordingly
(check-expect (update-gameworld WORLD-INIT " ")
              (make-world SHIP-INIT INVADERS-INIT
                          (list (make-posn 250 480)) '() 1))
(check-expect (update-gameworld world0 "1") WORLD-INIT)
(check-expect (update-gameworld world0 " ") world0)
(check-expect (update-gameworld WORLD-INIT "1") WORLD-INIT)

(define (update-gameworld agw akey)
  (cond
    [(string? agw) (get-level agw akey)]
    [(world? agw) (update-world agw akey)]))

;; -----------------------------------------------------------------------------
;; # STOP-GAMEWORLD #

;; stop-world?: World -> Boolean
;; determines if a ship has been hit
;; to make the game easier, only remove the space ship
;; if the bullet hit the center of the spaceship
(check-expect (stop-world? (make-world (make-ship 'right (make-posn 30 480))
                                       (list (make-posn 420 110))
                                       (list (make-posn 300 10))
                                       (list (make-posn 30 480)) 1)) #true)
(check-expect (stop-world? (make-world (make-ship 'right (make-posn 30 480))
                                       (list (make-posn 420 110))
                                       (list (make-posn 300 10))
                                       (list (make-posn 20 480)) 2)) #false)

(check-expect (stop-world? (make-world (make-ship 'left (make-posn 0 480)) '()
                                       (list (make-posn 300 10)) '() 2)) #true)
(check-expect (stop-world? WORLD-INIT) #false)

(define (stop-world? a-world)
  (or (hit-abst? (ship-loc (world-ship a-world))
                 (world-invader-bullets a-world)
                 (world-level a-world))
      (empty? (world-invaders a-world))))

;; stop-gameworld?: Gameworld -> Boolean
;; stop the gameworld 
(check-expect (stop-gameworld? world0) #false)
(check-expect (stop-gameworld?
               (make-world (make-ship 'left (make-posn 0 480)) '()
                           (list (make-posn 300 10)) '() 3)) #true)

(define (stop-gameworld? agw)
  (and (not (string? agw)) (stop-world? agw)))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; BIG-BANG!
(define (main str)
  (big-bang str
            [to-draw draw-gameworld]
            [on-tick move-gameworld]
            [on-key update-gameworld]
            [stop-when stop-gameworld?]))
