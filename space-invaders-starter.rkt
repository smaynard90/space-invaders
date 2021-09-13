;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;; Pilot a tank to fire missiles at invaders

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define MAX-INVADER-VELOCITY 3)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

 

;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (ListOfInvader) (ListOfMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below ListOfMissile data definition

#;
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))                              ;center going right
(define T1 (make-tank 50 1))                                       ;going right
(define T2 (make-tank 50 -1))                                      ;going left
(define T0-NEXT (make-tank (+ (/ WIDTH 2) (* TANK-SPEED 1)) 1))    ;T0, one tick later
(define T1-NEXT (make-tank (+ 50 (* TANK-SPEED 1)) 1))             ;T1, one tick later
(define T2-NEXT (make-tank (+ 50 (* TANK-SPEED -1)) -1))           ;T2, one tick later

(define T-REACHES-REDGE (make-tank (- WIDTH (* TANK-SPEED 1)) 1))  ;reaches right edge
(define T-REACHES-REDGE-NEXT (make-tank WIDTH 1))                  ;reaches right edge, next tick
(define T-REACHES-LEDGE (make-tank (abs (* TANK-SPEED -1)) -1))    ;reaches left edge
(define T-REACHES-LEDGE-NEXT (make-tank 0 -1))                     ;reaches left edge, next tick

(define T-CROSSES-REDGE (make-tank (- WIDTH 1) 1))                 ;crossing right edge on next tick
(define T-CROSSES-LEDGE (make-tank 1 -1))                          ;crossing left edge on next tick

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves along x and y by dx pixels per clock tick

(define I0 (make-invader 150 100 1))                                             ;not landed, moving right
(define I1 (make-invader 150 HEIGHT -1))                                         ;exactly landed, moving left
(define I2 (make-invader 150 (+ HEIGHT 10) 1))                                   ;> landed, moving right
(define I0-NEXT (make-invader (+ 150 (* INVADER-X-SPEED 1))                      ;I0 on next game tick
                              (+ 100 (* INVADER-Y-SPEED (abs 1)))
                              1))
(define I0-NEGATIVE (make-invader 150 100 -1))                                   ;I0 moving left

(define I0-NEGATIVE-NEXT (make-invader (+ 150 (* INVADER-X-SPEED -1))            ;I0-NEGATIVE on next game tick
                                       (+ 100 (* INVADER-Y-SPEED (abs -1)))
                                       -1))
(define I-REACHES-REDGE (make-invader (- WIDTH (* INVADER-X-SPEED 1))            ;about to reach right edge 
                                      50
                                      1))
(define I-REACHES-REDGE-NEXT (make-invader WIDTH                                 ;reached right edge
                                           (+ 50 (* INVADER-Y-SPEED (abs 1)))
                                           1))
(define I-REACHES-LEDGE (make-invader (abs (* INVADER-X-SPEED -1))               ;about to reach left edge
                                      50
                                      -1))
(define I-REACHES-LEDGE-NEXT (make-invader 0                                     ;reached left edge
                                           (+ 50 (* INVADER-Y-SPEED (abs -1)))  
                                           -1))
(define I-CROSSES-REDGE (make-invader (- WIDTH 0.5)                              ;about to cross right edge   
                                      100
                                      1))
(define I-CROSSES-REDGE-NEXT (make-invader WIDTH                                 ;attempted to cross right edge
                                           (+ 100 (* INVADER-Y-SPEED (abs -1)))
                                           -1))
(define I-CROSSES-LEDGE (make-invader 0.5 100 -1))                               ;about to cross left edge
(define I-CROSSES-LEDGE-NEXT (make-invader 0                                     ;attempted to cross left edge
                                           (+ 100 (* INVADER-Y-SPEED (abs 1)))
                                           1))

#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M0             (make-missile 150 300))                               ;not hit I0
(define M1             (make-missile (invader-x I0) (+ (invader-y I0) 10)))  ;exactly hit I0
(define M2             (make-missile (invader-x I0) (+ (invader-y I0)  5)))  ;> hit I0
(define M0-NEXT        (make-missile 150 (- 300 MISSILE-SPEED)))             ;M0 on next game tick
(define M1-NEXT        (make-missile 150 (- 110 MISSILE-SPEED)))             ;M1 on next game tick
(define M-REACHING-TOP (make-missile 150 0))                                 ;reaching top
(define M-CROSSING-TOP (make-missile 150 -1))                                ;crossing top
(define MT0            (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)))  ;T0 fires missile

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of Invader

(define LOIE empty)
(define LOI0 (list I0))
(define LOI1 (list I0 I1))
(define LOI0-NEXT (list I0-NEXT))
(define LOIRR (list I-REACHES-REDGE))
(define LOIRR-IRL (list I-REACHES-REDGE I-REACHES-LEDGE))
(define LOIRL-IRR (list I-REACHES-LEDGE I-REACHES-REDGE))
(define LOI0-IRR (list I0 I-REACHES-REDGE))
(define LOIRR-I0 (list I-REACHES-REDGE I0))



#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - reference: (first loi) is Invader
;;  - self-reference: (rest loi) is ListOfInvader



;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of Missile

(define LOME empty)
(define LOM0 (list M0))
(define LOM1 (list M0 M1))
(define LOM1M0 (list M1 M0))
(define LOM0-NEXT (list M0-NEXT))
(define LOM1-NEXT (list M0-NEXT M1-NEXT))
(define LOMT0 (list MT0))
  

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissile)
;;  - reference: (first lom) is Missile
;;  - self-reference: (rest lom) is ListOfMissile



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game LOI0 LOM0 T1))
(define G3 (make-game LOI1 LOM1 T1)) 



;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                    ; Game
    (on-tick   next-game)        ; Game -> Game
    (to-draw   render-game)      ; Game -> Image
    (stop-when stop-game?)       ; Game -> Boolean
    (on-key    handle-key)))     ; Game KeyEvent -> Game


;; Game -> Game
;; advances invaders to the left/right and downwards, based on velocity
;;     when invaders reach side-edge, change direction
;;     randomly spawn new invaders
;; advances missiles upwards, based on MISSILE-SPEED
;;     when missiles reaches top-edge, remove missile
;;     when missiles hit invaders, remove missile and invader
;; advances tank left/right based on TANK-SPEED and direction
;;     when tank reaches side-edge, halt further movement

;(define (next-game g) 0) ;stub

;<template from Game>
(define (next-game g)
  (update-lists (make-game (next-loi (game-invaders g))
                           (next-lom (game-missiles g))
                           (next-tank (game-tank g)))))



;; Game -> Game
;; Updates lists of invaders and missiles based on collision
(check-expect (update-lists G0) G0)           ;both lists are empty
(check-expect (update-lists                   ;LOI empty
               (make-game empty LOM0 T0))
              (make-game empty LOM0 T0))
(check-expect (update-lists                   ;LOM empty
               (make-game LOI0 empty T0))
              (make-game LOI0 empty T0))

(check-expect (update-lists                   ;missile destroys invader
               (make-game LOI0 (list M1) T0))
              (make-game empty empty T0))
(check-expect (update-lists                   ;missile misses invader
               (make-game LOI0 LOM0 T0))
              (make-game LOI0 LOM0 T0))

(check-expect (update-lists                   ;missile destroys invader
               (make-game LOI0-IRR LOM1 T0))
              (make-game LOIRR LOM0 T0))
(check-expect (update-lists                   ;missile misses invader
               (make-game LOIRL-IRR LOM1 T0))
              (make-game LOIRL-IRR LOM1 T0))
(check-expect (update-lists                   ;missile destroys invader
               (make-game LOIRR-I0 LOM1 T0))
              (make-game LOIRR LOM0 T0))
(check-expect (update-lists                   ;missile misses invader
               (make-game LOIRR-IRL LOM1 T0))
              (make-game LOIRR-IRL LOM1 T0))

(check-expect (update-lists                   ;missile destroys invader
               (make-game LOI0-IRR LOM1M0 T0))
              (make-game LOIRR LOM0 T0))
(check-expect (update-lists                   ;missile misses invader
               (make-game LOIRL-IRR LOM1M0 T0))
              (make-game LOIRL-IRR LOM1M0 T0))
(check-expect (update-lists                   ;missile destroys invader
               (make-game LOIRR-I0 LOM1M0 T0))
              (make-game LOIRR LOM0 T0))
(check-expect (update-lists                   ;missile misses invader
               (make-game LOIRR-IRL LOM1M0 T0))
              (make-game LOIRR-IRL LOM1M0 T0))

;(define (update-lists g) G0) ;stub

;<template from Game>
(define (update-lists g)
  (make-game (destroy-invaders (game-invaders g) (game-missiles g))
             (destroy-missiles (game-missiles g) (game-invaders g))
             (game-tank g)))



;; ListOfInvader ListOfMissile -> ListOfInvader
;; removes invader from list upon missile collision
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders empty LOM0) empty)
(check-expect (destroy-invaders LOI0 empty) LOI0)

(check-expect (destroy-invaders LOI0 (list M1)) empty)        ;missile destroys invader
(check-expect (destroy-invaders LOI0 LOM0) LOI0)              ;missile misses invader

(check-expect (destroy-invaders LOI0-IRR LOM1) LOIRR)         ;missile destroys invader
(check-expect (destroy-invaders LOIRL-IRR LOM1) LOIRL-IRR)    ;missile misses invader
(check-expect (destroy-invaders LOIRR-I0 LOM1) LOIRR)         ;missile destroys invader
(check-expect (destroy-invaders LOIRR-IRL LOM1) LOIRR-IRL)    ;missile misses invader

(check-expect (destroy-invaders LOI0-IRR LOM1M0) LOIRR)       ;missile destroys invader
(check-expect (destroy-invaders LOIRL-IRR LOM1M0) LOIRL-IRR)  ;missile misses invader
(check-expect (destroy-invaders LOIRR-I0 LOM1M0) LOIRR)       ;missile destroys invader
(check-expect (destroy-invaders LOIRR-IRL LOM1M0) LOIRR-IRL)  ;missile misses invader
             
;(define (destroy-invaders loi lom) empty) ;stub
;<template from ListOfInvader>
(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (invader-collision? (first loi) lom)
             (destroy-invaders (rest loi) lom)
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))



;; Invader ListOfMissile -> Boolean
;; returns true if invader collides with any missile in list
(check-expect (invader-collision? I0 empty) false)
(check-expect (invader-collision? I0 LOM0) false)
(check-expect (invader-collision? I0 LOM1) true)
(check-expect (invader-collision? I1 LOM1) false)

;(define (invader-collision? i lom) false) ;stub
;<template from ListOfMissile>
(define (invader-collision? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collision? i (first lom))
             true
             (invader-collision? i (rest lom)))]))



;; Invader Missile -> Boolean
;; returns true if invader and missile collide
(check-expect (collision? I0 M0) false)
(check-expect (collision? I0 M1) true)
(check-expect (collision? I1 M1) false)

;(define (collision? i m) false) ;stub
;<template from Invader>
(define (collision? i m)
  (and
   (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
   (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))



;; ListOfMissile ListOfInvader -> ListOfMissile
;; removes missile from list upon invader collision
(check-expect (destroy-missiles empty empty) empty)
(check-expect (destroy-missiles LOM0 empty) LOM0)
(check-expect (destroy-missiles empty LOI0) empty)

(check-expect (destroy-missiles (list M1) LOI0) empty)     ;missile destroys invader
(check-expect (destroy-missiles LOM0 LOI0) LOM0)           ;missile misses invader

(check-expect (destroy-missiles LOM1 LOI0-IRR) LOM0)       ;missile destroys invader
(check-expect (destroy-missiles LOM1 LOIRL-IRR) LOM1)      ;missile misses invader
(check-expect (destroy-missiles LOM1 LOIRR-I0) LOM0)       ;missile destroys invader
(check-expect (destroy-missiles LOM1 LOIRR-IRL) LOM1)      ;missile misses invader

(check-expect (destroy-missiles LOM1M0 LOI0-IRR) LOM0)     ;missile destroys invader
(check-expect (destroy-missiles LOM1M0 LOIRL-IRR) LOM1M0)  ;missile misses invader
(check-expect (destroy-missiles LOM1M0 LOIRR-I0) LOM0)     ;missile destroys invader
(check-expect (destroy-missiles LOM1M0 LOIRR-IRL) LOM1M0)  ;missile misses invader
              
              
;(define (destroy-missiles loi lom) empty) ;stub
;<template from ListOfInvader>
(define (destroy-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (missile-collision? (first lom) loi)
             (destroy-missiles (rest lom) loi)
             (cons (first lom) (destroy-missiles (rest lom) loi)))]))



;; Missile ListOfInvader -> Boolean
;; returns true if missile collides with any invader in list
(check-expect (missile-collision? M0 empty) false)
(check-expect (missile-collision? M0 LOI0) false)
(check-expect (missile-collision? M1 LOI1) true)
(check-expect (missile-collision? M0 LOI1) false)

;(define (missile-collision? m loi) false) ;stub
;<template from ListOfMissile>
(define (missile-collision? m loi)
  (cond [(empty? loi) false]
        [else
         (if (collision? (first loi) m)
             true
             (missile-collision? m (rest loi)))]))



;; ListOfInvader -> ListOfInvader
;; spawns new invaders and advances invaders
(check-expect (next-loi empty) empty)
(check-expect (next-loi LOI0) LOI0-NEXT)
(check-expect (next-loi
               (list I-REACHES-REDGE                          
                     I0))                                      
              (list I-REACHES-REDGE-NEXT
                    I0-NEXT))
                                     
;(define (next-loi loi) empty) ;stub

(define (next-loi loi)
  (advance-loi (spawn-loi loi)))



;; ListOfInvader -> ListOfInvader
;; advances invaders by one game tick
(check-expect (advance-loi empty) empty)
(check-expect (advance-loi LOI0) LOI0-NEXT)
(check-expect (advance-loi
               (list I-REACHES-REDGE                          
                     I0))                                      
              (list I-REACHES-REDGE-NEXT
                    I0-NEXT))

;(define (advance-loi loi) empty) ;stub
;<template from ListOfInvader>
(define (advance-loi loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invaders (first loi))                       
               (advance-loi (rest loi)))]))



;; Invader -> Invader
;; advances individual invaders downwards, changing x-direction if wall is crossed
(check-expect (advance-invaders I0) I0-NEXT)                             ; away from edge, moving right
(check-expect (advance-invaders I0-NEGATIVE) I0-NEGATIVE-NEXT)           ; away from edge, moving left
(check-expect (advance-invaders I-REACHES-REDGE) I-REACHES-REDGE-NEXT)   ; reaches right edge
(check-expect (advance-invaders I-REACHES-LEDGE) I-REACHES-LEDGE-NEXT)   ; reaches left edge
(check-expect (advance-invaders I-CROSSES-REDGE) I-CROSSES-REDGE-NEXT)   ; crosses right edge
(check-expect (advance-invaders I-CROSSES-LEDGE) I-CROSSES-LEDGE-NEXT)   ; crosses left edge

;(define (advance-invaders i) (make-invader 100 100 10) ;stub

;<template from invader>
(define (advance-invaders i)
  (cond [(> (calc-ix i) WIDTH)
         (make-invader WIDTH
                       (calc-iy i)
                       (- (invader-dx i)))]
        [(< (calc-ix i) 0)
         (make-invader 0
                       (calc-iy i)
                       (- (invader-dx i)))]
        [else
         (make-invader (calc-ix i)
                       (calc-iy i)
                       (invader-dx i))]))



;; Invader -> Number
;; calculates x-position of an invader on next tick
(check-expect (calc-ix I0) (+ 150 (* INVADER-X-SPEED 1)))           ; moving right
(check-expect (calc-ix I0-NEGATIVE) (+ 150 (* INVADER-X-SPEED -1))) ; moving left

;(define (calc-ix i) 0) ;stub

;<template from Invader>
(define (calc-ix i)
  (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))))



;; Invader -> Number
;; calculates y-position of an invader on next tick
(check-expect (calc-iy I0) (+ 100 (* INVADER-Y-SPEED (abs 1))))           ; moving down
(check-expect (calc-iy I0-NEGATIVE) (+ 100 (* INVADER-Y-SPEED (abs -1)))) ; moving down

;(define (calc-iy i) 0) ;stub

;<template from Invader>
(define (calc-iy i)
  (+ (invader-y i) (* INVADER-Y-SPEED (abs (invader-dx i)))))



;; ListOfInvader -> ListOfInvader
;; rolls a random number based on INVADE-RATE, appending an invader with random stats
(check-random (spawn-loi empty)
              (if (= (- INVADE-RATE 1) (random INVADE-RATE))
                  (cons (make-invader (random WIDTH)
                                      0
                                      (+ (random MAX-INVADER-VELOCITY) 1))
                        empty)
                  empty))

(check-random (spawn-loi LOI0)
              (if (= (- INVADE-RATE 1) (random INVADE-RATE))
                  (cons (make-invader (random WIDTH)
                                      0
                                      (+ (random MAX-INVADER-VELOCITY) 1))
                        LOI0)
                  LOI0))

(check-random (spawn-loi LOI1)
              (if (= (- INVADE-RATE 1) (random INVADE-RATE))
                  (cons (make-invader (random WIDTH)
                                      0
                                      (+ (random MAX-INVADER-VELOCITY) 1))
                        LOI1)
                  LOI1))

;(define (spawn-loi loi) empty) ;stub

(define (spawn-loi loi)
  (if (= (- INVADE-RATE 1) (random INVADE-RATE))
      (cons (make-invader (random WIDTH)
                          0
                          (+ (random MAX-INVADER-VELOCITY) 1))
            loi)
      loi))



;; ListOfMissile -> ListOfMissile
;; advance list of missiles by one tick, removing missiles at top of screen
(check-expect (next-lom empty) empty)
(check-expect (next-lom LOM0) LOM0-NEXT)
(check-expect (next-lom LOM1) LOM1-NEXT)
(check-expect (next-lom (list M-REACHING-TOP)) empty)
(check-expect (next-lom (list M-REACHING-TOP M-CROSSING-TOP)) empty)
(check-expect (next-lom (list M0 M1 M-REACHING-TOP M-CROSSING-TOP)) LOM1-NEXT)

;(define (next-lom lom) empty) ;stub

;<template from ListOfMissile>
(define (next-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (top? (first lom))
             (next-lom (rest lom))
             (cons (advance-missile (first lom))
                   (next-lom (rest lom))))]))



;; Missile -> Boolean
;; produces true if missile has reached the top of the screen
(check-expect (top? (make-missile (/ WIDTH 2) (/ HEIGHT 2))) false)
(check-expect (top? (make-missile (/ WIDTH 2) 0)) true)
(check-expect (top? (make-missile (/ WIDTH 2) 1)) false)
(check-expect (top? (make-missile (/ WIDTH 2) -1)) true)

;(define (top? m) false) ;stub

;<template from Missile>
(define (top? m)
  (<= (missile-y m) 0))



;; Missile -> Missile
;; advances individual missiles towards top of screen
(check-expect (advance-missile M0) M0-NEXT)
(check-expect (advance-missile M1) M1-NEXT)

;(define (advance-missile m) (make-missile 100 100)) ;stub

;<template from Missile>
(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; advance tank's x coordinate depending on tank's direction
(check-expect (next-tank T0) T0-NEXT)                            ;away from edge, moving right
(check-expect (next-tank T1) T1-NEXT)                            ;away from edge, moving right
(check-expect (next-tank T2) T2-NEXT)                            ;away from edge, moving left

(check-expect (next-tank T-REACHES-REDGE) T-REACHES-REDGE-NEXT)  ;reaches right edge
(check-expect (next-tank T-REACHES-LEDGE) T-REACHES-LEDGE-NEXT)  ;reaches left edge
(check-expect (next-tank T-CROSSES-REDGE) T-REACHES-REDGE-NEXT)  ;crosses right edge
(check-expect (next-tank T-CROSSES-LEDGE) T-REACHES-LEDGE-NEXT)  ;crosses left edge

;(define (next-tank t) T0) ;stub

;<template from Tank>
(define (next-tank t)
  (cond [(> (calc-tx t) WIDTH) (make-tank WIDTH 1)]
        [(< (calc-tx t) 0) (make-tank 0 -1)]
        [else
         (make-tank (calc-tx t) (tank-dir t))]))



;; Tank -> Number
;; calculates x-position of Tank on next tick
(check-expect (calc-tx T0) (+ (/ WIDTH 2) (* 1 TANK-SPEED)))
(check-expect (calc-tx T2) (+ 50 (* -1 TANK-SPEED)))

;(define (calc-tx t) 0) ;stub

;<template from Tank>
(define (calc-tx t)
  (+ (tank-x t) (* (tank-dir t) TANK-SPEED)))
  


;; Game -> Image
;; render image with tank, missile(s), and invader(s) 
(check-expect (render-game G0)
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-game G1)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-game G2)
              (place-image INVADER 150 100 
                           (place-image MISSILE 150 300 
                                        (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))

(check-expect (render-game G3)
              (place-image INVADER 150 100 
                           (place-image INVADER 150 HEIGHT 
                                        (place-image MISSILE 150 300 
                                                     (place-image MISSILE 150 110 
                                                                  (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

;(define (render-game g) BACKGROUND) ;stub

;<template from Game>
(define (render-game g)
  (render-loi (game-invaders g)
              (render-lom (game-missiles g)
                          (render-tank (game-tank g)))))



;; ListOfInvader Image -> Image
;; renders list of invaders on background, tank, and missile(s)
(check-expect (render-loi empty
                          (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-loi LOI0
                          (place-image MISSILE 150 300 
                                       (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))
              (place-image INVADER 150 100 
                           (place-image MISSILE 150 300 
                                        (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))

(check-expect (render-loi LOI1
                          (place-image MISSILE 150 300 
                                       (place-image MISSILE 150 110 
                                                    (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
              (place-image INVADER 150 100 
                           (place-image INVADER 150 HEIGHT 
                                        (place-image MISSILE 150 300 
                                                     (place-image MISSILE 150 110 
                                                                  (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))))) 
                          
;(define (render-loi loi img) BACKGROUND) ;stub

;<template from ListOfInvader>
(define (render-loi loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                      (render-loi (rest loi) img))]))



;; ListOfMissile Image -> Image
;; renders list of missiles on background and tank
(check-expect (render-lom empty
                          (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-lom LOM0
                          (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              (place-image MISSILE 150 300 
                           (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))

(check-expect (render-lom LOM1
                          (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              (place-image MISSILE 150 300 
                           (place-image MISSILE 150 110 
                                        (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))
                          
;(define (render-lom lom img) BACKGROUND) ;stub

;<template from ListOfMissile>
(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                      (render-lom (rest lom) img))]))



;; Tank -> Image
;; renders tank on background
(check-expect (render-tank T0)
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-tank T1)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              
;(define (render-tank t) BACKGROUND) ;stub

;<template from Tank>
(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))



;; Game -> Boolean
;; produce true if invader has reached the bottom of the screen
(check-expect (stop-game? G0) false)
(check-expect (stop-game? G3) true)

;(define (stop-game? g) false) ;stub

;<template from Game>
(define (stop-game? g)
  (landed? (game-invaders g)))



;; ListOfInvader -> Boolean
;; produce true if invader in list has landed
(check-expect (landed? empty) false)
(check-expect (landed? LOI0) false)
(check-expect (landed? (list I1)) true)
(check-expect (landed? LOI1) true)
(check-expect (landed? LOIRR-IRL) false)

;(define (landed? loi) false) ;stub

;<template from ListOfInvader>
(define (landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (landed? (rest loi)))]))



;; Game KeyEvent -> Game
;; controls tank's movement and missile firing via left/right and space, respectively
(check-expect (handle-key G0 " ")                                 ;T0 fires a missile                           
              (make-game empty LOMT0 T0))

(check-expect (handle-key (make-game empty LOMT0 T0) " ")         ;T0 fires a redundant missile 
              (make-game empty (list MT0 MT0) T0))

(check-expect (handle-key G0 "right") G0)                         ;T0 continues right

(check-expect (handle-key                                         ;T2 changes to right
               (make-game empty empty T2) "right")
              (make-game empty empty (make-tank 50 1)))

(check-expect (handle-key G0 "left")                              ;T0 changes to left
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))

(check-expect (handle-key                                         ;T2 continues left
               (make-game empty empty T2) "left")
              (make-game empty empty T2))

(check-expect (handle-key G0 "a") G0)                             ;nothing changes

(check-expect (handle-key (make-game empty LOMT0 T0) "a")         ;nothing changes
              (make-game empty LOMT0 T0))

;(define (handle-key g ke) 0) ;stub

;<template from Game>
(define (handle-key g ke)
  (cond [(key=? ke " ") (add-missile g)]
        [(and (key=? ke "right") (moving-left? g)) (move-right g)]
        [(and (key=? ke  "left") (moving-right? g)) (move-left g)]
        [else g]))


;; Game -> Game
;; adds Missile to ListOfMissile
(check-expect (add-missile G0) (make-game empty LOMT0 T0))
(check-expect (add-missile (make-game empty LOMT0 T0))
              (make-game empty (list MT0 MT0) T0))

;(define (add-missile g) G0) ;stub

;<template from Game>
(define (add-missile g)
  (make-game (game-invaders g)
             (cons (make-missile
                    (tank-x (game-tank g))
                    (- HEIGHT TANK-HEIGHT/2))
                   (game-missiles g))
             (game-tank g)))



;; Game -> Game
;; moves Tank left
(check-expect (move-left G0)
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))

(check-expect (move-left
               (make-game empty empty T2))
              (make-game empty empty T2))

;(define (move-left g) G0) ;stub
;<template from Game>
(define (move-left g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) -1)))



;; Game -> Game
;; moves Tank right
(check-expect (move-right G0) G0)                         ;T0 continues right

(check-expect (move-right                                 ;T2 changes to right
               (make-game empty empty T2))
               (make-game empty empty (make-tank 50 1)))

;(define (move-right g) G0) ;stub

;<template from Game>
(define (move-right g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) 1)))



;; Game -> Boolean
;; produces true if Tank is moving left
(check-expect (moving-left? G0) false)
(check-expect (moving-left?
               (make-game empty empty T2))
              true)
 
;(define (moving-left? G0) false) ;stub

;<template from Game>
(define (moving-left? g)
  (= -1 (tank-dir (game-tank g))))



;; Game -> Boolean
;; produces true if Tank is moving right
(check-expect (moving-right? G0) true)
(check-expect (moving-right?
               (make-game empty empty T2))
              false)

;(define (moving-right? G0) true) ;stub

;<template from Game>
(define (moving-right? g)
  (=  1 (tank-dir (game-tank g))))