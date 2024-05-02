;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
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



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions

;; Game -> Game
;; start the world with instance of Game
;;     Game includes list of invaders, bullets, and a tank object
;; 
(define (main g)
  (big-bang g                                            ; Game
            (on-tick   next-game)                        ; Game -> Game
            (to-draw   render-game)                      ; Game -> Image
            (stop-when stop-game?)                       ; Game -> Boolean
            (on-key    handle-key)))                     ; Game KeyEvent -> Game


;; NEXT-GAME
;; Game -> Game
;; updates locations of bullets, invaders, and tank.
(define (next-game gs)
  (cond [(< (random INVADE-RATE) 2)
         (make-game
          (append (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))
                  (new-invaders (game-invaders gs) (game-missiles gs)))
          (new-missiles (game-missiles gs))
          (new-tank (game-tank gs)))]
        [else
         (make-game (new-invaders (game-invaders gs) (game-missiles gs))
                    (new-missiles (game-missiles gs))
                    (new-tank (game-tank gs)))]))

;; NEW-INVADERS
;; List of invaders -> List of invaders
;; updates location of active invaders
;<template from Invader>
(define (new-invaders loi lom)
  (cond [(empty? loi) empty]
        [(invader-collision? (first loi) lom) (new-invaders (rest loi) lom)]
        [else
         (append (list (new-invader (first loi)))
                 (new-invaders (rest loi) lom))]))

;; NEW-INVADER
;; Invader -> Invader
;; produces updated invader
(define (new-invader i)
  (cond [(< 0 (+ (invader-x i) (invader-dx i)) WIDTH)
         (make-invader
          (+ (invader-x i) (invader-dx i))
          (+ (invader-y i) (abs (invader-dx i)))
          (invader-dx i))]
        [(<= (+ (invader-x i) (invader-dx i)) 0)
         (make-invader 0 (invader-y i) (* -1 (invader-dx i)))]
        [(<= WIDTH (+ (invader-x i) (invader-dx i)))
         (make-invader WIDTH (invader-y i) (* -1 (invader-dx i)))]))

;; INVADER-COLLISION
;; Invader (listof Missile) -> Boolean
;; produces true if given invader is within hit range
;; of any one of given missiles, false otherwise
(check-expect (invader-collision? I1 (list M1)) false)
(check-expect (invader-collision? I1 (list M1 M2)) true)
;(define (invader-collision? i lom) false) ;stub
(define (invader-collision? i lom)
  (cond [(empty? lom) false]
        [(and
          (<= (- (invader-x i) HIT-RANGE)
              (missile-x (first lom))
              (+ (invader-x i) HIT-RANGE))
          (<= (- (invader-y i) HIT-RANGE)
              (missile-y (first lom))
              (+ (invader-y i) HIT-RANGE)))
         true]
        [else (invader-collision? i (rest lom))]))



;; NEW-MISSILES
;; List of missiles -> List of missiles
;; updates location of active missiles
(check-expect (new-missiles (cons (make-missile 50 50) empty)) (cons (make-missile 50 (- 50 MISSILE-SPEED)) empty))
;(define (new-missiles m) m) ;stub
;; <Used template for Missile>
(define (new-missiles lst)
  (cond [(empty? lst) empty]
        [else (append (list (make-missile
                        (missile-x (first lst))
                        (- (missile-y (first lst)) MISSILE-SPEED)))
                 (new-missiles (rest lst)))]))


;; NEW-TANK
;; Tank -> Tank
;; updates location of tank if tank does not collide with either side of the window
(check-expect(new-tank T0) (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))
(check-expect(new-tank T1) (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1)))
(check-expect(new-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))
;(define (new-tank t) t) ;stub
;;Template taken from Tank
(define (new-tank t)
  (cond
    [(and (= (tank-dir t) 1) (< (tank-x t) (- WIDTH (/ (image-width TANK) 2))))(make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]
    [(and (= (tank-dir t) -1) (> (tank-x t) (+ 0 (/ (image-height TANK) 2))))(make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]
    [else t]
  ))


;; RENDER-GAME
;; Game -> Image
;; renders game with all objects
;;(define (render-game g) BACKGROUND) ; stub
;; Used template from Game
(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g) BACKGROUND))))



;; RENDER-TANK
;; Tank Image -> Image
;; Renders tank on BACKGROUND
(check-expect (render-tank T0 BACKGROUND) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank T1 BACKGROUND) (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank T2 BACKGROUND) (place-image TANK (tank-x T2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
;(define (render-tank t) BACKGROUND) ;stub
;; Used template from Tank
(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))


;; RENDER-INVADERS
;; (listOfInvader) Image -> Image
;; Renders invaders onto image
(check-expect (render-invaders (game-invaders G0) BACKGROUND) BACKGROUND)
(check-expect (render-invaders (game-invaders G1) BACKGROUND) BACKGROUND)
(check-expect (render-invaders (game-invaders G2) BACKGROUND)
              (place-image INVADER
                           (invader-x (first (game-invaders G2)))
                           (invader-y (first (game-invaders G2)))
                           (render-invaders (rest (game-invaders G2))
                                            BACKGROUND)))
(check-expect (render-invaders (game-invaders G3) BACKGROUND)
              (place-image INVADER
                           (invader-x (first (game-invaders G3)))
                           (invader-y (first (game-invaders G3)))
                           (render-invaders (rest (game-invaders G3))
                                            BACKGROUND)))
;(define (render-invaders loi img) img) ; stub
; <Template from Invader>
(define (render-invaders lst img)
  (cond [(empty? lst) img]
        [else
         (place-image INVADER
                      (invader-x (first lst))
                      (invader-y (first lst))
                      (render-invaders(rest lst) img))]))


;; RENDER-MISSILES
;; (listOfMissiles) Image -> Image
;; Renders missiles onto image
(check-expect (render-missiles (game-missiles G2) BACKGROUND)
              (place-image MISSILE
                           (missile-x (first (game-missiles G2)))
                           (missile-y (first (game-missiles G2)))
                           (render-missiles (rest (game-missiles G2))
                                            BACKGROUND)))
;(define (render-missiles lst img) img) ; stub
; <Used template from Missile>
(define (render-missiles lst img)
  (cond
    [(empty? lst) img]
    [else
     (place-image
      MISSILE
      (missile-x (first lst))
      (missile-y (first lst))
      (render-missiles (rest lst) img)
      )]))

;; Game, KeyEvent -> Game
;; shoots missile or changes direction of tank if applicable
;(define (handle-key g ke) g) ; stub
;; <use template for large enumerations>
(define (handle-key g ke)
  (cond [(key=? " " ke) (fire-missile g)]
        [(key=? "left"  ke) (move-tank g "left")]
        [(key=? "right" ke) (move-tank g "right")]
        [else g]))


;; MOVE-TANK
;; Game String -> Game
(check-expect
 (move-tank (make-game empty empty T0) "left")
 (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect
 (move-tank (make-game empty empty T0) "right")
 (make-game empty empty (make-tank (tank-x T0) (tank-dir T0))))
;(define (move-tank g dir) g) ; stub
;; <Used template for Game>
(define (move-tank g dir)
  (if (equal? dir "left")
      (make-game (game-invaders g)(game-missiles g)(make-tank (tank-x (game-tank g)) -1))
      (make-game (game-invaders g)(game-missiles g)(make-tank (tank-x (game-tank g)) 1))))


;; FIRE-MISSILE
;; Game -> Game
(check-expect
 (fire-missile (make-game empty empty T0))
               (make-game empty
                          (cons (make-missile (tank-x T0) (- HEIGHT TANK-HEIGHT/2)) empty)
                          T0))
;(define (fire-missile g) g) ; stub
;; <use template for Game>
(define (fire-missile g)
  (make-game (game-invaders g)
       (append (game-missiles g)
                     (list (make-missile
                            (tank-x (game-tank g))
                            (- HEIGHT TANK-HEIGHT/2))))
       (game-tank g)))




;; Game -> Boolean
;; ends game when invader reaches y-level 0
;(define (stop-game? g) false) ;stub
(define (stop-game? g)
  (cond[(empty? (game-invaders g)) false]
  [else 
         (cond [(<= HEIGHT (invader-y (first (game-invaders g)))) true]
               [else (stop-game? (make-game
                                 (rest (game-invaders g))
                                 (game-missiles g)
                                 (game-tank g)))])]))



;; Sample starter method:
(main(make-game empty empty (make-tank 50 1)))