(ns starter.quil
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [starter.util :as u]
            [starter.fxhash :as f]
            [clojure.string :as s]))

(defn get-viewport
  "Returns a vector of [vw vh] for the current browser window, scaled by the scale parameter.
   If true is passed to force-square the smaller dimension will be used on both axes."
  [scale force-square]
  (if force-square
    (let [dim (min (* scale (.-innerWidth js/window)) (* scale (.-innerHeight js/window)))]
      [dim dim])
    [(* scale (.-innerWidth js/window)) (* scale (.-innerHeight js/window))]))

(def dims (get-viewport 1 true))

(defn x
  "Return t% of viewport width, allows for resolution independent drawing i.e. 10px in a 100px window = (x 10) = 25px in a 250px window"
  [t]
  (let [[w _] dims] (* t (/ w 100))))

(defn y
  "Return t% of viewport height, allows for resolution independent drawing i.e. 10px in a 100px window = (x 10) = 25px in a 250px window"
  [t]
  (let [[_ h] dims] (* t (/ h 100))))


(defn hex->rgb [hex]
  (let [hex (s/replace hex #"[^0-9A-Fa-f]" "")
        bigint (js/parseInt hex 16)
        r (bit-and (bit-shift-right bigint 16) 255)
        g (bit-and (bit-shift-right bigint 8) 255)
        b (bit-and bigint  255)]

    [r g b]))

(defn update-state [state]
  (merge state {:t (inc (:t state))}))

(def palette [(hex->rgb "#1B3536")
              (hex->rgb "#CB4437")
              (hex->rgb "#EDC11D")
              (hex->rgb "#EBE3D3")
              (hex->rgb "#EDA8C4")
              (hex->rgb "#1792C6")
              (hex->rgb "#8F908F")
              (hex->rgb "#BEBAAE")
              (hex->rgb "#EFC987")])

(def palette2 [(hex->rgb "#1F2526")
               (hex->rgb "#B29543")
               (hex->rgb "#6294A1")
               (hex->rgb "#CBD9E6")
               (hex->rgb "#A9A47D")
               (hex->rgb "#52606A")
               (hex->rgb "#91B3C2")
               (hex->rgb "#8D99A9")
               (hex->rgb "#3A7D8C")])

(def palette3 [(hex->rgb "#924D1B")
               (hex->rgb "#085266")
               (hex->rgb "#046DC7")
               (hex->rgb "#EEAD24")
               (hex->rgb "#F13931")
               (hex->rgb "#148C73")
               (hex->rgb "#91705E")
               (hex->rgb "#0A2846")
               (hex->rgb "#C4445C")])

(def palette4 [(hex->rgb "#040404")
               (hex->rgb "#080404")
               (hex->rgb "#D098A4")
               (hex->rgb "#0AB21F")
               (hex->rgb "#5454FC")
               (hex->rgb "#AC0404")
               (hex->rgb "#040444")
               (hex->rgb "#044440")
               (hex->rgb "#444040")])

(def palette5 [(hex->rgb "#98ADD6")
               (hex->rgb "#0B0E19")
               (hex->rgb "#6C7BAB")
               (hex->rgb "#404A74")
               (hex->rgb "#272F4E")
               (hex->rgb "#8C949C")
               (hex->rgb "#C4C5A")
               (hex->rgb "#4C545B")
               (hex->rgb "#8C8C94")])


(def palette6 [(hex->rgb "#BF8AA4")
               (hex->rgb "#050405")
               (hex->rgb "#5D5057")
               (hex->rgb "#678DE9")
               (hex->rgb "#63689C")
               (hex->rgb "#2E2326")
               (hex->rgb "#2D2F52")
               (hex->rgb "#394666")
               (hex->rgb "#88353B")])

(def palette7 [(hex->rgb "#46B2BF")
               (hex->rgb "#B49F8D")
               (hex->rgb "#070D11")
               (hex->rgb "#296BA4")
               (hex->rgb "#65453E")
               (hex->rgb "#1A616A")
               (hex->rgb "#A23515")
               (hex->rgb "#13305E")
               (hex->rgb "#361C1B")])

(def palette8 [(hex->rgb "#130A0C")
               (hex->rgb "#EE17ED")
               (hex->rgb "#549199")
               (hex->rgb "#615F52")
               (hex->rgb "#473739")
               (hex->rgb "#914484")
               (hex->rgb "#494741")
               (hex->rgb "#5C847C")
               (hex->rgb "#3E2544")])

(def palettes [palette palette2 palette3 palette4 palette5 palette6 palette7 palette8])

(def PI (.-PI js/Math))


(defn choose [from]
  (get from (q/floor (* (f/fx-rand) (count from)))))

(defn setup []
  ;; EXAMPLE CODE, DO NOT HARDCODE YOUR FEATURES
  (f/register-features {:dark true :another-feature "yes"})

  (q/frame-rate 30)

  ; setup function returns initial state
  {:t 0
   :alpha (f/fx-rand)
   :beta (f/fx-rand)
   :gamma (f/fx-rand)
   :pal (shuffle (choose palettes))})

(defn rand-range [min max]
  (+ min (* (- max min) (f/fx-rand))))

(defn draw-state [{:keys [t alpha beta gamma pal]}]
  (let [t' (/ t 10)
        rw (* (x (* 50 gamma (q/sin t))) (q/noise (* t gamma) (f/fx-rand) (f/fx-rand)))
        rh (* (y (* 50 beta (q/cos t))) (q/noise (* t alpha) (f/fx-rand) (f/fx-rand)))
        rx (+ 20 (* (x 80) (q/noise (* t alpha) beta (f/fx-rand))) (x 0))
        ry (+ 20 (* (y 80) (q/noise (* t beta) (f/fx-rand) gamma)) (y 0))
        count (* (+ 4 (* beta 30)) (q/noise (* t alpha) gamma beta))]
    ; clear screen
    (when (= t 0)
      (q/fill 255 255 255)
      (q/stroke-weight 0)
      (let [[r g b] (choose pal)]
        (q/background r g b 255)))


    (when (< t (+ 100 (q/round (* gamma 500))))
      (q/with-translation [0 0]
        (q/with-rotation [0]
          (doseq [i (range (q/round count))]
            (q/rotate (/ PI (q/round (+ 1 (* 5 beta)))))
            (let [col (get pal (/ i 2))
                  [width height] dims]
              (when (some? col)
                (apply q/fill col))
              (q/stroke-weight (+ 1 (* 6 alpha)))
              (apply q/stroke (get pal 0))
              (q/rect (+ (* (q/sin (/ i 2)) 100) (* width beta (q/atan2 rx ry))) (+ (/ ry rx) ry (* i (f/fx-rand) rh)) rw rh))))))



    ; fxhash functions
    ;; (q/text (str "fxhash=" (f/fx-hash)) (x 2) (y 2))
    ;; (q/text (str "fxrand=" (f/fx-rand)) (x 2) (y 4))
    ; draw resolution independent circle
    ))

; this function is called in index.html
(defn ^:export run-sketch []
  (q/defsketch fxhash
    :host "app"
    :size dims
    :renderer :p2d
    :setup setup
    :update update-state
    :draw draw-state
    ;; :key-pressed (u/save-image "export.png")
    :middleware [m/fun-mode]))

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start"))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (run-sketch)
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))
