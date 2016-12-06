;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; InDyTeRRoD
;; InDyTeRRoD (Industrial Dynamics, Technological Regimes and the Role of Demand)
;; is an evolutionary model designed to explore industrial
;; dynamics in alternative technological regimes.
;; Copyright (C) 2011 Isabel Almudi, Francisco Fatas & Luis R. Izquierdo
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es

breed [firms firm]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals [
  list-of-firms
  avg-competitiveness
  max-profit
  max-profit-r&d

  herfindahl-index

  max-performance


  ;; For reporters:
  accumulated-entrants
  leader
  accumulated-leadership-shifts
  market-share-of-leader
  diff-leader
  performance-of-leader
]


firms-own [

  capital
  age
  performance
  r&d-over-profit

  production
  cost

  market-share
  price

  profit

  r&d-expenditure
  r&d-expenditure-next-period
  r&d-productivity

  competitiveness

]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to startup
  clear-all
  setup-firms
  my-setup-plots
  setup-variables
  reset-ticks
  no-display
end


to setup-firms
  create-ordered-firms initial-n-of-firms [
    set capital (1 / initial-n-of-firms)
    set age 0
    set performance initial-performance
    set r&d-over-profit initial-r&d-over-profit
  ]
  set list-of-firms (sort firms)
end


to my-setup-plots
  setup-histogram "Market Share" [0.3 0.4]
  setup-histogram "Age distribution" [0 1]
  setup-histogram "Relative Performance" [0 1.1]
  setup-histogram "Price" [1 1.1]
  setup-histogram "Competitiveness" [-0.1 0.1]
  setup-histogram "R&D over profits" (list initial-r&d-over-profit (initial-r&d-over-profit + 0.0001))
end


to setup-histogram [name x-range]
  set-current-plot name
  auto-plot-on
  set-plot-x-range (first x-range) (last x-range)
    ;; remember that values equal to the maximum x-range will fall outside of the histogram's range
  set-histogram-num-bars 20
  set-plot-y-range 0 0.1
end

to setup-variables
  set plot-period ifelse-value (plot-period <= 1)
    [1] [floor plot-period]
  set accumulated-entrants 0
  set accumulated-leadership-shifts 0
  if (report-from-time < 1) [set report-from-time 1]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN-TIME PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;; Main procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;

to go

  ask firms [set r&d-expenditure r&d-expenditure-next-period]
  ask firms [update-production]
  do-market-shares
  ask firms [update-cost]
  ask firms [update-price]
  do-profits

  ask firms [update-r&d-expenditure-next-period]
  do-r&d-productivity

  do-competitiveness

  with-local-randomness [ask firms [set performance (performance / max-performance)]]

  ask firms [set age (age + 1)]
  if entry-barriers < 1 [ do-new-entrant ]

  tick

  if (ticks >= report-from-time) [update-reporters]
    ;; Updates the reporters that are calculated from tick "report-from-time"

  with-local-randomness [if ticks mod plot-period = 0 [do-plots]]

  ask firms [update-performance]
  do-r&d-over-profits
  do-capitals

  check-deaths
end



to update-production
  set production capital
end

to do-market-shares
  let total-production (sum [production] of firms)
  ask firms [ set market-share (production / total-production) ]
  set herfindahl-index (sum [market-share ^ 2] of firms)
end

to update-cost
  set cost 1 + (r&d-expenditure / production)
end

to update-price
  set price cost * (1 + market-share)
end

to do-profits
  ask firms [update-profit]

  let max-profit-firm max-one-of firms [profit]
    ;; note that if there's a tie, one of the firms
    ;; with the maximum profit will be chosen at random
  set max-profit-r&d ([r&d-over-profit] of max-profit-firm)
  set max-profit ([profit] of max-profit-firm)
end

to update-profit
  set profit (market-share * cost)
end

to update-r&d-expenditure-next-period
 set r&d-expenditure-next-period (r&d-over-profit * profit * production)
end

to do-r&d-productivity
  set max-performance (max [performance] of firms)

  ask firms [
   set r&d-productivity (knowledge-spillover-rate * (max-performance - performance) / performance) + (triangular-dist 0 u-max 0)
  ]
end

to check-deaths
  ask firms [
    if capital < min-capital [
      set list-of-firms remove self list-of-firms
      die
    ]
  ]
  let sum-of-capitals (sum [capital] of firms)
  ask firms [set capital (capital / sum-of-capitals)]
end

to do-competitiveness
  let avg-performance mean ([performance] of firms)
  let avg-price mean ([price] of firms)

  ask firms [
    set competitiveness  (
      (1 - price/performance-sensitivity) * ((performance - avg-performance) / avg-performance)
      - (price/performance-sensitivity * ((price - avg-price) / avg-price))
      )
  ]

  set avg-competitiveness sum [market-share * competitiveness] of firms
end

to update-performance
  set performance (performance + (performance * r&d-productivity * r&d-expenditure))
end

to do-r&d-over-profits
  ask firms [
    set r&d-over-profit
        clip (r&d-over-profit + learning-rate * (clip (max-profit-r&d + (random-normal 0 sigma)) - r&d-over-profit))
  ]
end

to do-capitals
  ask firms [
    set capital (capital + capital * (competitiveness - avg-competitiveness))
    if capital < 0 [set capital 0]
  ]

  let sum-of-capitals (sum [capital] of firms)
  ask firms [set capital (capital / sum-of-capitals)]
end

to do-new-entrant
  if random-float 1.0 < (1 - entry-barriers) * (max-profit / (1 + max-profit)) [
    let rd-capital (random-float max-entry-capital)
    let sum-of-capitals ((sum [capital] of firms) + rd-capital)

    ask firms [set capital (capital / sum-of-capitals)]
    let new-capital (rd-capital / sum-of-capitals)

    let parent (select-one-firm-proportional-to-market-shares)

    ask parent [
      hatch-firms 1 [
        ;; will copy performance, r&d-over-profit, and r&d-productivity.

        set capital new-capital
        set age 0
        set r&d-expenditure 0
        set production new-capital
        update-cost
        set market-share new-capital
        update-price
        update-profit
        set r&d-expenditure-next-period (r&d-over-profit * profit * production)

        set list-of-firms lput self list-of-firms
      ]
    ]

    if (ticks >= (report-from-time - 1)) [set accumulated-entrants (accumulated-entrants + 1)]

    do-market-shares
    do-competitiveness
  ]

end

to-report select-one-firm-proportional-to-market-shares

  let cum-shares [0]

  let ordered-market-shares (map [[market-share] of ?] list-of-firms)
    ;; cum-shares starts at 0 and is one item longer than num-firms
  foreach ordered-market-shares [set cum-shares lput (? + last cum-shares) cum-shares]

    ;; select the firm to replicate with probability proportional to market share
  let rand random-float (last cum-shares)                                  ;; to select the one to clone
  let firm-to-clone-pos length (filter [? <= rand] but-first cum-shares)   ;; select the to-clone position in the list of firms
  report item firm-to-clone-pos list-of-firms   ;; select and report the firm to clone
end


;;;;;;;;;;;;;;;;;;;;;
;;;   Reporters   ;;;
;;;;;;;;;;;;;;;;;;;;;

to update-reporters ;; The following reporters are updated since tick "report-every-tick-from" - 1

  let market-shares (map [[market-share] of ?] list-of-firms)
  let sorted-market-shares (sort-by [?1 > ?2] market-shares)
  let max-market-share (first sorted-market-shares)
  set diff-leader ifelse-value ((count firms) > 1)
  [(max-market-share - (item 1 sorted-market-shares))]
  [1]

  if (ticks = report-from-time) [
    set leader max-one-of firms [market-share]
  ]

  ifelse leader != nobody
  [ set market-share-of-leader ([market-share] of leader)    set performance-of-leader ([performance] of leader)]
  [ set market-share-of-leader 0 ]

  ;; this is done to avoid computing unneccessary leadership shifts when there are ties.
  if market-share-of-leader < max-market-share [
    set accumulated-leadership-shifts (accumulated-leadership-shifts + 1)
    set leader max-one-of firms [market-share]
    set market-share-of-leader ([market-share] of leader)
    set performance-of-leader ([performance] of leader)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Plots       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to do-plots


  set-current-plot "Herfindahl index"
  plotxy ticks herfindahl-index

  set-current-plot "Market Share"
  histogram normalised-histogram-of (map [[market-share] of ?] list-of-firms) 1

  set-current-plot "Age distribution"
  histogram normalised-histogram-of ([age] of firms) 0

  set-current-plot "Relative Performance"
  histogram normalised-histogram-of (map [[performance] of ?] list-of-firms) 2

  set-current-plot "Price"
  histogram normalised-histogram-of (map [[price] of ?] list-of-firms) 1

  set-current-plot "Competitiveness"
  histogram normalised-histogram-of (map [[competitiveness] of ?] list-of-firms) 1

  set-current-plot "Number of firms"
  plotxy ticks (count firms)

  set-current-plot "R&D over profits"
  histogram normalised-histogram-of (map [[r&d-over-profit] of ?] list-of-firms) 1

  set-current-plot "Firms' life"
  ask firms [plotxy ticks who]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Supporting procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report triangular-dist [minimum maximum mode]
  let my-rd-number (random-float 1.0)

  if (maximum = minimum) [report maximum]

  report ifelse-value (my-rd-number < ((mode - minimum) / (maximum - minimum)))
         [ minimum + sqrt (my-rd-number * (maximum - minimum) * (mode - minimum)) ]
         [ maximum - sqrt ((1 - my-rd-number) * (maximum - minimum) * (maximum - mode)) ]
end

to-report clip [number]
  if number > 1 [ report 1 ]
  if number < 0 [ report 0 ]
  report number
end

to-report normalised-histogram-of [l d]
  let min-value (min l)
  let max-value (max l)

  ;; update the range of the histogram to show all the values.
  ;; this is done by rounding up the maximum to d digits
  ;; and rounding down the minimum to d digits
  if min-value < plot-x-min [set-plot-x-range (precision (min-value - (10 ^ (- d)) / 2) d) plot-x-max]
  if max-value > plot-x-max [set-plot-x-range plot-x-min (precision (max-value + (10 ^ (- d)) / 2) d)]

  set-histogram-num-bars 20
  set-plot-y-range 0 (count firms)

  report l
end
@#$#@#$#@
GRAPHICS-WINDOW
868
10
1113
204
1
1
54.33333333333334
1
17
1
1
1
0
0
0
1
-1
1
-1
1
0
0
1
time
30.0

SLIDER
7
182
230
215
price/performance-sensitivity
price/performance-sensitivity
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
7
229
142
262
learning-rate
learning-rate
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
144
229
279
262
sigma
sigma
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
7
276
224
309
knowledge-spillover-rate
knowledge-spillover-rate
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
5
50
177
83
initial-n-of-firms
initial-n-of-firms
1
200
1
1
1
NIL
HORIZONTAL

SLIDER
5
87
177
120
initial-performance
initial-performance
0
2
1
0.05
1
NIL
HORIZONTAL

SLIDER
5
124
177
157
initial-r&d-over-profit
initial-r&d-over-profit
0
1
0.3
0.01
1
NIL
HORIZONTAL

BUTTON
5
10
74
43
Setup
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
93
10
177
43
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
191
10
271
43
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
283
10
568
206
Herfindahl index
time
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
571
404
843
613
Relative Performance
time
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"default" 1.0 1 -16777216 false "" ""

PLOT
283
404
568
613
Market share
time
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"default" 1.0 1 -16777216 false "" ""

PLOT
571
208
843
401
Price
time
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 1 -16777216 false "" ""

PLOT
571
10
843
206
Competitiveness
time
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 1 -16777216 false "" ""

PLOT
283
208
568
401
Number of firms
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 false "" ""

PLOT
846
447
1127
613
R&D over profits
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 1 -16777216 false "" ""

SLIDER
7
311
224
344
u-max
u-max
0
1
0.5
0.01
1
NIL
HORIZONTAL

INPUTBOX
191
101
270
161
plot-period
1
1
0
Number

SLIDER
154
397
279
430
min-capital
min-capital
0
0.1
1.0E-6
0.000001
1
NIL
HORIZONTAL

SLIDER
7
397
151
430
max-entry-capital
max-entry-capital
0
0.5
0.05
0.01
1
NIL
HORIZONTAL

PLOT
846
275
1127
444
Age distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

PLOT
846
10
1127
273
Firms' life
time
id
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

SLIDER
7
361
230
394
entry-barriers
entry-barriers
0
1
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
10
568
120
613
entrants
accumulated-entrants
0
1
11

MONITOR
9
519
120
564
leadership shifts
accumulated-leadership-shifts
0
1
11

MONITOR
127
568
266
613
mk. sh. edge of leader
diff-leader
3
1
11

INPUTBOX
7
452
120
512
report-from-time
1
1
0
Number

MONITOR
127
520
266
565
market share of leader
market-share-of-leader
3
1
11

MONITOR
191
50
271
95
time
ticks
0
1
11

MONITOR
127
471
266
516
rel. perf. of leader
performance-of-leader
3
1
11

@#$#@#$#@
## WHAT IS IT?

This section could give a general understanding of what the model is trying to show or explain.

## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
