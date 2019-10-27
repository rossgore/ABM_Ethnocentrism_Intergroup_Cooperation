;; This version runs with NetLogo 6.0.1

globals     [ origins      ;; "Tag" for agents (currently "Host" and "Immigrant"
              languages    ;; "Languages" (currently "Host", "Immigrant" and "Pivot")
              R S T P      ;;  Entries in game matrix, A = [ [R S] [T P] ]
            ]

turtles-own [ ;; origin/ethnicity
              origin

              ;; Language proficiency
              langLevelHost
              langLevelPivot
              langLevelImmigrant

              ;; Communality
              payoff

              ;;;; Likenesses (individual)
              likenessOwn
              likenessOther
              ;;;;

              ;; Strategies
              strategyOwn
              strategyOther

              ;; The next variables were introduced for post-processing with RNetLogo
              numSuccessfulInteractions   ;; # successful interactions
              numUnsuccessfulInteractions ;; # unsuccessful interactions
              numHostNeighbors            ;; # host neighbors
              numImmigrantNeighbors       ;; # immigrant neighbors
              numHostCC                   ;; # "host" neighbors with C/C
              numHostCD                   ;; # "host" neighbors with C/D
              numHostDC                   ;; # "host" neighbors with D/C
              numHostDD                   ;; # "host" neighbors with D/D
              numImmigrantCC              ;; # "immigrant" neighbors with C/C
              numImmigrantCD              ;; # "immigrant" neighbors with C/D
              numImmigrantDC              ;; # "immigrant" neighbors with D/C
              numImmigrantDD              ;; # "immigrant" neighbors with D/D
              avgLangHostHost             ;; avg langLevel "host" neigh. on "host" lang
              avgLangHostImmigrant        ;; avg langLevel "host" neigh. on "immigrant" lang
              avgLangHostPivot            ;; avg langLevel "host" neigh. on "pivot" lang
              avgLangImmigrantHost        ;; avg langLevel "immigrant" neigh. on "host" lang
              avgLangImmigrantImmigrant   ;; avg langLevel "immigrant" neigh. on "immigrant" lang
              avgLangImmigrantPivot       ;; avg langLevel "immigrant" neigh. on "pivot" lang
            ]

links-own   [ rewired?      ;; used for generating "small-world" network
              understand?
              like?
            ]


to setup

  clear-all
  ;random-seed 1234567890

  ;; Game matrix, set for PD
  set R 1
  set P 0
  set T (1 + gamma)
  set S (- gamma)

  ;; Create agents list
  let propHost 1 - propImmigrant
  set origins [ "host" "immigrant" ]
  set languages [ "host" "immigrant" "pivot" ]
  let numImmigrant round (propImmigrant * numAgents)
  let numHost (numAgents - numImmigrant)

  crt numHost [
    move-to one-of patches with [not any? turtles-here]
    set origin "host"
    set payoff 0.5

    ;;;;
    set likenessOwn random-float likenessHH
    set likenessOther random-float likenessHI
    ;;;;

    ifelse random-float 1 < likenessOwn ;; use "likeness" to initialize
      [ set strategyOwn "C" ]            ;; in-group strategy
      [ set strategyOwn "D" ]
    ifelse random-float 1 < likenessOther ;; use "likeness" to initialize
      [ set strategyOther "C" ]          ;; out-group strategy
      [ set strategyOther "D" ]

    set size 4
    set shape "circle"
    set color blue

    ;; Set language proficiency for "host" agents
    ;; Example: "host" agents do not understand "immigrant" and
    ;;           and "pivot" languages
    set langLevelHost 1.0
    set langLevelPivot 0.0
    set langLevelImmigrant 1.0

  ]

  crt numImmigrant [
    move-to one-of patches with [not any? turtles-here]
    set origin "immigrant"
    set payoff 0.5

    ;;;;
    set likenessOwn random-float likenessII
    set likenessOther random-float likenessIH
    ;;;;

    ifelse random-float 1 < likenessOwn ;; use "likeness" to initalize
      [ set strategyOwn "C" ]            ;; in-group strategy
      [ set strategyOwn "D" ]
    ifelse random-float 1 < likenessOther ;; use "likeness" to initialize
      [ set strategyOther "C" ]          ;; out-group strategy
      [ set strategyOther "D" ]

    set size 4
    set shape "square"
    set color green

    ;; Set language proficiency for "immigrant" agents
    ;; Example: "immigrant" agents understand a little "host" language
    ;;           but not "pivot" language
    set langLevelHost 1.0
    set langLevelPivot 0.0
    set langLevelImmigrant 1.0

  ]


  create-small-world-network


  ask links [
    set understand? true
    set like? true
    display-link
  ]

  ask turtles [ update-state ]
  ask turtles [ display-turtle ]

  reset-ticks

end


to go

  ifelse ticks >= maxWeeks
  [ stop ]
  [
    ;; Reset the payoff (synchronous step)
    ask turtles [ set payoff 0.5 ]

    ;; Pairwise interactions
    ask links [

      let thisLink self

      ;; Get endpoints
      let ag1 end1
      let ag2 end2

      ;; Ask endpoints to interact
      ask ag1 [

        likeDislike ag2 ;; like? = true if successful

        ;; If two previous successful, endpoints play game
        let play? ([ like? ] of thisLink)
        if play? [ playGame ag2 ]
      ]
    ]

    ;; Strategy & Structural Updates (individual decision)
    ask turtles [ reviseStrategyAndLinks ]

  ]

  ;; Now, update the variables used for post-processing with
  ;; RNetLogo, if required
  ask turtles [ update-state ]

  tick

  ;; Update display
  ask turtles [ display-turtle ]
  ask links [ display-link ]

end

;;; AGENT INTERACTION PROCEDURES


to likeDislike [partner]
  let ag1 self
  let ag2 partner
  let likeEachOther? true
  let mutualLikeness 0

  ifelse [origin] of ag1 = "host"
  [ ifelse [origin] of ag2 = "host"
    [ set mutualLikeness min (list ([likenessOwn] of ag1) ([likenessOwn] of ag2)) ]
    [ set mutualLikeness min (list ([likenessOther] of ag1) ([likenessOther] of ag2)) ] ]
  [ ifelse [origin] of ag2 = "host"
    [ set mutualLikeness min (list ([likenessOther] of ag1) ([likenessOther] of ag2)) ]
    [ set mutualLikeness min (list ([likenessOwn] of ag1) ([likenessOwn] of ag2)) ] ]

  set likeEachOther? random-float 1 < mutualLikeness
  ask link ([who] of ag1) ([who] of ag2) [ set like? likeEachOther? ]

end


to playGame [partner]
  ;; Play normalized game, curently PD
  ;; Payoff matrix:
  ;; [ R = 1            S =  -gamma ]
  ;; [ T = 1 + gamma    P = 0       ]

  let sameOrigin? true
  if origin != [origin] of partner [ set sameOrigin? false ]

  let nCooperators 0
  let nDefectors 0

  ;; Play with "like" agent
  if sameOrigin? [
    ifelse strategyOwn = "C"
    [
      ;; I'm a cooperator with "like" agents:
      ask partner [
        ifelse [strategyOwn] of self = "C"
        [ set payoff (payoff + R)
          set nCooperators (nCooperators + 1) ]
        [ set payoff (payoff + T)
          set nDefectors (nDefectors + 1) ]
      ]
      set payoff (payoff + nCooperators * R + nDefectors * S)
    ]
    [
      ;; I'm a defector with "like" agents:
      ask partner [
        if [strategyOwn] of self = "C"
        [ set payoff (payoff + S)
          set nCooperators (nCooperators + 1) ]
      ]
      set payoff (payoff + nCooperators * T)
    ]
  ]

  set nCooperators 0
  set nDefectors 0

  ;; Play with "other" agent
  if not sameOrigin? [
    ifelse strategyOther = "C"
    [
      ;; I'm a cooperator with "other" agents:
      ask partner [

        ifelse [strategyOther] of self = "C"
        [ set payoff (payoff + R)
          set nCooperators (nCooperators + 1) ]
        [ set payoff (payoff + T)
          set nDefectors (nDefectors + 1) ]
      ]
      set payoff (payoff + nCooperators * R + nDefectors * S)
    ]
    [
      ;; I'm a defector with "other" agents:
      ask partner [
        if [strategyOther] of self = "C"
        [ set payoff (payoff + S)
          set nCooperators (nCooperators + 1) ]
      ]
      set payoff (payoff + nCooperators * T)
    ]
  ]

end


to reviseStrategyAndLinks
  let prob random-float (1 + W)

  let candidateLinks my-links with [ understand? and like? ]
  let candidatePartners turtle-set [ other-end ] of candidateLinks

  let myLinkNeighbors link-neighbors
  let likeLinkNeighbors myLinkNeighbors with [origin = [origin] of self]
  let otherLinkNeighbors myLinkNeighbors with [origin != [origin] of self]

  ;; Pick one partner for potential rewiring
  let B one-of myLinkNeighbors

  let likeMe? (origin = [origin] of B)
  let understandB? [understand?] of link-with B
  let likeB? [like?] of link-with B

  ifelse prob < 1 [

    ;; Strategy update: based on dominant strategies & corresponding payoffs
    if any? candidatePartners [

      let candidate max-one-of candidatePartners [count link-neighbors]

      ;; Test for update of the strategy profile
      if Fermi-Dirac payoff ([payoff] of candidate) 0.005 > random-float 1 [
        set strategyOwn [strategyOwn] of candidate
        set strategyOther [strategyOther] of candidate
      ]

    ]

    ;; End of strategy update

  ]
  [
    ;; Structural update
    ifelse likeMe?
    [
         if [strategyOwn] of B = "D" or not understandB? or not likeB? [
           if (Fermi-Dirac ([payoff] of B) payoff 0.005 > random-float 1)
               and
             ((count [link-neighbors] of B) > 1) [ pickAnother self B ]

           ]

    ]
    [
         if [strategyOther] of B = "D" or not understandB? or not likeB? [
           if (Fermi-Dirac ([payoff] of B) payoff 0.005 > random-float 1)
               and
             ((count [link-neighbors] of B) > 1) [ pickAnother self B ]
         ]

    ]


  ]
end



;;; END OF AGENT INTERACTION PROCEDURES

;; Report a list of the global language level of
;; agents in "agentset", of given "origin (group)", on "language"
to-report languageLevel [agentset group language]
  let thisGroup agentset with [origin = group]
  let groupLevel []

  if language = "host" [ set groupLevel [langLevelHost] of thisGroup ]
  if language = "pivot" [ set groupLevel [langLevelPivot] of thisGroup ]
  if language = "immigrant" [ set groupLevel [langLevelImmigrant] of thisGroup ]

  report groupLevel

end

;; AUXILIARY FUNCTIONS - AGENT INTERACTIONS
;; Conditioned learning
to-report update [value alpha lambda]
  ifelse logisticLearning?
  [ report value + alpha * value * (lambda - value) ]
  [ report value + alpha *  (lambda - value) ]
end

;; Pick another neighbor
to pickAnother [A B]
  let myLinkNeighbors [link-neighbors] of A
  let candidates ([link-neighbors] of B) with [not member? self (turtle-set A myLinkNeighbors)]
  if any? candidates [
    let C one-of candidates
    ask A [
      create-link-with C [
        set understand? true
        set like? true
      ]
      ask link-with B [ die ]
    ]
  ]
end

to-report Fermi-Dirac [x y beta]
  report 1 / (1 + exp(beta * (x - y)))
end

to-report random-weighted [values weights]
  let selector (random-float sum weights)
  let running-sum 0
  (foreach values weights [ [?1 ?2] ->
      set running-sum (running-sum + ?2)
      if (running-sum > selector) [report ?1]
                          ]
  )
end

;; AUXILIARY FUNCTIONS - NETWORK GENERATION
;; Auxiliary function, used to create a small-world network
to create-small-world-network
  ;; Start with a ring network
  ask turtles [
    let links-done 0
    while [links-done < (numInteractionsWeek / 2) ] [
      create-link-with turtle ((who + links-done  + 1) mod numAgents) [
        set rewired? false
      ]
      set links-done (links-done + 1)
    ]
  ]

  layout-circle (sort turtles) (max-pxcor / 2) - 2

  ;; Rewire links with probability P
  ask links with [not rewired? and count [link-neighbors] of end2 > 1] [
    if pRewire > random-float 1.0 [

      let a end1
      let b end2
      let neigh [link-neighbors] of a
      let candidates turtles with [not member? self (turtle-set a neigh)]
      if any? candidates [
        ask a [
          create-link-with one-of candidates [
            set rewired? true
          ]
        ]
        ask self [die]
      ]

    ]
  ]
end

;; AUXILIARY FUNCTIONS - DISPLAY AGENTS, LINKS, NETWORK
to layout
  layout-spring turtles links (springK / numInteractionsWeek) (springL * numInteractionsWeek) (springR * numInteractionsWeek)
end

to display-turtle
  ifelse origin = "host"
  [
    set color blue
    ifelse strategyOwn = "C"
    [ if strategyOther = "D" [ set color red ] ]
    [
      ifelse strategyOther = "C"
      [ set color pink ]
      [ set color brown ]
    ]
  ]
  [
    set color green
    ifelse strategyOwn = "C"
    [ if strategyOther = "D" [ set color red ] ]
    [
      ifelse strategyOther = "C"
      [ set color pink ]
      [ set color brown ]
    ]
  ]
end

to display-link
  ifelse [origin] of end1 = "host" and [origin] of end2 = "host" [
    set color blue - 2
  ]
  [
    ifelse [origin] of end1 = "immigrant" and [origin] of end2 = "immigrant"
    [ set color green - 1 ]
    [ set color yellow - 3 ]

  ]

end

;; REPORTERS FOR POST-PROCESSING OPERATIONS
to update-state
  ;; Update agent's state, for post-processing
  set numHostNeighbors count link-neighbors with [origin = item 0 origins]
  set numImmigrantNeighbors count link-neighbors with [origin = item 1 origins]

  ;; Get average language levels of link-neighbors
  set avgLangHostHost 0
  set avgLangHostImmigrant 0
  set avgLangHostPivot 0
  set avgLangImmigrantHost 0
  set avgLangImmigrantImmigrant 0
  set avgLangImmigrantPivot 0
  if numHostNeighbors > 0 [
    set avgLangHostHost (mean languageLevel link-neighbors "host" "host")
    set avgLangHostImmigrant (mean languageLevel link-neighbors "host" "immigrant")
    set avgLangHostPivot (mean languageLevel link-neighbors "host" "pivot")
  ]
  if numImmigrantNeighbors > 0 [
    set avgLangImmigrantHost (mean languageLevel link-neighbors "immigrant" "host")
    set avgLangImmigrantImmigrant (mean languageLevel link-neighbors "immigrant" "immigrant")
    set avgLangImmigrantPivot (mean languageLevel link-neighbors "immigrant" "pivot")
  ]


  ;; Get # of strategy profiles of link neighbors (and origin)
  set numHostCC 0
  set numHostCD 0
  set numHostDC 0
  set numHostDD 0
  set numImmigrantCC 0
  set numImmigrantCD 0
  set numImmigrantDC 0
  set numImmigrantDD 0
  ask link-neighbors [
    ifelse origin = item 0 origins
    [ if strategyOwn = "C" and strategyOther = "C" [set numHostCC (numHostCC + 1)]
      if strategyOwn = "C" and strategyOther = "D" [set numHostCD (numHostCD + 1)]
      if strategyOwn = "D" and strategyOther = "C" [set numHostDC (numHostDC + 1)]
      if strategyOwn = "D" and strategyOther = "D" [set numHostDD (numHostDD + 1)]
    ]
    [ if strategyOwn = "C" and strategyOther = "C" [set numImmigrantCC (numImmigrantCC + 1)]
      if strategyOwn = "C" and strategyOther = "D" [set numImmigrantCD (numImmigrantCD + 1)]
      if strategyOwn = "D" and strategyOther = "C" [set numImmigrantDC (numImmigrantDC + 1)]
      if strategyOwn = "D" and strategyOther = "D" [set numImmigrantDD (numImmigrantDD + 1)]
    ]
  ]

end


to-report get-cycle-information

  ;; Host agents:
  let hostAgents turtles with [origin = item 0 origins]

  ;; Immigrant
  let immigrantAgents turtles with [origin = item 1 origins]
  report (list
    (ticks)
    (count hostAgents with [strategyOwn = "C" and strategyOther = "C"])
    (count hostAgents with [strategyOwn = "C" and strategyOther = "D"])
    (count hostAgents with [strategyOwn = "D" and strategyOther = "C"])
    (count hostAgents with [strategyOwn = "D" and strategyOther = "D"])
    (count immigrantAgents with [strategyOwn = "C" and strategyOther = "C"])
    (count immigrantAgents with [strategyOwn = "C" and strategyOther = "D"])
    (count immigrantAgents with [strategyOwn = "D" and strategyOther = "C"])
    (count immigrantAgents with [strategyOwn = "D" and strategyOther = "D"])
    (count links with [ [origin] of end1 = "host" and [origin] of end2 = "host"])
    (count links with [ [origin] of end1 = "immigrant" and [origin] of end2 = "immigrant"])
    (count links with [ [origin] of end1 != [origin] of end2])
    (mean languageLevel turtles "host" "host")
    (mean languageLevel turtles "host" "immigrant")
    (mean languageLevel turtles "host" "pivot")
    (mean languageLevel turtles "immigrant" "host")
    (mean languageLevel turtles "immigrant" "immigrant")
    (mean languageLevel turtles "immigrant" "pivot")
    (mean [payoff] of hostAgents)
    (mean [count link-neighbors] of hostAgents)
    (mean [payoff] of immigrantAgents)
    (mean [count link-neighbors] of immigrantAgents)
  )
end

to-report cycle-information-names
  report (list
    "time"
    "total.num.host.CC"
    "total.num.host.CD"
    "total.num.host.DC"
    "total.num.host.DD"
    "total.num.immigrant.CC"
    "total.num.immigrant.CD"
    "total.num.immigrant.DC"
    "total.num.immigrant.DD"
    "num.links.HH"
    "num.links.II"
    "num.links.HI"
    "avg.langlev.host.host.lang"
    "avg.langlev.host.immigrant.lang"
    "avg.langlev.host.pivot.lang"
    "avg.langlev.immigrant.host.lang"
    "avg.langlev.immigrant.immigrant.lang"
    "avg.langlev.immigrant.pivot.lang"
    "avg.communality.host"
    "avg.degree.host"
    "avg.communality.immigrant"
    "avg.degree.immigrant"
    )
end
@#$#@#$#@
GRAPHICS-WINDOW
284
84
1167
968
-1
-1
1.75
1
10
1
1
1
0
0
0
1
0
499
0
499
1
1
1
ticks
30.0

BUTTON
23
102
89
135
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
147
340
271
400
propImmigrant
0.15
1
0
Number

INPUTBOX
10
542
144
602
learningConstant
0.0
1
0
Number

INPUTBOX
10
674
147
734
threshold
0.0
1
0
Number

BUTTON
104
102
170
135
NIL
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

SWITCH
10
804
148
837
logisticLearning?
logisticLearning?
0
1
-1000

INPUTBOX
10
607
145
667
forgettingConstant
0.0
1
0
Number

BUTTON
587
25
654
59
layout
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
14
194
263
239
# links (check)
count links
0
1
11

TEXTBOX
14
315
164
339
Population
16
0.0
1

TEXTBOX
7
423
290
472
Network initialization
16
0.0
1

TEXTBOX
15
516
165
540
Language
16
0.0
1

TEXTBOX
159
516
309
540
Social game
16
0.0
1

INPUTBOX
155
542
270
602
gamma
1.25
1
0
Number

INPUTBOX
155
607
270
667
W
1.0
1
0
Number

TEXTBOX
37
25
258
58
INPUT & CONTROL
24
0.0
1

TEXTBOX
447
27
597
56
NETWORK
24
0.0
1

MONITOR
1757
553
1875
598
% links H - H
round (count links with [ [origin] of end1 = \"host\" and [origin] of end2 = \"host\"] / count links * 100)
2
1
11

MONITOR
1757
608
1874
653
% links I - I
round (count links with [ [origin] of end1 = \"immigrant\" and [origin] of end2 = \"immigrant\"] / count links * 100)
2
1
11

MONITOR
1757
663
1875
708
% links H (I) - I (H)
round (count links with [ [origin] of end1 != [origin] of end2] / count links * 100)
2
1
11

PLOT
1204
87
1739
297
Communality - HOST
communality
# agents
-15.0
75.0
0.0
125.0
false
true
"" ""
PENS
"C - C" 1.0 1 -10899396 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [payoff] of hostAgents with [ strategyOwn = \"C\" and  strategyOther = \"C\"]"
"C - D" 1.0 1 -2674135 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [payoff] of hostAgents with [ strategyOwn = \"C\" and  strategyOther = \"D\"]"
"D - C" 1.0 1 -13345367 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [payoff] of hostAgents with [ strategyOwn = \"D\" and  strategyOther = \"C\"]"
"D - D" 1.0 1 -7500403 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [payoff] of hostAgents with [ strategyOwn = \"D\" and  strategyOther = \"D\"]"

PLOT
1205
320
1740
533
Communality - IMMIGRANT
NIL
NIL
-15.0
75.0
0.0
125.0
false
true
"" ""
PENS
"C - C" 1.0 1 -10899396 true "" "let immigrants turtles with [ origin = \"immigrant\"]\nhistogram [payoff] of immigrants with [strategyOwn = \"C\" and strategyOther = \"C\"]"
"C - D" 1.0 1 -2674135 true "" "let immigrants turtles with [ origin = \"immigrant\"]\nhistogram [payoff] of immigrants with [strategyOwn = \"C\" and strategyOther = \"D\"]"
"D - C" 1.0 1 -13345367 true "" "let immigrants turtles with [ origin = \"immigrant\"]\nhistogram [payoff] of immigrants with [strategyOwn = \"D\" and strategyOther = \"C\"]"
"D - D" 1.0 1 -7500403 true "" "let immigrants turtles with [ origin = \"immigrant\"]\nhistogram [payoff] of immigrants with [strategyOwn = \"D\" and strategyOther = \"D\"]"

TEXTBOX
1220
32
1752
102
COMMUNALITY & NODE DEG. DISTRIBUTION
24
0.0
1

PLOT
1208
553
1742
763
Node degree distribution - HOST
degree
# nodes
0.0
75.0
0.0
125.0
false
true
"" ""
PENS
"C - C" 1.0 1 -10899396 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [count link-neighbors] of hostAgents with [ strategyOwn = \"C\" and strategyOther = \"C\"]"
"C - D" 1.0 1 -2674135 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [count link-neighbors] of hostAgents with [ strategyOwn = \"C\" and strategyOther = \"D\"]"
"D - C" 1.0 1 -13345367 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [count link-neighbors] of hostAgents with [ strategyOwn = \"D\" and strategyOther = \"C\"]"
"D - D" 1.0 1 -7500403 true "" "let hostAgents turtles with [origin = \"host\"]\nhistogram [count link-neighbors] of hostAgents with [ strategyOwn = \"D\" and strategyOther = \"D\"]"

PLOT
1209
779
1742
992
Node degree distribution - IMMIGRANT
NIL
NIL
0.0
75.0
0.0
125.0
false
true
"" ""
PENS
"C - C" 1.0 1 -10899396 true "" "let immigrants turtles with [origin = \"immigrant\"]\nhistogram [count link-neighbors] of immigrants with [strategyOwn = \"C\" and strategyOther = \"C\"]"
"C - D" 1.0 1 -2674135 true "" "let immigrants turtles with [origin = \"immigrant\"]\nhistogram [count link-neighbors] of immigrants with [strategyOwn = \"C\" and strategyOther = \"D\"]"
"D - C" 1.0 1 -13345367 true "" "let immigrants turtles with [origin = \"immigrant\"]\nhistogram [count link-neighbors] of immigrants with [strategyOwn = \"D\" and strategyOther = \"C\"]"
"D - D" 1.0 1 -7500403 true "" "let immigrants turtles with [origin = \"immigrant\"]\nhistogram [count link-neighbors] of immigrants with [strategyOwn = \"D\" and strategyOther = \"D\"]"

TEXTBOX
297
1003
710
1320
AGENTS' COLOR CODE:\n         \"Host\"  C-C  ->  blue\n\"Immigrant\"  C-C  ->  green\n Ethnocentric C-D  ->  red\n  Coop. other D-C  ->  pink\n    Defectors D-D  ->  brown
16
0.0
1

TEXTBOX
717
1009
1184
1284
LINKS' COLOR CODE:\n                 \"host\"   <->  \"host\"                       blue\n          \"immigrant\"  <->  \"immigrant\"              green\n\"host\"(\"immigrant\") <->  \"immigrant\"(\"host\")   yellow
16
0.0
1

INPUTBOX
680
13
749
73
springK
0.5
1
0
Number

INPUTBOX
760
13
829
73
springL
1.5
1
0
Number

INPUTBOX
840
14
909
74
springR
16.0
1
0
Number

TEXTBOX
5
837
155
857
Likeness
16
0.0
1

INPUTBOX
144
861
277
921
likenessHI
0.45
1
0
Number

INPUTBOX
144
929
277
989
likenessIH
0.5
1
0
Number

INPUTBOX
193
90
264
150
maxWeeks
520.0
1
0
Number

INPUTBOX
13
339
137
399
numAgents
1000.0
1
0
Number

INPUTBOX
9
447
134
507
numInteractionsWeek
6.0
1
0
Number

INPUTBOX
143
447
270
507
pRewire
0.25
1
0
Number

INPUTBOX
2
860
134
920
likenessHH
0.9
1
0
Number

INPUTBOX
2
928
134
988
likenessII
0.95
1
0
Number

MONITOR
14
257
264
302
NIL
count links with [like? and understand?]
17
1
11

INPUTBOX
10
739
147
799
langOverlap
1.0
1
0
Number

MONITOR
1758
87
1861
132
% host CC
round (count turtles with [origin = \"host\" and strategyOwn = \"C\" and strategyOther = \"C\"] / (count turtles with [origin = \"host\"]) * 100)
17
1
11

MONITOR
1759
140
1862
185
% host CD
round (count turtles with [origin = \"host\" and strategyOwn = \"C\" and strategyOther = \"D\"] / (count turtles with [origin = \"host\"]) * 100)
17
1
11

MONITOR
1759
195
1862
240
% host DC
round (count turtles with [origin = \"host\" and strategyOwn = \"D\" and strategyOther = \"C\"] / (count turtles with [origin = \"host\"]) * 100)
17
1
11

MONITOR
1759
250
1862
295
% host DD
round (count turtles with [origin = \"host\" and strategyOwn = \"D\" and strategyOther = \"D\"] / (count turtles with [origin = \"host\"]) * 100)
17
1
11

MONITOR
1759
322
1862
367
% immigrant CC
round (count turtles with [origin = \"immigrant\" and strategyOwn = \"C\" and strategyOther = \"C\"] / (count turtles with [origin = \"immigrant\"]) * 100)
17
1
11

MONITOR
1759
377
1862
422
% immigrant CD
round (count turtles with [origin = \"immigrant\" and strategyOwn = \"C\" and strategyOther = \"D\"] / (count turtles with [origin = \"immigrant\"]) * 100)
17
1
11

MONITOR
1759
433
1862
478
% immigrant DC
round (count turtles with [origin = \"immigrant\" and strategyOwn = \"D\" and strategyOther = \"C\"] / (count turtles with [origin = \"immigrant\"]) * 100)
17
1
11

MONITOR
1759
488
1862
533
% immigrant DD
round (count turtles with [origin = \"immigrant\" and strategyOwn = \"D\" and strategyOther = \"D\"] / (count turtles with [origin = \"immigrant\"]) * 100)
17
1
11

@#$#@#$#@
## WHAT IS IT?

This is network model of interaction between immigrants and a host population.


Agents have an "origin", currently set to "host" and "immigrant", and a "communication"
variable (list of lists) which stores the language level for each language.

The number and name of languages is arbitrary. "Pivot" languages (e.g. english) can be introduced. The model can deal with the trivial cases in which "host" and "immigrant" agents have the same language, or in which immigrants are highly proficient in the host language

## HOW IT WORKS.

In each cycle, agents interact with a specified number of other agents.

In each interaction, they try to communicate with the other agent, choosing first the language in which they have the highest proficiency.

The interaction is successful if the proficiency of both agents in the trial language is above a threshold.

If the interaction is successful and the agent's proficiency is lesser than that of the other agent, the language is inproved using the Rescorla-Wagner model of conditioned learning. Otherwise, the agent tries a different language

Languages are then "forgotten", with a rate that can be different than that of learning.
The proficiency is maintained by the interactions - if there are no interactions, language proficiency fades.


## HOW TO USE IT

Vary the parameters and initialization of the agents' language proficiency (level) and see how the language proficiency of "host" and "immigrant" agents varies with time.



## THINGS TO NOTICE
The threshold combines with the setup of language proficiency in determining the proportion of immigrants that learn "host" language (and "host" that learn immigrants' language)

The threshold used for testing for successful interation is multiplied random-uniform variable, so that agents get a chance of "stepping over the language barrier"

## THINGS TO TRY

Replace the learning/forgetting model by an alternative (e.g. Cobb-Douglas)

## EXTENDING THE MODEL

Introduce a "pivot" language, include new plots, and see what's the effect;
Specify languages with different learning constants (difficulty);
Specify learning constants dependent on age, etc (heterogeneous among agents, according to some model).

## NETLOGO FEATURES

This file was automatically converted by NetLogo from version 5.3.1 to version 6.0.1.
(Tasks were replaced by anonymous procedures)
Changes introduced after reviewers' comments:
- Language learning decoupled (initial proficiency levels set to 1, thresholds & learning constants 0)

Changes in this version:
- Payoff updated asynchronously;
- Error corrected in the language module - agents now forget langages if they do not have enough interactions to sustain them;
- Communication, likeness and game play are done pairwise; strategy & structural updates are individual;
- Strategy updates are based on neighbors' strategies instead of a single random partner;
- Error corrected in reporter of % H-H links
- Error corrected in "go": "tick" must precede plot updates, etc.
- The following reporters were added:
* get-cycle-information: global information for time cycle
* cycle-information-names: variable names in "get-cycle-information"
* get-agent-state [agent] : get the individual information of "agent"
* agent-state-names: variable names in "get-agent-state"
* get-links: get the network links

## RELATED MODELS

NA

## CREDITS AND REFERENCES

NA
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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
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
