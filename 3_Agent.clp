;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate agent-type (slot name (allowed-values Ada Bill)))

(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
 )

; deve essere utilizzata solo al passo 0 e inizializza il tipo di agente
(defrule choose-agent
  (status (step 0) (mode computer))
  =>
  (printout t "Choose your agent (Ada/Bill):" crlf)
  (bind ?input (readline))
  (printout t "You chose " ?input crlf)
  (assert (agent-type (name ?input)))
)

(defrule keep-agent
  (agent-type (name ?name))
  (status (step ?s) (mode computer))
  =>
  (printout t "[" ?name "] guess number: " ?s crlf)
  (if(eq ?name "Ada") then (focus ADA) else (if(eq ?name "Bill") then (focus BILL) ))
)
