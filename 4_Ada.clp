(defmodule ADA (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deftemplate guess-computer
    (slot step)
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

;; dato un numero, ritorna i colori associati
;; (1234) -> (blue green red yellow)
(deffunction convert_code($?number)
    (bind $?colours (create$  blue green red yellow orange white black purple ))
    return (create$ (nth$(nth$ 1 $?number) $?colours) (nth$(nth$ 2 $?number)$?colours) (nth$(nth$ 3 $?number)$?colours)(nth$(nth$ 4 $?number) $?colours))
)

(defrule init-1
    (status (step 0) (mode computer))
    =>
    (assert (guess-computer (step 0) (code (create$ 1 2 3 4))))
    (assert (guess (step 0) (g blue green red yellow)))
    (printout t "[ADA] Step: 0" ", I guessed: (blue green red yellow) (1 2 3 4)" crlf)
)

(defrule init-2
    (status (step 1) (mode computer))
    =>
    (assert (guess-computer (step 1) (code (create$ 5 6 7 8))))
    (assert (guess (step 1) (g orange white black purple)))
    (printout t "[ADA] Step: 0" ", I guessed: (orange white black purple) (5 6 7 8)" crlf)
)

(defrule make-guess
    ?guess-computer <- (guess-computer (step ?last_s) (code $?pw))
    (answer (step ?last_s) (right-placed ?black-pegs) (miss-placed ?white-pegs))
    (status (step ?new_s) (mode computer))
    =>
)
