(defmodule ADA (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deftemplate guess-computer
    (slot step)
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate possible-colors
    (multislot colors (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 8))
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
    (assert (possible-colors (colors 1 2 3 4 5 6 7 8)))
    (printout t "(blue green red yellow) (1 2 3 4)" crlf)
)

(defrule init-2
    (status (step 1) (mode computer))
    (answer (step 0) (right-placed ?bp) (miss-placed ?wp))
    ?pc <- (possible-colors (colors $?cols))
    =>
    (if (eq (+ ?bp ?wp) 0) then
        (modify ?pc (colors (delete-member$ $?cols 1)))
        (modify ?pc (colors (delete-member$ $?cols 2)))
        (modify ?pc (colors (delete-member$ $?cols 3)))
        (modify ?pc (colors (delete-member$ $?cols 4)))
    )

    (assert (guess-computer (step 1) (code (create$ 5 6 7 8))))
    (assert (guess (step 1) (g orange white black purple)))
    (printout t "> init-2" crlf)
    (printout t "(orange white black purple) (5 6 7 8)" crlf)
)

(defrule make-first-guess
    ?guess-computer-0 <- (guess-computer (step 0) (code $?pw-0))
    (answer (step 0) (right-placed ?bp-0) (miss-placed ?wp-0))
    ?guess-computer-1 <- (guess-computer (step 1) (code $?pw-1))
    (answer (step 1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (status (step 2) (mode computer))
    =>
    (if (> ?bp-0 ?bp-1) then
        (bind ?pos1 (nth$ 1 $?pw-0))
        (bind ?pos2 (nth$ 2 $?pw-0))
        (bind ?pos3 (nth$ 1 $?pw-1))
        (bind ?pos4 (nth$ 2 $?pw-1))
     else
        (bind ?pos1 (nth$ 1 $?pw-1))
        (bind ?pos2 (nth$ 2 $?pw-1))
        (bind ?pos3 (nth$ 1 $?pw-0))
        (bind ?pos4 (nth$ 2 $?pw-0))
    )

    (bind $?pw-n (create$ ?pos1 ?pos2 ?pos3 ?pos4))

    (assert (guess-computer (step 2) (code $?pw-n)))
    (assert (guess (step 2) (g (convert_code $?pw-n))))
    (printout t "> make-first-guess" crlf)
    (printout t (convert_code $?pw-n) " " $?pw-n crlf)
)

(defrule make-guess
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s) (code $?pw))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(> ?s 2))
    (test(eq ?last-s (- ?s 1)))
    (possible-colors (colors $?cols))
    =>
    (bind ?l (length$ $?cols))
    (bind ?roll1 (random 1 ?l))
    (bind ?pos1 (nth$ ?roll1 $?cols))

    (bind ?roll2 ?roll1)
    (bind ?roll3 ?roll1)
    (bind ?roll4 ?roll1)

    (while (eq ?roll1 ?roll2)
        (bind ?roll2 (random 1 ?l)))
    (bind ?pos2 (nth$ ?roll2 $?cols))

    (while (or (eq ?roll1 ?roll3) (eq ?roll2 ?roll3))
        (bind ?roll3 (random 1 ?l)))
    (bind ?pos3 (nth$ ?roll3 $?cols))

    (while (or (eq ?roll1 ?roll4) (or (eq ?roll2 ?roll4) (eq ?roll3 ?roll4)))
        (bind ?roll4 (random 1 ?l)))
    (bind ?pos4 (nth$ ?roll4 $?cols))

    (bind ?pw-n (create$ ?pos1 ?pos2 ?pos3 ?pos4))
    (assert (guess-computer (step ?s) (code ?pw-n)))
    (assert (guess (step ?s) (g (convert_code ?pw-n))))
    (printout t "> make-guess" crlf)
    (printout t (convert_code ?pw-n)" "?pw-n crlf)
)
