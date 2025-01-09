(defmodule BILL (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))


(deftemplate guess-computer
    (slot step)
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate possible-colors
    (multislot colors (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 8))
)

(deftemplate hit-1st-half
    (slot v (allowed-values TRUE FALSE)))
(deftemplate hit-2nd-half
    (slot v (allowed-values TRUE FALSE)))
(deftemplate colors-found
    (slot v (allowed-values TRUE FALSE)))
(deftemplate found-1st
    (slot v (allowed-values TRUE FALSE)))
(deftemplate found-2nd
    (slot v (allowed-values TRUE FALSE)))
(deftemplate found-3rd
    (slot v (allowed-values TRUE FALSE)))
(deftemplate found-4th
    (slot v (allowed-values TRUE FALSE)))

;; dato un numero, ritorna i colori associati
;; (1234) -> (blue green red yellow)
(deffunction convert_code($?number)
    (bind $?colours (create$  blue green red yellow orange white black purple))
    return (create$ (nth$ (nth$ 1 $?number) $?colours) (nth$ (nth$ 2 $?number) $?colours)
                    (nth$ (nth$ 3 $?number) $?colours)(nth$ (nth$ 4 $?number) $?colours))
)

(deffunction swap(?pos1 ?pos2 $?list)
    (if (not (and(integerp ?pos1)(integerp ?pos2))) then
        return FALSE)

    (bind ?n1 (nth$ ?pos1 $?list))
    (bind ?n2 (nth$ ?pos2 $?list))
    (bind ?l (length$ $?list))
    (bind ?i 1)
    (bind $?updated-list (create$))

    (while (<= ?i ?l)
        (if (eq ?i ?pos1) then
            (bind $?updated-list (insert$ $?updated-list ?i ?n2))
        else
            (if (eq ?i ?pos2) then
                (bind $?updated-list (insert$ $?updated-list ?i ?n1))
            else
                (bind ?n (nth$ ?i $?list))
                (bind $?updated-list (insert$ $?updated-list ?i ?n))
            )
        )
        (bind ?i (+ ?i 1))
    )

    return $?updated-list
)

(deffunction difference(?listA ?listB)
    (bind ?i 1)
    (bind ?j 1)
    (bind ?la (length$ $?listA))
    (bind ?lb (length$ $?listB))
    (bind $?result (create$))
    (while (<= ?i ?la)
        (bind ?n (nth$ ?i $?listA))
        (while (<= ?j ?lb)
            (if (eq ?n (nth$ ?j $?listB)) then
                (bind ?j (+ ?j 9))
            )
            (if (eq ?j ?lb) then
                (bind $?result (insert$ $?result ?la ?n)))

            (bind ?j (+ ?j 1))
        )
        (bind ?i (+ ?i 1))
    )
    return $?result
)

(deffunction changed-digit-0(?pw-1 ?pw)
    (bind ?i 1)
    (bind ?l (length$ $?pw))
    (while (<= ?i ?l)
        (if (neq (nth$ ?i $?pw-1) (nth$ ?i $?pw)) then
            return (nth$ ?i $?pw)
        )
        (bind ?i (+ ?i 1))
    )
    return -1
)

(deffunction changed-digit-1(?pw-1 ?pw)
    (bind ?i 1)
    (bind ?l (length$ $?pw))
    (while (<= ?i ?l)
        (if (neq (nth$ ?i $?pw-1) (nth$ ?i $?pw)) then
            return (nth$ ?i $?pw-1)
        )
        (bind ?i (+ ?i 1))
    )
    return -1
)

(defrule init-1
    (status (step 0) (mode computer))
    =>
    (assert (guess-computer (step 0) (code (create$ 1 2 3 4))))
    (assert (guess (step 0) (g blue green red yellow)))
    (assert (possible-colors (colors 1 2 3 4 5 6 7 8)))
    (printout t "(blue green red yellow) (1 2 3 4)" crlf)
)

(defrule init-1-hit
    (not (hit-1st-half (v TRUE)))
    (status (step 1) (mode computer))
    (answer (step 0) (right-placed ?bp) (miss-placed ?wp))
    (guess-computer (step 0) (code $?pw-0))
    (test (eq (+ ?bp ?wp) 4))
    ?pc <- (possible-colors (colors $?cols))
    =>
    (printout t "> init-1-hit" crlf)

    (bind $?cols (delete-member$ $?cols 5))
    (bind $?cols (delete-member$ $?cols 6))
    (bind $?cols (delete-member$ $?cols 7))
    (bind $?cols (delete-member$ $?cols 8))
    (modify ?pc (colors $?cols))

    (bind $?pw (swap 1 2 $?pw-0))
    (assert (hit-1st-half (v TRUE)))
    (assert (colors-found (v TRUE)))
    (assert (guess-computer (step 1) (code $?pw)))
    (assert (guess (step 1) (g (convert_code $?pw))))
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule init-2
    (status (step 1) (mode computer))
    (answer (step 0) (right-placed ?bp) (miss-placed ?wp))
    (test (< (+ ?bp ?wp) 4))
    ?pc <- (possible-colors (colors $?cols))
    =>
    (assert (guess-computer (step 1) (code (create$ 5 6 7 8))))
    (assert (guess (step 1) (g orange white black purple)))
    (printout t "> init-2" crlf)
    (printout t "(orange white black purple) (5 6 7 8)" crlf)
)

(defrule init-2-hit
    (not (hit-1st-half (v TRUE)))
    (not (hit-2nd-half (v TRUE)))
    (status (step 2) (mode computer))
    (answer (step 1) (right-placed ?bp) (miss-placed ?wp))
    (guess-computer (step 1) (code $?pw-1))
    (test (eq (+ ?bp ?wp) 4))
    ?pc <- (possible-colors (colors $?cols))
    =>
    (bind $?cols (delete-member$ $?cols 1))
    (bind $?cols (delete-member$ $?cols 2))
    (bind $?cols (delete-member$ $?cols 3))
    (bind $?cols (delete-member$ $?cols 4))
    (modify ?pc (colors $?cols))

    (bind $?pw (swap 1 2 $?pw-1))
    (assert (hit-2nd-half (v TRUE)))
    (assert (colors-found (v TRUE)))
    (assert (guess-computer (step 2) (code $?pw)))
    (assert (guess (step 2) (g (convert_code $?pw))))
    (printout t "> init-2-hit" crlf)
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule make-first-guess
    (not (colors-found (v TRUE)))
    (status (step 2) (mode computer))
    ?guess-computer-0 <- (guess-computer (step 0) (code $?pw-0))
    ?guess-computer-1 <- (guess-computer (step 1) (code $?pw-1))
    (answer (step 0) (right-placed ?bp-0) (miss-placed ?wp-0))
    (answer (step 1) (right-placed ?bp-1) (miss-placed ?wp-1))
    ?pc <- (possible-colors (colors $?cols))
    =>
    (if (> ?bp-0 ?bp-1) then
        (printout t "> bp increase" crlf)
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
    ;; (declare (salience 99))
    (not (colors-found (v TRUE)))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s) (code $?pw))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(>= ?s 2))
    (test(eq ?last-s (- ?s 1)))
    (possible-colors (colors $?cols))
    =>
    (printout t "> make-guess " ?s crlf)

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
    (printout t (convert_code ?pw-n)" "?pw-n crlf)
)

(defrule rule-out-colors
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s) (code $?pw))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (guess-computer (step ?last-s-1) (code $?pw-1))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (test(> ?s 2))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    (test(eq 1 (length$ (difference $?pw-1 $?pw))))
    ?pc <- (possible-colors (colors $?cols))
    =>
    (printout t "> rule-out-colors" crlf)
    (bind ?score-0 (+ ?bp ?wp))
    (bind ?score-1 (+ ?bp-1 ?wp-1))
    (if (< ?score-0 ?score-1) then
        (bind ?ch (changed-digit-0 ?pw-1 ?pw))
    )
    (if (> ?score-0 ?score-1) then
        (bind ?ch (changed-digit-1 ?pw-1 ?pw))
    )
    (bind $?cols (delete-member$ $?cols ?ch))
    (modify ?pc (colors $?cols))

    (bind ?l (length$ $?cols))

    (if (<= ?l 4) then
        (assert (colors-found (v TRUE)))
    )

)

(defrule guess-order-1st
    (declare (salience 99))
    (not (found-1st (v TRUE)))
    (not (found-2nd (v TRUE)))
    (colors-found (v TRUE))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s) (code ?n2 ?n1 ?n3 ?n4))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    =>
    (printout t "> guess-order-1st-0" crlf)
    (if (> ?bp ?bp-1) then
        (printout t "> bp increase" crlf)
        (if (eq (- ?bp ?bp-1) 2) then
            (assert (found-1st (v TRUE)))
            (assert (found-2nd (v TRUE)))
            (bind ?pw (create$ ?n2 ?n1 ?n4 ?n3))
        else
            (bind ?pw (create$ ?n3 ?n1 ?n2 ?n4))
        )
    else
        (if (eq (- ?bp-1 ?bp) 2) then
            (assert (found-1st (v TRUE)))
            (assert (found-2nd (v TRUE)))
            (bind ?pw (create$ ?n1 ?n2 ?n4 ?n3))
        else
            (bind ?pw (create$ ?n1 ?n3 ?n2 ?n4))
        )
    )
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule guess-order-1st-0
    (declare (salience 99))
    (colors-found (v TRUE))
    (not (found-1st (v TRUE)))
    (not (found-2nd (v TRUE)))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s-1) (code ?n1 ?n2 ?n3 ?n4))
    (guess-computer (step ?last-s) (code ?n2 ?n1 ?n3 ?n4))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    =>
    (printout t "> guess-order-1st-0" crlf)
    (if (> ?bp ?bp-1) then
        (printout t "> bp increase" crlf)
        (if (eq (- ?bp ?bp-1) 2) then
            (assert (found-1st (v TRUE)))
            (assert (found-2nd (v TRUE)))
            (bind ?pw (create$ ?n2 ?n1 ?n4 ?n3))
        else
            (bind ?pw (create$ ?n3 ?n1 ?n2 ?n4))
        )
    else
        (if (eq (- ?bp-1 ?bp) 2) then
            (assert (found-1st (v TRUE)))
            (assert (found-2nd (v TRUE)))
            (bind ?pw (create$ ?n1 ?n2 ?n4 ?n3))
        else
            (bind ?pw (create$ ?n1 ?n3 ?n2 ?n4))
        )
    )
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule guess-order-1st-1
    (colors-found (v TRUE))
    (not (found-1st (v TRUE)))
    (not (found-3rd (v TRUE)))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s-1) (code ?n2 ?n1 ?n3 ?n4))
    (guess-computer (step ?last-s) (code ?n3 ?n1 ?n2 ?n4))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    =>
    (printout t "> guess-order-1st-1" crlf)
    (if (> ?bp ?bp-1) then
        (printout t "> bp increase" crlf)
        (if (eq (- ?bp ?bp-1) 2) then
            (assert (found-1st (v TRUE)))
            (assert (found-3rd (v TRUE)))
            (bind ?pw (create$ ?n3 ?n4 ?n2 ?n1))
        else
            (assert (found-1st (v TRUE)))
            (bind ?pw (create$ ?n1 ?n3 ?n2 ?n4))
        )
    else
        (if (eq (- ?bp-1 ?bp) 2) then
            (assert (found-1st (v TRUE)))
            (assert (found-3rd (v TRUE)))
            (bind ?pw (create$ ?n2 ?n1 ?n3 ?n4))
        else
            (bind ?pw (create$ ?n3 ?n1 ?n4 ?n2))
        )
    )
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule guess-order-3rd-0
    (colors-found (v TRUE))
    (found-1st (v TRUE))
    (not (found-2nd (v TRUE)))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s-1) (code ?n1 ?n2 ?n3 ?n4))
    (guess-computer (step ?last-s) (code ?n1 ?n3 ?n2 ?n4))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    =>
    (if (> ?bp ?bp-1) then
        (printout t "> bp increase" crlf)
        (assert (found-2nd (v TRUE)))
        (bind ?pw (create$ ?n1 ?n3 ?n4 ?n2))
    else
        (bind ?pw (create$ ?n1 ?n2 ?n4 ?n3))
    )
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t "> guess-order-3rd-0" crlf)
    (printout t (convert_code $?pw) " " $?pw crlf)

)

(defrule guess-order-3rd-1
    (colors-found (v TRUE))
    (found-1st (v TRUE))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s-1) (code ?n1 ?n2 ?n3 ?n4))
    (guess-computer (step ?last-s) (code ?n1 ?n2 ?n4 ?n3))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    =>
    (if (> ?bp ?bp-1) then
        (printout t "> bp increase" crlf)
        (assert (found-4th (v TRUE)))
        (bind ?pw (create$ ?n1 ?n4 ?n2 ?n3))
    else
        (bind ?pw (create$ ?n1 ?n2 ?n4 ?n3))
    )
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t "> guess-order-3rd-1" crlf)
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule guess-order-3rd-2
    (colors-found (v TRUE))
    (found-1st (v TRUE))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s-1) (code ?n1 ?n2 ?n3 ?n4))
    (guess-computer (step ?last-s) (code ?n1 ?n4 ?n2 ?n3))
    (answer (step ?last-s-1) (right-placed ?bp-1) (miss-placed ?wp-1))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    (test(eq ?last-s-1 (- ?last-s 1)))
    =>
    (bind ?pw (create$ ?n1 ?n2 ?n4 ?n3))
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t "> guess-order-3rd-2" crlf)
    (printout t (convert_code $?pw) " " $?pw crlf)
)

(defrule guess-order-last
    (colors-found (v TRUE))
    (found-1st (v TRUE))
    (found-2nd (v TRUE))
    (status (step ?s) (mode computer))
    (guess-computer (step ?last-s) (code ?n1 ?n2 ?n3 ?n4))
    (answer (step ?last-s) (right-placed ?bp) (miss-placed ?wp))
    (test(eq ?last-s (- ?s 1)))
    =>
    (bind ?pw (create$ ?n1 ?n2 ?n4 ?n3))
    (assert (guess-computer (step ?s) (code $?pw)))
    (assert (guess (step ?s) (g (convert_code $?pw))))
    (printout t "> guess-order-last" crlf)
    (printout t (convert_code $?pw) " " $?pw crlf)
)
