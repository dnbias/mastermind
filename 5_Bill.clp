(defmodule BILL (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deftemplate guess-computer
    (slot step)
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
