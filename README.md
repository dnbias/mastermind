# Mastermind Player in CLIPS
A CLIPS implementation of possible human tactics to the Mastermind game.
## rules
- code is 4 different colors
- colors are 8:
  - blue
  - green
  - red
  - yellow
  - orange
  - white
  - black
  - purple
- player has 10 tries to guess correctly
- each turn the system tells the player how many colors where guessed in the correct position and how many in the incorrect one
- the guess cannot contain repeated colors, if this happens the guess is consumed with no response
## run
'''
> ./clips
CLIPS> (load "./0_Main.clp")
CLIPS> (load "./1_Game.clp")
CLIPS> (load "./2_Code.clp")
CLIPS> (load "./3_Agent.clp")
CLIPS> (reset)
CLIPS> (run)
'''

or 
'''
>./clips -f go.bat
CLIPS> (run)
'''
