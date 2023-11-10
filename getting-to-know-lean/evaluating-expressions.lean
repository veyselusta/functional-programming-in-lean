/- 1.1 Evaluating Expressions -/
#eval 1 + 2
#eval 1 + 2 * 5

#eval String.append "Hello" "lean"

#eval String.append "recursive"
   (String.append "function, " (String.append "application" ""))

#eval String.append "it is " (if 1 > 2 then "yes" else "no")

/- Exercises-/
#eval 42 + 19
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"
#eval if 3 == 3 then 5 else 7
#eval if 3 == 4 then "equal" else "not equal"
