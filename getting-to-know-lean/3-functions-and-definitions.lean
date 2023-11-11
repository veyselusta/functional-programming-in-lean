/- 1.3 Functions and Definitions -/
def hello := "Hello"
def lean : String := "Lean"

#eval String.append hello (String.append " " lean)

/- Defining Functions -/
def add1 (n : Nat) : Nat := n + 1
#eval add1 7

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then k else n

#check maximum
#check (maximum)

#check String.append "hello"

 /- Using a function that returns a function to implement
  multiple-argument functions is called currying -/

/- Exercises -/
/-1-/
def joinStringsWith (first second third : String) : String :=
  String.append second (String.append first third)
#eval joinStringsWith "," "one" "and another"

/-2-/
#check joinStringsWith ":"

/-3-/
def volume (height width depth : Nat) : Nat :=
  height * width * depth

/- Defining Types -/
def Str : Type := String
def Str2 : Type := Str

#check Str
#check Str2

def astr : Str := "hello"
#check astr

/- Messages You May Meet -/
def NaturalNumber : Type := Nat
def thirtyEight : NaturalNumber := (38 : Nat)

abbrev N : Type := Nat
def thirtyNine : N := 39
