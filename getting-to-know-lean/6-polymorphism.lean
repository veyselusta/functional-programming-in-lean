/- 1.6 Polymorphism -/
structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

def natOrigin : PPoint Nat :=
  { x := Nat.zero, y := Nat.zero }

def replaceX (α : Type) (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }

#check (replaceX)
#check replaceX Nat
#check replaceX Nat natOrigin
#check replaceX Nat natOrigin 5
#eval replaceX Nat natOrigin 5

inductive Sign where
  | pos
  | neg

def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)

/- Linked List -/
def primesUnder10 : List Nat := [2, 3, 5, 7]

inductive List2 (α : Type) where
  | nil : List2 α
  | cons : α → List2 α → List2 α
deriving Repr

def explicitPrimesUnder10 : List2 Nat :=
  List2.cons 2 (List2.cons 3 (List2.cons 5 (List2.cons 7 List2.nil)))

#eval explicitPrimesUnder10

def lengthMyVersion (a : Type) (xs : List a) : Nat :=
  match xs with
  | List.nil => 0
  | List.cons y ys => 1 + (lengthMyVersion a ys)

#eval lengthMyVersion Nat [1,2,3,4,5]

def lengthOrigin (α : Type) (xs : List α) : Nat :=
  match xs with
  | List.nil => Nat.zero
  | List.cons y ys => Nat.succ (lengthOrigin α ys)

#eval lengthOrigin Nat [1,2,3,4,5]

def lengthShorter (α : Type) (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (lengthShorter α ys)

#eval lengthShorter Nat [1,2,3,4,5]

/- Implicit Arguments -/
def lengthImplicit {α : Type} (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (lengthImplicit ys)

#eval lengthImplicit primesUnder10

/- In the standard library, Lean calls this function List.length,
   which means that the dot syntax that is used for structure field
   access can also be used to find the length of a list:-/
#eval primesUnder10.length

/- More Built-In Datatypes -/
/- In addition to lists, Lean's standard library contains
   a number of other structures and inductive datatypes that
   can be used in a variety of contexts. -/

/- Option -/
inductive Option2 (α : Type) : Type where
  | none : Option2 α
  | some (val : α) : Option2 α
deriving Repr

def List.head?? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => none
  | y :: _ => some y

#eval primesUnder10.head??
#check primesUnder10.head??

#eval [].head? (α := Int)
#eval ([] : List Int).head?

/- Prod -/
/- The Prod structure, short for "Product",
   is a generic way of joining two values together -/

structure Prod2 (α : Type) (β : Type) : Type where
  fst : α
  snd : β

def product : Prod2 Nat String := {fst := 2, snd := "hello" }

/- Lists are used so frequently that there is special syntax
   to make them more readable. For the same reason, both the
   product type and its constructor have special syntax -/

def fives : String × Int := { fst := "five", snd := 5 }
def fives2 : String × Int := ("five", 5)

def sevens : String × Int × Nat := ("VII", 7, 4 + 3)
def sevens2 : String × (Int × Nat) := ("VII", (7, 4 + 3))

/- In other words, all products of more than two types,
   and their corresponding constructors, are actually
   nested products and nested pairs behind the scenes. -/

/- Sum -/
/- The Sum datatype is a generic way of allowing a choice
   between values of two different types. -/

inductive Sum2 (α : Type) (β : Type) : Type where
  | inl : α → Sum2 α β
  | inr : β → Sum2 α β
deriving Repr

def foo : Sum2 Int String := Sum2.inl 2

def PetName0 : Type := Sum String String
def PetName : Type := String ⊕ String

#check foo
#check PetName0
#check PetName

def animals : List PetName :=
  [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi", Sum.inl "Rex", Sum.inr "Floof"]

def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | Sum.inl _ :: morePets => 1 + howManyDogs morePets
  | Sum.inr _ :: morePets => howManyDogs morePets

#eval howManyDogs animals

/- Unit -/
/- Unit is a type with just one argumentless constructor,
   called unit. -/

inductive Unit2 : Type where
  | unit : Unit2

inductive ArithExpr (ann : Type) : Type where
  | int : ann → Int → ArithExpr ann
  | plus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | minus : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann
  | times : ann → ArithExpr ann → ArithExpr ann → ArithExpr ann


/- Additionally, because all Lean functions have arguments,
   zero-argument functions in other languages can be represented
   as functions that take a Unit argument. In a return position,
   the Unit type is similar to void in languages derived from C.
   In the C family, a function that returns void will return control
   to its caller, but it will not return any interesting value.  -/

/- Empty -/
/- The Empty datatype has no constructors whatsoever.
   Thus, it indicates unreachable code, because no series of calls
   can ever terminate with a value at type Empty. -/

/- Naming: Sums, Products, and Units -/
/- Generally speaking, types that offer multiple constructors
   are called sum types, while types whose single constructor
   takes multiple arguments are called product types. -/

/- Messages You May Meet -/

inductive MyType (α : Type) : Type where
  | ctor : α → MyType α


inductive MyType2 (α : Type) : Type where
  | ctor : α → MyType2 α
deriving Repr

def ofFive : MyType2 Int := MyType2.ctor 5

/- Exercises -/
/- 1 -/
/- Write a function to find the last entry in a list. It should return an Option.-/
def lastEntry (list : List α) : Option α :=
  match list with
  | [] => none
  | [y] => some y
  | y :: ys => lastEntry ys

#eval lastEntry [1,2,3]

/- 2 -/
/- Write a function that finds the first entry in a list that satisfies
   a given predicate. Start the definition with
   def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α := -/

def findFirst? {α  : Type} (xs : List α) (predicate : α -> Bool) : Option α :=
 match xs with
 | [] => none
 | x :: xs' => if predicate x then some x else findFirst? xs' predicate


/- 3 -/
/- Write a function Prod.swap that swaps the two fields in a pair.
   Start the definition with
   def Prod.swap {α β : Type} (pair : α × β) : β × α := -/

def Prod.swap {α β : Type} (p : Prod α β) : Prod β α :=
{ fst := p.snd, snd := p.fst }

#eval Prod.swap {fst := "hello", snd := 5}

/- 4 -/
/- Rewrite the PetName example to use a custom datatype
   and compare it to the version that uses Sum. -/

inductive Animal where
  | Dog : String → Animal
  | Cat : String → Animal

def animal : List Animal := [ Animal.Dog "Spot", Animal.Cat "Garfield", Animal.Dog "Rex"]

def howManyDogs2 (animals : List Animal) : Nat :=
  match animals with
  | [] => 0
  | Animal.Dog _ :: rest => 1 + howManyDogs2 rest
  | Animal.Cat _ :: rest => howManyDogs2 rest

  #eval howManyDogs2 animal

/- 5 -/
