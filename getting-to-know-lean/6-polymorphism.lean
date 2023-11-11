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
