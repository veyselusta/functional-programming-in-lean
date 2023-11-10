/- 1.5 Datatypes and Patterns -/
/-  Types such as structures that group together a collection
     of values are called product types. -/

/- Datatypes that allow choices are called sum types and datatypes
   that can include instances of themselves are called recursive
    datatypes.-/

/- Recursive sum types are called inductive datatypes,
   because mathematical induction may be used to prove
   statements about them. When programming, inductive
   datatypes are consumed through pattern matching
   and recursive functions.-/

inductive Bool2 where
  | false : Bool2
  | true : Bool2

inductive Nat2 where
  | zero : Nat2
  | succ (n : Nat2) : Nat2
deriving Repr

def three : Nat2 := Nat2.succ (Nat2.succ (Nat2.succ Nat2.zero))
#eval Nat2.succ three

/- Pattern Matching -/
def isZero (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => false

#eval isZero Nat.zero

def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k

#eval pred 5


def depth (p : Point3D) : Float :=
  match p with
  | { x:= h, y := w, z := d } => d

/- Pattern matching can be used with structures
   as well as with sum types. -/

def even (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (even k)

#eval even 10

/- This pattern of thought is typical for writing recursive
   functions on Nat. First, identify what to do for zero.
   Then, determine how to transform a result for an arbitrary Nat into
   a result for its successor, and apply this transformation
   to the result of the recursive call.
   This pattern is called structural recursion.-/

def plus (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => Nat.succ (plus n k')

def times (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => Nat.zero
  | Nat.succ k' => plus n (times n k')

def minus (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => pred (minus n k')
