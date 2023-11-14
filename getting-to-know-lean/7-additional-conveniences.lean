/- Additional Conveniences -/

/- Automatic Implicits Arguments -/
def length {α : Type} (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (length ys)

def length2 (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (length2 ys)

#check length2

/- Pattern-Matching Definitions -/
def length3 : List α → Nat
  | [] => 0
  | y :: ys => Nat.succ (length3 ys)

def drop : Nat → List α → List α
  | Nat.zero, xs => xs
  | _, [] => []
  | Nat.succ n, x :: xs => drop n xs

def fromOption (default : α) : Option α → α
  | none => default
  | some x => x

#eval (some "salmonberry").getD ""
#eval none.getD ""

/- Local Definitions -/
def unzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    (x :: (unzip xys).fst, y :: (unzip xys).snd)

def unzip2 : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped : List α × List β := unzip2 xys
    (x :: unzipped.fst, y :: unzipped.snd)

def unzip3 : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let (xs, ys) : List α × List β := unzip3 xys
    (x :: xs, y :: ys)

def reverse (xs : List α) : List α :=
  let rec helper : List α → List α → List α
    | [], soFar => soFar
    | y :: ys, soFar => helper ys (y :: soFar)
  helper xs []

#eval unzip [("a", 1), ("b", 2), ("c", 3)]
