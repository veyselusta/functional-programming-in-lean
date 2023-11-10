/- 1.4 Structures -/
#check 1.2
#check -454.2123215
#check 0.0

structure Point where
  x : Float
  y : Float
deriving Repr

def origin : Point := { x := 0.0, y := 0.0 }

#eval origin
#check origin

#eval origin.x
#eval origin.y

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y:= p1.y + p2.y }

#eval addPoints { x := 1.5, y := 2.0 } { x := 4.2, y := 2.4 }

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt (((p2.x - p1.x) ^ 2.0) + ((p2.y - p1.y) ^ 2.0))

#eval distance { x := 1.0, y := 2.0 } { x := 5.0, y := -1.0 }

structure Point3D where
  x : Float
  y : Float
  z : Float
deriving Repr

def origin3D : Point3D := { x := 0.0, y := 0.0, z := 0.0 }

#check { x := 0.0, y := 0.0 : Point}

/- Updating Structures -/
def zeroX (p : Point) : Point :=
  { p with x := 0 }

def fourAndThree : Point :=
  { x := 4.3, y := 3.4 }

#eval fourAndThree
#eval zeroX fourAndThree
#eval fourAndThree

#check Point.mk 1.5 2.8
#check (Point.mk)

structure Point2 where
  point2 ::
  x : Float
  y : Float
deriving Repr

#check (Point2.point2)

#check (Point.x)
#check (Point.y)

#eval origin.x
#eval Point.x origin

#eval "one string".append " and another"

def Point.modifyBoth (f : Float → Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

#eval { x := 4.3, y := 3.4 : Point}.modifyBoth Float.floor

/- Exercies -/

/- 1 -/
structure RectangularPrism where
  height : Float
  width : Float
  depth : Float
deriving Repr

/- 2 -/
def volumeOfPrism (p : RectangularPrism) : Float :=
  p.height * p.width * p.depth

#eval volumeOfPrism { height := 1.0, width := 2.0, depth := 3.0 }

/- 3 -/
structure Segment where
  starting : Float
  ending : Float
deriving Repr

def length (s: Segment) : Float :=
  s.ending - s.starting

#eval length {starting := 0.0, ending := 3.14}

/- 4 -/
/- mk, height, width, depth-/

/- 5 -/
structure Hamster where
  name : String
  fluffy : Bool

structure Book where
  makeBook ::
  title : String
  author : String
  price : Float

/- Hamster = mk, name, fluffy -/
/- Book = makebook, author, title, price -/
