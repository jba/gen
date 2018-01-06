package geo

type Point struct {
	X, Y int
}

func (p1 Point) Less(p2 Point) bool {
	return p1.X < p2.X || (p1.X == p2.X && p1.Y < p2.Y)
}

func (p1 Point) Equal(p2 Point) bool { return p1 == p2 }

func (p Point) Hash() uint64 {
	return uint64(p.X*33 + p.Y)
}
