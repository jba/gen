package geo

type Point struct {
	X, Y int
}

func (p1 Point) Less(p2 Point) bool {
	return p1.X < p2.X || (p1.X == p2.X && p1.Y < p2.Y)
}
