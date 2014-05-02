namespace Canopy2048

type Move = Up | Down | Left | Right
type Cell = { Col:int; Row:int; Value:int }
type State = Cell list