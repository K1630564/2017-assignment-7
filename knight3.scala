// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

object CW7c {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(3a) Complete the function that calculates a list of onward
//     moves like in (1b) but orders them according to Warnsdorf’s 
//     rule. That means moves with the fewest legal onward moves 
//     should come first.

//def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = ..


//(3b) Complete the function that searches for a single *closed* 
//     tour using the ordered moves function.

//def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = ...


//(3c) Same as (3b) but searches for *non-closed* tours. However, 
//     you have to be careful to write a tail-recursive version as this 
//     function will be called with dimensions of up to 40 * 40.

//def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = ...


}
