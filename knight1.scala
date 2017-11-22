// Part 1 about finding and counting Knight's tours
//==================================================

object CW7a {

  type Pos = (Int, Int)
  type Path = List[Pos]


  def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {


    if (path.contains(x)) false

    else if (x._1 > dim - 1)  false

    else if (x._2 > dim -1) false

    else if (x._1 < 0) false

    else if (x._2 < 0) false

    else true

  }



  def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

    val listOfAllMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))

    for(i <- listOfAllMoves if is_legal(dim, path)(i)) yield i



  }




//some test cases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(1c) Complete the two recursive functions below. 
//     They exhaustively search for knight's tours starting from the 
//     given path. The first function counts all possible tours, 
//     and the second collects all tours in a list of paths.

//def count_tours(dim: Int, path: Path) : Int = ...

//def enum_tours(dim: Int, path: Path) : List[Path] = ...


}
