// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

object CW7c {

  type Pos = (Int, Int)    // a position on a chessboard
  type Path = List[Pos]    // a path...a list of positions

  def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {


    if (path.contains(x)) false

    else if (x._1 > dim - 1)  false

    else if (x._2 > dim -1) false

    else if (x._1 < 0) false

    else if (x._2 < 0) false

    else true

  }

  def is_legal_closed(dim: Int, path: Path)(x: Pos) : Boolean = {


    if (x._1 > dim - 1)  false

    else if (x._2 > dim -1) false

    else if (x._1 < 0) false

    else if (x._2 < 0) false

    else true

  }



  def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

    val listOfAllMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))

    for(i <- listOfAllMoves if is_legal(dim, path)(i)) yield i



  }

  def legal_moves_closed(dim: Int, path: Path, x: Pos) : List[Pos] = {

    val listOfAllMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))

    for(i <- listOfAllMoves if is_legal_closed(dim, path)(i)) yield i



  }

  def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {

    legal_moves(dim, path, x).sortBy(legal_moves(dim,path,_).length)

  }


  def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {

    if(xs.isEmpty) None

    else{

      val head = f(xs.head)

      if(head != None) head

      else first(xs.drop(1), f)
    }

  }




  def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {

    if(path.length == dim*dim && legal_moves_closed(dim, path, path.head).contains(path.last)){
      Some(path)
    }
    else{

      val recFunc = (x: Pos) => first_closed_tour_heuristic(dim, x::path)

      first(ordered_moves(dim, path, path.head), recFunc)

    }
  }



  def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {

    if(path.length == dim*dim){
      Some(path)
    }
    else{

      val recFunc = (x: Pos) => first_tour_heuristic(dim, x::path)

      first(ordered_moves(dim, path, path.head), recFunc)

    }


  }


}
