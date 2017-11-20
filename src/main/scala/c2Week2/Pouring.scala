package c2Week2

class Pouring(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  // Init state is all glasses empty
  val initState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val dif = state(from) min capacity(to) - state(to)
      state updated (from, state(from) - dif) updated (to, state(to) + dif)
    }
  }

  // Paths (sequences of moves)
  class Path(history: List[Move], val endState: State) {
    //def endState: State = history.foldRight(initState)((move, acc) => move change acc)
    //    private def trackState(xs: List[Move]): State = xs match {
    //      case Nil => initState
    //      case move :: xs1 => move change trackState(xs1)
    //    }
    def extend(move: Move): Path = new Path(move :: history, move change endState)

    override def toString: String = (history.reverse mkString "") + "-->" + endState
  }

  val glasses = 0 until capacity.length
  val moves: Seq[Move] =
    (for (g <- glasses) yield  Empty(g)) ++
    (for (g <- glasses) yield  Fill(g)) ++
    (for {
      from <- glasses
      to <- glasses
      if from != to
    } yield Pour(from, to))
  val initialPath = new Path(Nil, initState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initState))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
