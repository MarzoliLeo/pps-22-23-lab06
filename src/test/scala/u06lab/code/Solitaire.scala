package u06lab.solutions

import org.junit.Test
import org.junit.Assert.*
import ulab06.solution.Solitaire
import org.junit.Test
import org.junit.Assert.*
import ulab06.solution.Solitaire.{Position, isSafe}

enum Content:
  case Empty    // X = Empty
  case Number   // N = Number
  case Safe     // S = Safe for N

class Board(val solution: Map[Position, Content])

object Board:
  def apply(contents: Seq[Seq[Content]]): Board =
    val solution = contents.zipWithIndex.flatMap { (row, y) =>
      row.zipWithIndex.map { (content, x) =>
        ((x, y), content)
      }
    }.toMap
    new Board(solution)

class SolitaireTest:

  @Test def logicMoveTest =
    val center = (2, 3)
    val board = Board(Seq(
      Seq(Content.Empty, Content.Safe, Content.Empty, Content.Empty, Content.Empty, Content.Safe, Content.Empty),
      Seq(Content.Empty, Content.Empty, Content.Empty, Content.Empty, Content.Empty, Content.Empty, Content.Empty),
      Seq(Content.Safe, Content.Empty, Content.Empty, Content.Number, Content.Empty, Content.Empty, Content.Safe),
      Seq(Content.Empty, Content.Empty, Content.Empty, Content.Empty, Content.Empty, Content.Empty, Content.Empty),
      Seq(Content.Empty, Content.Safe, Content.Empty, Content.Empty, Content.Empty, Content.Safe, Content.Empty)
    ))

    board.solution.keySet.foreach { p =>
      if board.solution(p) == Content.Safe then assertTrue(isSafe(p, center))
      else assertFalse(isSafe(p, center))
    }



