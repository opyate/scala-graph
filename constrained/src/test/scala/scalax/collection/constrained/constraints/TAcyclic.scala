package scalax.collection.constrained
package constraints

import org.scalatest.{Suite, Suites}
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import PreCheckFollowUp._
import generic.GraphFactory

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TAcyclicRootTest
  extends Suites(
      new TAcyclic[immutable.Graph](immutable.Graph),
      new TAcyclic[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  import mutable.Graph
  import AcyclicWithException._

  def test_mutableAsyclic {
    val g = Graph(Acyclic)(1~>2, 2~>3)
    evaluating { g += 3~>1 } should produce [CycleException]
    g + 3~>4 should have size (7)
  }
}

class TAcyclic [CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC[N,E]]]
    (val factory: GraphFactory[CC])
  extends Suite
  with    ShouldMatchers
{
  import AcyclicWithException._

  def test_0(info : Informer) {
    info("factory = " + factory.getClass)
  }
  def test_addDiEdge {
    val g = factory(Acyclic)(1~>2, 2~>3)
    evaluating { g + 3~>1 } should produce [CycleException]
    g + 3~>4 should have size (7)
  }
  def test_addDiHyperEdge {
    val g = factory[Int,HyperEdge](Acyclic)(1~>2~>3, 2~>3~>4)
    evaluating { g + 4~>2 } should produce [CycleException]
    g + 1~>4 should have size (7)
  }
  def test_addUnDiEdge {
    val g = factory(Acyclic)(1~2, 2~3)
    evaluating { g + 3~1 } should produce [CycleException]
    g + 3~4 should have size (7)
  }
  // TODO: GraphTraversal findCycle
  def Xtest_addHyperEdge {
    val g = factory[Int,HyperEdge](Acyclic)(1~2~3, 3~4~5)
    evaluating { g + 4~2 } should produce [CycleException]
    evaluating { g + 1~4 } should produce [CycleException]
    g + 1~6 should have size (9)
  }
}
object AcyclicWithException {
  object Acyclic extends ConstraintCompanion[Acyclic] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
      new Acyclic[N,E] (self) {
        override def onAdditionRefused( refusedNodes: Iterable[N],
                                        refusedEdges: Iterable[E[N]],
                                        graph:        Graph[N,E]) =
        { throw new CycleException("Addition refused: " +
                    "nodes = " + refusedNodes + ", " +
                    "edges = " + refusedEdges)
        }
      }
  }
  class CycleException(msg: String) extends IllegalArgumentException(msg)
}