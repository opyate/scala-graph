package scalax.collection

import org.scalatest.Suite
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import collection.Set
import collection.immutable.{Range, SortedSet}
import collection.mutable.{Set => MutableSet}

import GraphPredef._, GraphEdge._
import generic.GraphFactory

import edge._, edge.WBase._, edge.LBase._, edge.WLBase._
import io._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TCycleRootTest
  extends Suites(
      new TCycle[immutable.Graph](immutable.Graph),
      new TCycle[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  // ---------------------------------------- mutable tests
}
/**	This class contains tests for implementing node/edge input streams
 *  and using them for graph creation.
 */
class TCycle[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC[N,E]]]
    (val factory: GraphFactory[CC] with GraphAuxCompanion[CC])
	extends	Suite
	with	ShouldMatchers
{
  // ----------------------------------------------------------------- directed
  val acyclic_1 = factory(1 ~> 2, 1 ~> 3, 2 ~> 3, 3 ~> 4)
  val cyclic_1  = acyclic_1 + 4 ~> 2

  val acyclic_2 = factory(1~>2, 1~>3, 1~>4, 1~>5, 2~>3, 3~>7, 7~>4, 7~>8, 4~>5, 5~>6)
  val cyclic_21 = acyclic_2 + 8~>3
  val cyclic_22 = acyclic_2 + 6~>1

  def c_1 (outer: Int) = cyclic_1  get outer
  def c_21(outer: Int) = cyclic_21 get outer
  def c_22(outer: Int) = cyclic_22 get outer

  def test_nodeFindCycle {
    (acyclic_1 get 1 findCycle) should be (None)
    c_1(2).findCycle.get.nodes  should be (List(2, 3, 4, 2) map (c_1(_)))

    (acyclic_2 get 1 findCycle) should be (None)
    c_21(1).findCycle.get.nodes should be (List(3, 7, 8, 3) map (c_21(_)))
    c_22(1).findCycle.get.nodes should be (List(1, 5, 6, 1) map (c_22(_)))
    c_22(4).findCycle.get.nodes should be (List(5, 6, 1, 5) map (c_22(_)))
  }
  def test_findCycle {
    acyclic_1.findCycle            should be (None)
     cyclic_1.findCycle.get.nodes  should have size (4)

    acyclic_2 .findCycle           should be (None)
     cyclic_21.findCycle.get.nodes should have size (4)
     cyclic_22.findCycle.get.nodes should have size (4)
     cyclic_22.findCycle.get.nodes should have size (4)
  }
  def test_isCyclic {
    acyclic_1.isCyclic  should be (false)
     cyclic_1.isCyclic  should be (true)

    acyclic_2 .isCyclic should be (false)
     cyclic_21.isCyclic should be (true)
     cyclic_22.isCyclic should be (true)
     cyclic_22.isCyclic should be (true)
  }
  // --------------------------------------------------------------- undirected
  val unDiAcyclic_1 = factory(1~2, 2~3)
  val unDiCyclic_1  = unDiAcyclic_1 + 1~3
  
  val unDiAcyclic_2 = Graph(1~2, 1~3, 2~4, 2~5)
  val unDiCyclic_21 = unDiAcyclic_2 + 3~5
  val unDiCyclic_22 = unDiAcyclic_2 ++ List(3~6, 6~7, 7~4)

  def uc_1 (outer: Int) = unDiCyclic_1   get outer
  def uc_21(outer: Int) = unDiCyclic_21  get outer
  def uc_22(outer: Int) = unDiCyclic_22  get outer

  def test_UnDiNodeFindCycle {
    (unDiAcyclic_1 get 1 findCycle) should be (None)
    uc_1(2).findCycle.get.nodes  should be (List(2, 3, 1, 2) map (uc_1(_)))

    (unDiAcyclic_2 get 1 findCycle) should be (None)
    uc_21(1).findCycle.get.nodes    should be (List(1, 3, 5, 2, 1) map (uc_21(_)))

    uc_22(3).findCycle.get.nodes    should be (List(3, 1, 2, 4, 7, 6, 3) map (uc_22(_)))
  }
}