package scalax.collection.constrained
package immutable

import collection.{Set, Iterable}
import collection.generic.CanBuildFrom

import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn} 
import scalax.collection.GraphTraversalImpl
import scalax.collection.io._
import scalax.collection.constrained.{Graph => CGraph}
import scalax.collection.constrained.generic.ImmutableGraphFactory
import PreCheckFollowUp._
    
trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.immutable.Graph[N,E]
	with    scalax.collection.constrained.Graph[N,E]
	with	  GraphLike[N, E, Graph[N,E]] 
{
	override def empty: Graph[N,E] = Graph.empty(constraintFactory)
}
object Graph
  extends ImmutableGraphFactory[Graph]
  with    GraphAuxCompanion[Graph]
{
	override def empty[N, E[X] <: EdgeLikeIn[X]]
	                  (cFactory: ConstraintCompanion[Constraint]): Graph[N,E]
	  = DefaultGraphImpl.empty[N,E](cFactory)
  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                              (nodes:    Iterable[N],
                                               edges:    Iterable[E[N]]): DefaultGraphImpl[N,E]
    = DefaultGraphImpl.fromUnchecked[N,E](cFactory)(nodes, edges)
  override def from [N, E[X] <: EdgeLikeIn[X]]
                    (cFactory: ConstraintCompanion[Constraint])
                    (nodes: Iterable[N],
                     edges: Iterable[E[N]]): Graph[N,E]
    = DefaultGraphImpl.from[N,E](cFactory)(nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Seq[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]             = Seq.empty[N],
      edgeStreams: Seq[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]          = Seq.empty[E[N]]): Graph[N,E]
    = DefaultGraphImpl.fromStream[N,E](cFactory)(
                                     nodeStreams, nodes, edgeStreams, edges)
//	implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]
//    : CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] = new GraphCanBuildFrom[N,E]
}
abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
                               (iniNodes: Iterable[N]    = Set.empty[N],
                                iniEdges: Iterable[E[N]] = Set.empty[E[N]])
  extends Graph[N,E]
  with    AdjacencyListGraph[N,E,DefaultGraphImpl[N,E]]
  with    GraphTraversalImpl[N,E]
{
  protected val _nodes = new NodeSet 
  protected val _edges = new EdgeSet
  initialize(iniNodes, iniEdges)

  @inline final override def empty: DefaultGraphImpl[N,E] =
    DefaultGraphImpl.empty(constraintFactory)
  @inline final override def clone: DefaultGraphImpl[N,E] = {
    DefaultGraphImpl.fromUnchecked(constraintFactory)(_nodes.toNodeInSet, _edges.toEdgeInSet)
  }
  @SerialVersionUID(8081L)
  final protected class NodeBase(override val value: N)
    extends super.NodeBase(value)
    with    InnerNodeImpl
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNode(n: N) = new NodeT(n)
}
object DefaultGraphImpl
  extends ImmutableGraphFactory[DefaultGraphImpl]
  with    GraphAuxCompanion    [DefaultGraphImpl]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    = from (cFactory)(Set.empty[N], Set.empty[E[N]])

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                              (nodes:    Iterable[N],
                                               edges:    Iterable[E[N]]): DefaultGraphImpl[N,E]
    = new UserConstrainedGraphImpl[N,E](cFactory)(nodes, edges)

  override def from [N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                               (nodes: Iterable[N],
                                                edges: Iterable[E[N]]): DefaultGraphImpl[N,E] =
  { val existElems = nodes.nonEmpty || edges.nonEmpty
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (nodes, edges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = fromUnchecked[N,E](cFactory)(nodes, edges)
    if (existElems) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      var handle = false
      preCheckResult.followUp match {
        case Complete  =>
        case PostCheck => handle = ! constraint.postAdd(newGraph, nodes, edges, preCheckResult)
        case Abort     => handle = true
      }
      if (handle) {
        constraint.onAdditionRefused(nodes, edges, newGraph)
        emptyGraph
      } else
        newGraph
    } else
      newGraph
  }
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Seq[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      iniNodes:    Iterable[N]             = Seq.empty[N],
      edgeStreams: Seq[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      iniEdges:    Iterable[E[N]]          = Seq.empty[E[N]]): DefaultGraphImpl[N,E] =
  {
    var preCheckResult = PreCheckResult(Abort)
    if (iniNodes.nonEmpty || iniEdges.nonEmpty) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      preCheckResult = constraint.preCreate(iniNodes, iniEdges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (iniNodes, iniEdges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = new UserConstrainedGraphImpl[N,E](cFactory)() {
      from(nodeStreams, iniNodes, edgeStreams, iniEdges)
    }
    var handle = false
    preCheckResult.followUp match {
      case Complete  =>
      case PostCheck => handle = ! newGraph.postAdd(newGraph, iniNodes, iniEdges, preCheckResult)
      case Abort     => handle = true
    }
    if (handle) {
      newGraph.onAdditionRefused(iniNodes, iniEdges, newGraph)
      empty[N,E](cFactory)
    } else
      newGraph
  }
  //implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]
  //  : CanBuildFrom[Coll, GraphParamIn[N,E], DefaultGraphImpl[N,E]] = new GraphCanBuildFrom[N,E]
}
@SerialVersionUID(-73L)
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]]
                              (cFactory: ConstraintCompanion[Constraint])
                              (iniNodes: Iterable[N]    = Set.empty[N],
                               iniEdges: Iterable[E[N]] = Set.empty[E[N]])
  extends DefaultGraphImpl    [N,E](iniNodes, iniEdges)
  with    UserConstrainedGraph[N,E]
{
  final override def graphCompanion = DefaultGraphImpl
  final override val self = this
  final override val constraintFactory = cFactory
  final override val constraint  = cFactory(this)
  final override def copy(nodes: Iterable[N],
                          edges: Iterable[E[N]]) =
    DefaultGraphImpl.from(constraintFactory)(nodes, edges)
}
