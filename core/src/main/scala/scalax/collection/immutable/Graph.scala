package scalax.collection
package immutable

import collection.Set
import collection.generic.CanBuildFrom

import GraphEdge.{EdgeLike, EdgeCompanionBase}
import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn} 
import generic.{GraphCompanion, GraphFactory, ImmutableGraphFactory, MutableGraphFactory}
import mutable.GraphBuilder
import io._

trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.Graph[N,E]
	with	GraphLike[N, E, Graph[N,E]] 
{
	override def empty: Graph[N,E] = Graph.empty[N,E]
}
object Graph
  extends ImmutableGraphFactory[Graph]
  with    GraphAuxCompanion    [Graph]
{
	def empty[N, E[X] <: EdgeLikeIn[X]]: Graph[N,E] = DefaultGraphImpl.empty[N,E]
  override def from [N, E[X] <: EdgeLikeIn[X]]
              (nodes: Iterable[N],
               edges: Iterable[E[N]]): Graph[N,E] = DefaultGraphImpl.from[N,E](nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): Graph[N,E] =
    DefaultGraphImpl.fromStream[N,E](nodeStreams, nodes, edgeStreams, edges)
	implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
	  new GraphCanBuildFrom[N,E]
}
@SerialVersionUID(70L)
class TinyGraphImpl[N, E[X] <: EdgeLikeIn[X]]
		(	iniNodes: Iterable[N]    = Set[N](),
			iniEdges: Iterable[E[N]] = Set[E[N]]() )
  extends Graph[N,E]
  with    SetsOnlyGraph[N,E,TinyGraphImpl[N,E]]
  with    GraphTraversalImpl[N,E]
{
  override def graphCompanion = TinyGraphImpl
  protected val _nodes = new NodeSet 
  protected val _edges = new EdgeSet
  initialize(iniNodes, iniEdges)

  def this (nodeStreams: Iterable[NodeInputStream[N]],
            nodes:       Iterable[N],
            edgeStreams: Iterable[GenEdgeInputStream[N,E]],
            edges:       Iterable[E[N]]) = {
    this()
    from(nodeStreams, nodes, edgeStreams, edges)
  }

  override protected[this] def newBuilder = new GraphBuilder[N,E,TinyGraphImpl](TinyGraphImpl)
  @inline final override def empty: TinyGraphImpl[N,E] =
    TinyGraphImpl.empty[N,E]
  @inline final override def clone: TinyGraphImpl[N,E] =
    TinyGraphImpl.from [N,E](_nodes.toNodeInSet,
                             _edges.toEdgeInSet)
  @inline final override def copy(nodes: Iterable[N],
                                  edges: Iterable[E[N]]) =
    TinyGraphImpl.from[N,E](nodes, edges)
  @SerialVersionUID(7070L)
  protected class NodeBase(override val value: N)
    extends super.NodeBase(value)
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNode(n: N) = new NodeT(n)
}
object TinyGraphImpl
  extends ImmutableGraphFactory[TinyGraphImpl]
  with    GraphAuxCompanion    [TinyGraphImpl]
{
	def empty[N, E[X] <: EdgeLikeIn[X]] = new TinyGraphImpl[N,E]
	override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
	                                             edges: Iterable[E[N]])=
    new TinyGraphImpl[N,E](nodes, edges)
  override def fromStream[N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): TinyGraphImpl[N,E] =
    new TinyGraphImpl[N,E](nodeStreams, nodes, edgeStreams, edges)
	implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], TinyGraphImpl[N,E]] =
		new GraphCanBuildFrom[N,E]
}
@SerialVersionUID(71L)
class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
    ( iniNodes: Iterable[N]    = Set[N](),
      iniEdges: Iterable[E[N]] = Set[E[N]]() )
  extends Graph[N,E]
  with    AdjacencyListGraph[N,E,DefaultGraphImpl[N,E]]
  with    GraphTraversalImpl[N,E]
{
  override def graphCompanion = DefaultGraphImpl
  protected val _nodes = new NodeSet 
  protected val _edges = new EdgeSet
  initialize(iniNodes, iniEdges)

  def this (nodeStreams: Iterable[NodeInputStream[N]],
            nodes:       Iterable[N],
            edgeStreams: Iterable[GenEdgeInputStream[N,E]],
            edges:       Iterable[E[N]]) = {
    this()
    from(nodeStreams, nodes, edgeStreams, edges)
  }
  override protected[this] def newBuilder = new GraphBuilder[N,E,DefaultGraphImpl](DefaultGraphImpl)
  @inline final override def empty: DefaultGraphImpl[N,E] =
    DefaultGraphImpl.empty[N,E]
  @inline final override def clone: DefaultGraphImpl[N,E] =
    DefaultGraphImpl.from [N,E](_nodes.toNodeInSet,
                              _edges.toEdgeInSet)
  @inline final override def copy(nodes: Iterable[N],
                                  edges: Iterable[E[N]])=
    DefaultGraphImpl.from[N,E](nodes, edges)
  @SerialVersionUID(7170L)
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
  override def empty[N, E[X] <: EdgeLikeIn[X]] = new DefaultGraphImpl[N,E]
  override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                               edges: Iterable[E[N]])=
    new DefaultGraphImpl[N,E](nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): DefaultGraphImpl[N,E] =
    new DefaultGraphImpl[N,E](nodeStreams, nodes, edgeStreams, edges)
  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], DefaultGraphImpl[N,E]] =
    new GraphCanBuildFrom[N,E]
}
