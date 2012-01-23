package scalax.collection
package mutable

import collection.generic.{CanBuildFrom, Growable, Shrinkable}
import collection.mutable.{Builder, Cloneable, ListBuffer, Set => MutableSet}

import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn,
                    NodeIn, NodeOut, EdgeIn, EdgeOut} 
import generic.{GraphCompanion, GraphFactory, MutableGraphFactory}
import GraphEdge.EdgeCompanionBase
import io._

protected[collection]
trait BuilderImpl [N,
                   E[X] <: EdgeLikeIn[X],
                   CC[N,E[X] <: EdgeLikeIn[X]] <: scalax.collection.Graph[N,E] with
                                                  scalax.collection.GraphLike[N,E,CC[N,E]]]
  extends Builder[GraphParam[N,E], CC[N,E]]
{
  protected type This = CC[N,E]
  protected val nodes = ListBuffer.empty[N]
  protected val edges = ListBuffer.empty[E[N]]
  protected def add(elem: GraphParam[N,E]) {
    elem match {
      case n: NodeIn [N] => nodes.+=:(n.value)
      case n: NodeOut[N] => nodes.+=:(n.value)
      case e: E[N]                 => edges.+=:(e)
      case e: GraphBase[N,E]#EdgeT => edges.+=:(e.toEdgeIn)
    }
  }
  override def +=(elem: GraphParam[N,E]): this.type = {
    add(elem)
    this
  }
  /* overridden for increased performance */
  override def ++=(elems: TraversableOnce[GraphParam[N,E]]): this.type = {
    elems foreach (add(_))
    this
  }
  def clear() { nodes.clear; edges.clear }
}
class GraphBuilder[N,
                   E[X] <: EdgeLikeIn[X],
                   CC[N,E[X] <: EdgeLikeIn[X]] <: scalax.collection.Graph[N,E] with
                                                  scalax.collection.GraphLike[N,E,CC[N,E]]]
      (factory: GraphFactory[CC]) 
  extends BuilderImpl[N,E,CC]
{
  def result: This = factory.from(nodes, edges)
}
/**
 * Trait with common mutable Graph methods.
 * 
 * @author Peter Empen
 */
trait GraphLike[N,
                E[X] <: EdgeLikeIn[X],
               +This <: GraphLike[N, E, This] with Graph[N,E]]
	extends	scalax.collection.GraphLike[N, E, This]
  with    GraphAux  [N,E]
	with	  Growable  [GraphParam[N,E]]
	with	  Shrinkable[GraphParam[N,E]] 
	with	  Cloneable [Graph[N,E]]
  with    EdgeOps   [N,E,This]
  with    Mutable
{
	override def clone: This =
    coreCompanion.getOrElse(throw new UnsupportedOperationException).
      from[N,E](nodes.toNodeInSet, edges.toEdgeInSet).asInstanceOf[This]
  /**
   * Populates this graph with the nodes and edges read from `nodeStream` and `edgeStream`.
   * 
   * @param nodeStreams List of node streams to be processed.
   * @param edgeStreams List of edge streams to be processed with each edge having its
   *        own edge factory. 
   */
  def from (nodeStream: NodeInputStream[N],
            edgeStream: GenEdgeInputStream[N,E])
  {
    from(Seq(nodeStream), Set.empty[N], Seq(edgeStream), Set.empty[E[N]])
  }
	type NodeT <: InnerNodeLike 
  trait InnerNodeLike extends super.InnerNodeLike with InnerNodeOps {
    this: NodeT =>
  }
  type NodeSetT <: NodeSet
	trait NodeSet extends MutableSet[NodeT] with super.NodeSet with NodeSetAux
	{
		@inline final override def -= (node: NodeT): this.type = { remove(node); this }
    @inline final def -?=         (node: NodeT): this.type = { removeGently(node); this }
    @inline final def -?          (node: NodeT)            = {
      val c = copy
      c subtract (node, false, minus, (NodeT) => {})
      c
    }
		override def remove(node: NodeT) = subtract(node, true,  minus, minusEdges)
    def removeGently   (node: NodeT) = subtract(node, false, minus, minusEdges)
    /**
     * removes all incident edges of `node` from the edge set leaving the node set unchanged.
     * @param node the node the incident edges of which are to be removed from the edge set.
     */
    protected def minusEdges(node: NodeT): Unit
	}
  type EdgeSetT <: EdgeSet
	trait EdgeSet extends MutableSet[EdgeT] with super.EdgeSet with EdgeSetAux {
    @inline final def += (edge: EdgeT): this.type = { add(edge); this }
    @inline final def -= (edge: EdgeT): this.type = { remove(edge); this }
    def removeWithNodes(edge: EdgeT): Boolean
  }

	override def ++=(xs: TraversableOnce[GraphParam[N,E]]): this.type = { xs foreach += ; this } 
	
  /** Adds a node to this graph.
   *
   *  @param n the node to be added
   *  @return `true` if the node was not yet present in the graph, `false` otherwise.
   */
  def add(node: N): Boolean
  /**
   * Adds the given node if not yet present and returns it as an inner node.
   * 
   * @param node the node to add.
   * @return inner node containing the added node.
   */
  @inline final def addAndGet(node: N): NodeT = { add(node); find(node) get }
	def +  (node: N) = if (nodes contains Node(node)) this.asInstanceOf[This]
	                   else clone += node
	@inline final def += (node: N): this.type = { add(node); this }
  def add(edge: E[N]): Boolean
  /**
   * Adds the given edge if not yet present and returns it as an inner edge.
   * 
   * @param edge the edge to add.
   * @return the inner edge containing the added edge.
   */
  @inline final def addAndGet(edge: E[N]): EdgeT = { add(edge); find(edge) get }
	@inline final protected def +#  (edge: E[N]) = clone +=# edge
	protected def +=#(edge: E[N]): this.type
	def += (elem: GraphParam[N,E]) =
    elem match {
      case n: NodeIn [N] => this += n.value	
      case n: NodeOut[N] => this += n.value
      case e: E[N]                 => this +=# e
      case e: GraphBase[N,E]#EdgeT => this +=# e.toEdgeIn
    }
	@inline final def     - (node: N) = clone -= node
  @inline final def remove(node: N) = nodes remove Node(node)
	@inline final def    -= (node: N): this.type = { remove(node); this }
  @inline final def    -?=(node: N): this.type = { removeGently(node); this }
  @inline final def    -? (node: N) = clone -?= node
  @inline final def removeGently(node: N) = nodes removeGently Node(node)

	@inline final           def -     (edge: E[N]) = clone -=# edge
  @inline final           def remove(edge: E[N]) = edges remove Edge(edge)
	@inline final protected def -=#   (edge: E[N]): this.type = { remove(edge); this }
  @inline final protected def -!=#  (edge: E[N]): this.type = { removeWithNodes(edge); this }
  @inline final           def -!    (edge: E[N]) = clone -!=# edge
  @inline final protected def -#    (edge: E[N]) = clone  -=# edge
  @inline final protected def -!#   (edge: E[N]) = clone -!=# edge
  @inline final def removeWithNodes (edge: E[N]) = edges removeWithNodes Edge(edge)

  def -= (elem: GraphParam[N,E]): this.type =
    elem match {
      case n: NodeIn [N] => this -= n.value 
      case n: NodeOut[N] => this -= n.value
      case e: E[N]                 => this -=# e
      case e: GraphBase[N,E]#EdgeT => this -=# e.toEdgeIn
  	}
  def -!=(elem: GraphParam[N,E]): this.type =
    elem match {
      case n: NodeIn [N] => this  -= n.value 
      case n: NodeT      => nodes -= n; this
      case n: NodeOut[N] => this  -= n.value
      case e: E[N]                 => this -!=# e
      case e: GraphBase[N,E]#EdgeT => this -!=# e.toEdgeIn
    }
  /**
   * Shrinks this graph to its intersection with `coll`.
   *
   * @param coll Collection of nodes and/or edges to intersect with;
   * @return this graph shrinked by the nodes and edges not contained in `coll`.
   */
  def &=(coll: Iterable[GraphParam[N,E]]): This = {
    val collSet = coll.toSet
    foreach {e => if(! collSet.contains(e)) this -= e}
    this.asInstanceOf[This]
  }
  /**
   * Removes all elements of `coll` from this graph. Edges will be
   * ripple removed.
   *
   * @param coll Collection of nodes and/or edges to be removed; if the element type is N,
   *             it is removed from the node set otherwise from the edge set.
   * @return this graph shrinked by the nodes and edges contained in `coll`.
   */
  @inline final
  def --!=(coll: Iterable[GraphParam[N,E]]): This =
    (this.asInstanceOf[This] /: coll)(_ -!= _) 
}
/**
 * The main trait for mutable graphs bundling the functionality of traits concerned with
 * specific aspects.
 * 
 * @tparam N the type of the nodes (vertices) in this graph.
 * @tparam E the kind of the edges in this graph. 
 *
 * @author Peter Empen
 */
trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.Graph[N,E]
	with	  GraphLike[N, E, Graph[N,E]]
{
	override def empty: Graph[N,E] = Graph.empty[N,E]
}
/**
 * The main companion object for mutable graphs.
 *
 * @author Peter Empen
 */
object Graph
  extends MutableGraphFactory[Graph]
  with    GraphAuxCompanion  [Graph]
{
	def empty[N, E[X] <: EdgeLikeIn[X]]: Graph[N,E] = new DefaultGraphImpl[N,E]
  override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                               edges: Iterable[E[N]]): Graph[N,E] =
    DefaultGraphImpl.from[N,E](nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): Graph[N,E] =
    DefaultGraphImpl.fromStream[N,E](nodeStreams, nodes, edgeStreams, edges)

  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]
	           : CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
		new GraphCanBuildFrom[N,E]
}
@SerialVersionUID(73L)
class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
    ( iniNodes: Iterable[N]    = Set[N](),
      iniEdges: Iterable[E[N]] = Set[E[N]]() )
  extends Graph[N,E]
  with    AdjacencyListGraph[N, E, DefaultGraphImpl[N,E]]
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
  @inline final override def empty: DefaultGraphImpl[N,E] = DefaultGraphImpl.empty[N,E]
  @inline final override def clone(): this.type = super.clone.asInstanceOf[this.type]
                                                                                                                                
  @SerialVersionUID(7370L)
  protected class NodeBase(override val value: N)
    extends super.NodeBase(value)
    with    InnerNodeImpl
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNode(n: N) = new NodeT(n)
}
object DefaultGraphImpl
  extends MutableGraphFactory[DefaultGraphImpl]
  with    GraphAuxCompanion  [DefaultGraphImpl]
{
	def empty[N, E[X] <: EdgeLikeIn[X]] = new DefaultGraphImpl[N,E]
  override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                               edges: Iterable[E[N]]) =
    new DefaultGraphImpl[N,E](nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): DefaultGraphImpl[N,E] =
    new DefaultGraphImpl[N,E](nodeStreams, nodes, edgeStreams, edges)

  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]
	           : CanBuildFrom[Coll, GraphParamIn[N,E], DefaultGraphImpl[N,E]] =
		new GraphCanBuildFrom[N,E]
}