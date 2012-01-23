package scalax.collection

import collection.{SetLike, GenTraversableOnce}
import collection.mutable.{Set => MSet}
import collection.generic.GenericCompanion

import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, NodeIn, NodeOut, EdgeIn, EdgeOut}
import GraphEdge.{EdgeLike, EdgeCompanionBase}
import generic.{GraphCompanion, GraphFactory}
import io._

/**
 * A template trait for graphs.
 * 
 * This trait provides the common structure and operations of immutable graphs independently of its representation.
 * 
 * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
 * 
 * @tparam N    the user type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges (links) in this graph.
 * @tparam This the type of the graph itself.
 *
 * @define REIMPLFACTORY Note that this method must be reimplemented in each module
 *         having its own factory methods such as `constrained` does.
 * @author Peter Empen
 */
trait GraphLike[N,
                E[X]  <: EdgeLikeIn[X],
                +This <: GraphLike[N,E,This] with Set[GraphParam[N,E]] with Graph[N,E]]
  extends SetLike       [GraphParam[N,E], This]
  with    GraphTraversal[N,E]
  with    GraphBase     [N,E]
  with    GraphDegree   [N,E]
{ selfGraph =>
  protected type ThisGraph = this.type
  /** The companion object of this `Graph` instance. */
  def graphCompanion: GraphCompanion
  /** Downcasts `graphCompanion` to a companion of the core module. */
  final def coreCompanion: Option[GraphFactory[Graph]] =
    Option(graphCompanion match { case f: GraphFactory[Graph] => f
                                  case _ => null })
  override def stringPrefix: String = "Graph"
  /**
   * Ensures sorted nodes/edges unless this `Graph` has more than 64 elements.
   * See also `toSortedString`.
   */
  override def toString = if (size <= 64) toSortedString()
                          else super.toString
  /**
   * Concatenates all elements of the node and edge set respectively
   * in alphabetic order of their string representation. 
   * 
   * @param nodeSeparator to separate nodes by
   * @param edgeSeparator to separate edges by
   * @param nodesEdgesSeparator to separate nodes from edges by
   * @param withNodesEdgesPrefix whether the node and edge set are to prefix
   */
  def asSortedString (nodeSeparator       : String  = GraphBase.defaultSeparator,
                      edgeSeparator       : String  = GraphBase.defaultSeparator,
                      nodesEdgesSeparator : String  = GraphBase.defaultSeparator,
                      withNodesEdgesPrefix: Boolean = false) =
  { val ns = if (withNodesEdgesPrefix) nodes.toSortedString(nodeSeparator)
             else                      nodes.asSortedString(nodeSeparator)
    val es = if (withNodesEdgesPrefix) edges.toSortedString(edgeSeparator)
             else                      edges.asSortedString(edgeSeparator)
    ns + (if (ns.length > 0 && es.length > 0) nodesEdgesSeparator
          else                                "") +
    es
  }
  /**
   * Concatenates all elements of the node and edge set respectively
   * in alphabetic order of their string representation and
   * prefixes and parenthesizes the result with `stringPrefix`.  
   * 
   * @param nodeSeparator to separate nodes by
   * @param edgeSeparator to separate edges by
   * @param nodesEdgesSeparator to separate nodes from edges by
   * @param withNodesEdgesPrefix whether the node and edge set are to prefix
   */
  def toSortedString (nodeSeparator       : String  = GraphBase.defaultSeparator,
                      edgeSeparator       : String  = GraphBase.defaultSeparator,
                      nodesEdgesSeparator : String  = GraphBase.defaultSeparator,
                      withNodesEdgesPrefix: Boolean = false) =
  { stringPrefix +
    "(" + asSortedString(nodeSeparator,
                         edgeSeparator,
                         nodesEdgesSeparator,
                         withNodesEdgesPrefix) +
    ")"
  }
  /**
   * `Graph` instances are equal if their nodes and edges turned
   * to outer nodes and outer edges are equal. Any `TraversableOnce`
   * instance may also be equal to this graph if its set representation
   * contains equalling outer nodes and outer edges. Thus the following
   * expressions hold:
   * {{{
   * Graph(1~2, 3) == List(1~2, 3)
   * Graph(1~2, 3) == List(1, 2, 2, 3, 2~1)
   * }}}
   * The first test is `false` because of the failing nodes `1` and `2`.
   * The second is true because of duplicate elimination and undirected edge equivalence.   
   */
  override def equals(that: Any): Boolean = that match {
    case that: Graph[N,E] =>
      (this eq that) ||
      (this.order     == that.order    ) &&
      (this.graphSize == that.graphSize) &&
      { val thatNodes = that.nodes.toNodeInSet 
        try this.nodes forall (thisN => thatNodes(thisN.value))
        catch { case _: ClassCastException => false }          } &&
      { val thatEdges = that.edges.toEdgeInSet 
        try this.edges forall (thisE => thatEdges(thisE.toEdgeIn))
        catch { case _: ClassCastException => false }          }
    case that: TraversableOnce[_] =>
      val thatSet = that.toSet
      (this.size == thatSet.size) &&
      { val thatNodes = thatSet.asInstanceOf[Set[N]]
        try this.nodes forall (thisN => thatNodes(thisN.value))
        catch { case _: ClassCastException => false }          } &&
      { val thatEdges = thatSet.asInstanceOf[Set[E[N]]]
        try this.edges forall (thisE => thatEdges(thisE.toEdgeIn))
        catch { case _: ClassCastException => false }          }
    case _ =>
      false
  }
  type NodeT <: InnerNodeLike 
  trait InnerNodeLike extends super.InnerNodeLike {
    /** The `Graph` instance `this` node is contained in. */
    def containingGraph: ThisGraph = selfGraph.asInstanceOf[ThisGraph]
  }
  protected abstract class NodeBase(override val value: N)
    extends super.NodeBase
    with    NodeOut[N]
    with    InnerNodeLike

  type NodeSetT <: NodeSet
  trait NodeSet extends super.NodeSet {
    def copy: NodeSetT
    override final def -(node: NodeT) = { val c = copy; c minus node; c }
    /**
     * removes `node` from this node set leaving the edge set unchanged.
     * @param node the node to be removed from the node set.
     */
    protected def minus (node: NodeT): Unit
    /**
     * removes `node` either rippling or gently. 
     * 
     * @param node
     * @param rippleDelete if `true`, `node` will be deleted with its incident edges;
     *        otherwise `node` will be only deleted if it has no incident edges or
     *        all its incident edges are hooks.
     * @param minusNode implementation of node removal without considering incident edges.
     * @param minusEdges implementation of removal of all incident edges. 
     * @return `true` if `node` has been removed.
     */
    protected[collection] final def subtract(node: NodeT,
                                             rippleDelete: Boolean,
                                             minusNode: (NodeT) => Unit,
                                             minusEdges:(NodeT) => Unit ): Boolean = {
      def minusNodeTrue = { minusNode(node); true }
      def minusAllTrue  = { minusEdges(node); minusNodeTrue }
      if (contains(node))
        if (node.edges.isEmpty)     minusNodeTrue
        else if (rippleDelete)      minusAllTrue
        else if (node.hasOnlyHooks) minusAllTrue
        else                        handleNotGentlyRemovable
      else false
    }
    protected def handleNotGentlyRemovable = false
  }
  type EdgeSetT <: EdgeSet
  trait EdgeSet extends super.EdgeSet
  /** Checks whether a given node or edge is contained in this graph.
   *
   *  @param elem the node or edge the existence of which is to be checked
   *  @return true if `elem` is contained in this graph
   */
  def contains(elem: GraphParam[N,E]) =
    if (elem.isNode)
      nodes contains (elem match {
        case n: NodeIn[N] => newNode(n.value)
        case n: NodeT     => n
        case n: NodeOut[N]=> newNode(n.value)
      })
    else
      edges contains (elem match {
        case e: E[N]                 => newEdge(e)
        case e: EdgeT                => e
        case e: GraphBase[N,E]#EdgeT => newEdge(e.toEdgeIn)
        case _ => throw new IllegalArgumentException
      }) 
  /** Iterates over all nodes and all edges.
   *
   *  @return iterator containing all nodes and all edges
   */
  def iterator = nodes.toIterator ++ edges.toIterator
  /**
   * Searches for an inner node equaling to `node` in this graph.
   * 
   * @param node The outer node to search for in this graph.
   * @return `Some` of the inner node found or `None`.
   */
  @inline final def find(node: N)    = nodes.find(node)
  /**
   * Searches for an edge node equaling to `edge` in this graph.
   * 
   * @param edge The outer edge to search for in this graph.
   * @return `Some` of the inner edge found or `None`.
   */
  @inline final def find(edge: E[N]) = edges.find(edge)
  /**
   * Searches for an inner node equaling to `node` in this graph
   * which must exist in this graph.
   * 
   * @param node The outer node to search for in this graph.
   * @return The inner node looked up. If no inner node
   *         equaling to `node` is found NoSuchElementException is raised. 
   */
  @inline final def get (node: N)    = find(node).get
  /**
   * Searches for an inner edge equaling to `edge` in this graph
   * which must exist in this graph.
   * 
   * @param edge The outer edge to search for in this graph.
   * @return The inner edge looked up. If no inner edge
   *         equaling to `edge` is found NoSuchElementException is raised. 
   */
  @inline final def get (edge: E[N]) = find(edge).get
  /**
   * Searches for an inner node equaling to `node` in this graph.
   * 
   * @param node The outer node to search for in this graph.
   * @param default The inner node to return if `node` could not be found.
   * @return The inner node looked up or `default` if no inner node
   *         equaling to `node` could be found. 
   */
  @inline final def getOrElse(node: N,   default: NodeT) = find(node).getOrElse(default)
  /**
   * Searches for an inner edge equaling to `edge` in this graph.
   * 
   * @param edge The outer edge to search for in this graph.
   * @param default The inner edge to return if `edge` could not be found.
   * @return The inner edge looked up or `default` if no inner edge
   *         equaling to `edge` could be found. 
   */
  @inline final def getOrElse(edge: E[N],default: EdgeT) = find(edge).getOrElse(default)

  /** Creates a new supergraph with an additional node, unless the node passed is already present.
   *
   *  @param node the node to be added
   *  @return the new supergraph containing all nodes and edges of this graph and `node` additionally.
   */
  def +(node: N): This
  /** Creates a new supergraph with an additional edge, unless the edge passed is already present.
   *
   *  @param edge the edge to be added
   *  @return the new supergraph containing all nodes and edges of this graph plus `edge`.
   */
  protected def +#(edge: E[N]): This
  /** Creates a new supergraph with an additional node or edge, unless the node or edge passed is
   *  already present.
   *
   *  This method purely wraps `+(node: N)` respectively `+(edge: E[N])` granting the same behavior.
   *    
   *  @param elem the wrapped node or edge to be added; ; if `elem` is of type N, the wrapped object
   *         is added to the node set otherwise to the edge set.
   *  @return a new supergraph containing all nodes and edges of this graph plus `elem`.
   */
  def +(elem: GraphParam[N,E]): This = elem match {
    case n: NodeIn [N] => this + n.value
    case n: NodeOut[N] => this + n.value
    case e: E[N]                 => this +# e
    case e: GraphBase[N,E]#EdgeT => this +# e.toEdgeIn 
  }
  override def ++ (elems: GenTraversableOnce[GraphParam[N,E]]) = bulkOp(elems, true)
  override def -- (elems: GenTraversableOnce[GraphParam[N,E]]) = bulkOp(elems, false)
  /** Prepares and calls `plusPlus` or `minusMinus`. */
  final protected def bulkOp(elems:      GenTraversableOnce[GraphParam[N,E]],
                             isPlusPlus: Boolean): This = {
    val p = partition(elems)
    if (isPlusPlus) plusPlus  (p.toOuterNodes, p.toOuterEdges)
    else            minusMinus(p.toOuterNodes, p.toOuterEdges)
  }
  final protected def partition(elems: GenTraversableOnce[GraphParam[N,E]]) =
    new GraphParam.Partitions[N,E] (elems match {
      case x: Iterable       [GraphParam[N,E]] => x
      case x: TraversableOnce[GraphParam[N,E]] => x.toIterable
      case _ => throw new IllegalArgumentException("TraversableOnce expected.")
    })
  /** Implements the heart of `++` calling the `from` factory method of the companion object.
   *  $REIMPLFACTORY */
  protected def plusPlus(newNodes: Iterable[N], newEdges: Iterable[E[N]]): This =
    coreCompanion.getOrElse(throw new UnsupportedOperationException).
      from[N,E](nodes.toNodeInSet ++ newNodes,
                edges.toEdgeInSet ++ newEdges).asInstanceOf[This]
  /** Implements the heart of `--` calling the `from` factory method of the companion object.
   *  $REIMPLFACTORY */
  protected def minusMinus(delNodes: Iterable[N], delEdges: Iterable[E[N]]): This = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    coreCompanion.getOrElse(throw new UnsupportedOperationException).
      from[N,E](delNodesEdges._1, delNodesEdges._2).asInstanceOf[This]
  }
  /** Calculates the `nodes` and `edges` arguments to be passed to a factory method
   *  when delNodes and delEdges are to be deleted by `--`.
   */
  protected def minusMinusNodesEdges(delNodes: Iterable[N], delEdges: Iterable[E[N]]) =
    (nodes.toNodeInSet -- delNodes,
     { val delNodeSet = delNodes.toSet
        val restNodes = 
          for(e <- edges.toEdgeInSet if e forall (n =>
              ! (delNodeSet contains n))) yield e
        restNodes -- delEdges
      }
     )
  /** Creates a new subgraph consisting of all nodes and edges of this graph except ´node´
   *  and those edges which `node` is incident with.
   *
   *  @param node the node to be removed.
   *  @return the new subgraph of this graph after the "ripple" deletion of `node`.
   */
  def - (node: N): This
  /** Creates a new subgraph consisting of all nodes and edges of this graph but `node`
   *  which is conditionally removed from this graph. The removal only succeeds if the node
   *  is not incident with any edges or it is only incident with hooks.
   *
   *  @param node the node to be gently removed.
   *  @return the new subgraph of this graph after the "gentle" deletion of `node`. 
   *          If `node` could not be deleted, the new graph is a copy of this graph. 
   */
  def -?(node: N): This
  /** Creates a new subgraph consisting of all nodes and edges of this graph but `edge`.
   * The node set remains unchanged.
   *
   *  @param edge the edge to be removed.
   *  @return a new subgraph of this graph that contains all nodes and edges of this graph
   *          except of `edge`.
   */
  protected def -#(edge: E[N]): This
  /** Creates a new subgraph consisting of all nodes and edges of this graph except `edge`
   *  and those nodes which are incident with `edge` and would become edge-less after deletion. 
   *
   *  @param edge the edge to be removed.
   *  @return a new subgraph of this graph after the "ripple" deletion of `edge` from this graph.
   */
  protected def -!#(edge: E[N]): This
  /** Creates a new subgraph consisting of all nodes and edges of this graph except `elem`.
   *  If `elem` is of type N, this method maps to `-(node: N)`. Otherwise the edge is deleted
   *  leaving the node set unchanged.
   *
   *  @param elem node or edge to be removed.
   *  @return the new subgraph of this graph after the "ripple" deletion of the passed node or the
   *          simple deletion of the passed edge.
   */
  def - (elem: GraphParam[N,E]): This = elem match {
    case n: NodeIn [N] => this - n.value
    case n: NodeOut[N] => this - n.value
    case e: E[N]                 => this -# e
    case e: GraphBase[N,E]#EdgeT => this -# e.toEdgeIn 
  }
  /** Creates a new subgraph consisting of all nodes and edges of this graph except `elem`.
   *  If `elem` is of type N, this method maps to `-(node: N)`. Otherwise the edge is deleted
   *  along with those incident nodes which would become edge-less after deletion.
   *
   *  @param elem node or edge to be removed.
   *  @return a new subgraph of this graph after the "ripple" deletion of the passed node or edge.
   */
  def -!(elem: GraphParam[N,E]): This = elem match {
    case n: NodeIn [N] => this - n.value 
    case n: NodeOut[N] => this - n.value
    case e: E[N]                 => this -!# e
    case e: GraphBase[N,E]#EdgeT => this -!# e.toEdgeIn 
  }
  /** Creates a new subgraph consisting of all nodes and edges of this graph but the elements
   * of `coll` which will be unconditionally removed. This operation differs from `--`
   * in that edge are deleted along with those incident nodes which would become edge-less
   * after deletion.
   *
   *  @param coll Collection of nodes and/or edges to be removed; if the element type is N,
   *              it is removed from the node set otherwise from the edge set.
   *              See `-!(elem: GraphParam[N,E])`.
   *  @return the new subgraph containing all nodes and edges of this graph
   *          after the "ripple" deletion of nodes and the simple deletion of edges in `coll` .
   */
  def --! (elems: GenTraversableOnce[GraphParam[N,E]]): This = {
    val p = partition(elems)
    val (delNodes, delEdges) = (p.toOuterNodes, p.toOuterEdges)
    val unconnectedNodeCandidates = {
      val edgeNodes = MSet.empty[N]
      delEdges foreach (_ foreach (n => edgeNodes += n))
      edgeNodes -- delNodes
    }
    val delEdgeSet = {
      val edges = MSet.empty[EdgeT]
      delEdges foreach (this find _ map (edges += _))
      edges
    }
    minusMinus(delNodes ++
                 (unconnectedNodeCandidates filter (
                    nc => this find nc map (n =>
                      n.edges forall (delEdgeSet contains _)) getOrElse false)),
               delEdges)
  }
  /**
   * Provides a shortcut for predicates involving any graph element.
   * In order to compute a subgraph of this graph, the result of this method
   * may be passed to any graph-level method requiring a predicate such as
   * `count`, `exists`, `filter`, `filterNot`, `forall` etc. For instance
   * 
   * {{{
   * val g = Graph(2~>3, 3~>1, 5)
   * g filter g.having(nodes = _ >= 2) // yields Graph(2, 3, 5, 2~>3)   
   * }}}
   * 
   * @param node The predicate that must hold for the nodes. 
   * @param edge The predicate that must hold for the edges. If omitted, all edges
   *             between nodes to be included by `nodes` will also be included. 
   * @return A partial function combining the passed predicates.
   */
  def having(node: (NodeT) => Boolean = _ => false,
             edge: (EdgeT) => Boolean = null): PartialFunction[GraphParam[N,E], Boolean] = {
    val nodePred: PartialFunction[GraphParam[N,E], Boolean] = { case n: NodeT => node(n) }
    val edgePred: PartialFunction[GraphParam[N,E], Boolean] =
      if (edge eq null) { case e: EdgeT => e forall (node(_)) }
      else              { case e: EdgeT => edge(e) }
    nodePred orElse edgePred
  }
}

// ----------------------------------------------------------------------------
import collection.generic.CanBuildFrom

import generic.{GraphCompanion, GraphFactory}
/**
 * The main trait for immutable graphs bundling the functionality of traits concerned with
 * specific aspects.
 * 
 * @tparam N the type of the nodes (vertices) in this graph.
 * @tparam E the kind of the edges in this graph. 
 *
 * @author Peter Empen
 */
trait Graph[N, E[X] <: EdgeLikeIn[X]]
  extends Set[GraphParam[N,E]]
  with    GraphLike[N,E,Graph[N,E]]
{
  override def empty: Graph[N,E] = Graph.empty[N,E]
}
/**
 * The main companion object for immutable graphs.
 *
 * @author Peter Empen
 */
object Graph
  extends GraphFactory     [Graph]
  with    GraphAuxCompanion[Graph]
{
  override def newBuilder  [N, E[X] <: EdgeLikeIn[X]] = immutable.Graph.newBuilder[N,E]
  override def empty       [N, E[X] <: EdgeLikeIn[X]]: Graph[N,E] = immutable.Graph.empty[N,E]
  override def from        [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                                      edges: Iterable[E[N]]): Graph[N,E] =
    immutable.Graph.from[N,E](nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): Graph[N,E] =
    immutable.Graph.fromStream[N,E](nodeStreams, nodes, edgeStreams, edges)
  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
    new GraphCanBuildFrom[N,E]
}
