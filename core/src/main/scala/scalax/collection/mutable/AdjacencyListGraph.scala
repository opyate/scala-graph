package scalax.collection
package mutable

import collection.Set
import collection.mutable.{HashSet, HashMap, Set => MutableSet}

import GraphEdge.{EdgeLike, EdgeCompanionBase}
import GraphPredef.EdgeLikeIn
import io._

/**
 * Implements an adjacency list based graph representation.
 * 
 * An adjacency list based representation speeds up traversing a graph along its paths
 * by storing the list of connecting edges to each node.
 *   
 * @author Peter Empen
 */
trait AdjacencyListGraph[N,
                         E[X] <: EdgeLikeIn[X],
                        +This <: AdjacencyListGraph[N, E, This] with Graph[N,E]]
	extends GraphLike[N, E, This]
{
  trait InnerNodeImpl extends InnerNodeLike {
    this: NodeT =>
    override def edges: Iterable[EdgeT] = nodes edges this
  }
  type NodeSetT = NodeSet
  @SerialVersionUID(7970L)
  class NodeSet extends super.NodeSet
  {
    @SerialVersionUID(7971L)
    protected class MapExt extends HashMap[NodeT, MutableSet[EdgeT]]{
      @inline final override def findEntry(key: NodeT) = super.findEntry(key)
    }
    type Coll = MapExt
    protected val coll: Coll = new MapExt
    @inline final override def copy = {
      val nodeSet = new NodeSet
      nodeSet initialize (this.toNodeInSet, null)
      nodeSet
    }
    override protected[collection]
    def from (nodeStreams: Iterable[NodeInputStream[N]],
              nodes:       Iterable[N],
              edgeStreams: Iterable[GenEdgeInputStream[N,E]],
              edges:       Iterable[E[N]])
    {
      for (n <- new NodeAux.NodeContStream(nodeStreams, nodes))
        coll += (Node(n) -> emptyEdges)
    }
    protected val emptyEdges = MutableSet[EdgeT]() // must not be modified
    override protected[collection]
    def initialize(nodes: Iterable[N],
                   edges: collection.Iterable[E[N]]) = {
      if (nodes ne null) 
        coll ++= nodes map (n => (Node(n) -> emptyEdges))
      this
    }
    @inline final override def find(elem: N): Option[NodeT] = {
      val e = coll.findEntry(Node(elem))
      if (e == null) None else Some(e.key)
    }
    @inline final def edges(node: NodeT): Set[EdgeT] =
      if (contains(node)) coll(node)
      else                emptyEdges
    @inline final def contains(node: NodeT) =	coll.contains(node)
    @inline final def iterator: Iterator[NodeT] =	coll.keySet.iterator
    override def add(node: NodeT) = if(coll.contains(node)) false
                                    else { coll += (node -> emptyEdges)
                                           true }
    @inline final def += (node: NodeT): this.type	= { add(node); this }
    protected[collection]
    def += (edge: EdgeT): this.type	= {
      // more performant than: "for (n <- edge) { this += n; coll(n) += edge }"
      for (n <- edge) {
        val entry = coll.findEntry(n)
        if (entry == null)                  coll += (n -> MutableSet(edge))
        else if (entry.value eq emptyEdges) entry.value = MutableSet(edge) 
        else                                entry.value += edge
      }
      this
    }
    protected[collection]
    def -= (edge: EdgeT): this.type = { for (n <- edge) coll(n) -= edge
                                        this }
    override protected def minus     (node: NodeT) { coll -= node }
    override protected def minusEdges(node: NodeT) { _edges --= node.edges }
  }
  protected val _nodes: NodeSet 
  @inline final def nodes = _nodes

  
  type EdgeT = EdgeImpl
  @SerialVersionUID(7972L)
  class EdgeImpl(override val edge: E[NodeT]) extends EdgeBase(edge)
  {
    def remove: Boolean = _edges remove this
    def removeWithNodes(edge: E[N]) = if (_edges remove this) {
                                        _nodes --= privateNodes; true
                                      } else false
  }
  @inline final override protected def newEdge(innerEdge: E[NodeT]) = new EdgeT(innerEdge)
  type EdgeSetT = EdgeSet
  @SerialVersionUID(7973L)
  class EdgeSet extends super.EdgeSet
  {
    type Coll = HashSet[EdgeT]
    protected val coll: Coll = HashSet.empty
    override protected[collection]def initialize(edges: Iterable[E[N]]) {
      if (edges ne null)
        edges foreach (this += Edge(_))
    }
    override protected[collection]
    def from (nodeStreams: Iterable[NodeInputStream[N]],
              nodes:       Iterable[N],
              edgeStreams: Iterable[GenEdgeInputStream[N,E]],
              edges:       Iterable[E[N]])
    {
      for (e <- new EdgeAux.EdgeContStream(edgeStreams, edges))
        this += Edge(e)
    }
    @inline final override def contains(node: NodeT) = coll exists (_ contains node)
    @inline final override def find(elem: E[N]) = coll findEntry Edge(elem)
    override def add(edge: EdgeT) = { _nodes += edge; coll add edge }
    @inline final def contains(edge: EdgeT) =	coll.contains(edge)
    @inline final def iterator              =	coll.iterator
    override def remove(edge: EdgeT) = if (coll remove edge) {_nodes -= edge; true} else false
    def removeWithNodes(edge: EdgeT) = {
      val privateNodes = edge.privateNodes
      if (remove(edge)) {
        _nodes --= privateNodes
        true
      } else false
    }
    @inline final override def maxArity: Int = super.maxArity
  }
  protected val _edges: EdgeSetT 
  @inline final def edges = _edges

  @inline final def clear				  {	_nodes.clear }
  @inline final def add(node: N): Boolean = _nodes add Node(node)
  @inline final def add(edge: E[N]): Boolean  = _edges add Edge(edge)  
  @inline final protected def +=# (edge: E[N]): this.type = { _edges += Edge(edge); this }
}