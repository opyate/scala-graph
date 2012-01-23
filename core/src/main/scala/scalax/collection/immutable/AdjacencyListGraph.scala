package scalax.collection
package immutable

import collection.Set
import collection.mutable.{Set => MutableSet}

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
	extends GraphLike[N,E,This]
  with    GraphAux [N,E]
{ selfGraph: This =>
  trait InnerNodeImpl extends InnerNodeLike {
    this: NodeT =>
    @inline final override def edges: collection.Iterable[EdgeT] = nodes edges this
  }
  type NodeSetT = NodeSet
  @SerialVersionUID(7870L)
  class NodeSet extends super.NodeSet with NodeSetAux
  {
    @SerialVersionUID(7871L)
    protected class MapExt extends collection.mutable.HashMap[NodeT, MutableSet[EdgeT]]{
      @inline final override def findEntry(key: NodeT) = super.findEntry(key)
    }
    type Coll = MapExt
    protected val coll: Coll = new MapExt
    override def copy = {
      val nodeSet = new NodeSet 
      nodeSet initialize (this.toNodeInSet, null)
      nodeSet
    }
    protected val emptyEdges = MutableSet[EdgeT]() // must not be modified
    override protected[collection]
    def initialize(nodes: collection.Iterable[N],
                   edges: collection.Iterable[E[N]]) = {
      if (nodes ne null) 
        coll ++= nodes map (n => (Node(n) -> emptyEdges))
      this
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
    @inline final override def find(elem: N): Option[NodeT] = {
      val e = coll.findEntry(Node(elem))
      if (e == null) None else Some(e.key)
    }
    @inline final def edges(node: NodeT): Set[EdgeT] =
      if (contains(node)) coll(node)
      else                emptyEdges
    @inline final def contains(node: NodeT) =	coll.contains(node)
    @inline final def iterator: Iterator[NodeT] =	coll.keySet.iterator
    @inline final override protected def minus     (node: NodeT) { coll -= node }
    def +(node: NodeT) = { coll + (node -> emptyEdges); this }
    protected[collection] def += (edge: EdgeT): this.type = {
      for (n <- edge) {
        val entry = coll.findEntry(n)
        if (entry == null)                  coll += (n -> MutableSet(edge))
        else if (entry.value eq emptyEdges) entry.value = MutableSet(edge) 
        else                                entry.value += edge
      }
      this
    }
  }
  protected val _nodes: NodeSet 
  @inline final def nodes = _nodes

  type EdgeT = EdgeBase
  @inline final override protected def newEdge(innerEdge: E[NodeT]) = new EdgeT(innerEdge)
  type EdgeSetT = EdgeSet
  @SerialVersionUID(7872L)
  class EdgeSet extends super.EdgeSet with EdgeSetAux
  {
    type Coll = collection.mutable.HashSet[EdgeT]
    protected val coll: Coll = collection.mutable.HashSet.empty
    override protected[collection] def initialize(edges: collection.Iterable[E[N]]) {
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
    @inline final protected[AdjacencyListGraph] def +=(edge: EdgeT) = { _nodes += edge; coll add edge }
    @inline final def contains(edge: EdgeT) =	coll.contains(edge)
    @inline final def iterator              =	coll.iterator
    @inline final def +(edge: EdgeT) = coll + edge
    @inline final def -(edge: EdgeT) = coll - edge
    @inline final override lazy val maxArity = super.maxArity
  }
  protected val _edges: EdgeSetT 
  @inline final def edges = _edges

  def copy(nodes: collection.Iterable[N],
           edges: collection.Iterable[E[N]]): This
  def + (n: N)  = if (_nodes contains Node(n)) this
                  else copy(_nodes.toNodeInSet.toBuffer += n,
                            _edges.toEdgeInSet)
  protected
  def +#(e: E[N]) = if (_edges contains Edge(e)) this
                    else copy(_nodes.toNodeInSet,
                              _edges.toEdgeInSet.toBuffer += e)
  def - (n: N)  = _nodes find (nf => nf.value == n) match {
                    case Some(nf) => copy(_nodes.toNodeInSet.toBuffer  -= n,
                                          _edges.toEdgeInSet.toBuffer --= (nf.edges map (_.toEdgeIn)))
                    case None     => this
                  }
  def -?(n: N)  = _nodes find (nf => nf.value == n) match {
                    case Some(nf) => var newNodes = _nodes.toNodeInSet.toBuffer
                                     var newEdges = _edges.toEdgeInSet.toBuffer
                                     nodes.subtract(nf,
                                                    false,
                                                    nf => newNodes  -= n,
                                                    nf => newEdges --= (nf.edges map (_.toEdgeIn)))
                                     copy(newNodes, newEdges)
                    case None     => this
                  }
  protected
  def -#(e: E[N]) = if (_edges contains Edge(e))
                      copy(_nodes.toNodeInSet,
                           _edges.toEdgeInSet.toBuffer -= e)
                    else this
  protected
  def -!#(e: E[N]) = _edges find (ef => ef == e) match {
                    case Some(ef) => copy(_nodes.toNodeInSet.toBuffer --= ef.privateNodes map (n => n.value),
                                          _edges.toEdgeInSet.toBuffer  -= e)
                    case None     => this
                  }
}