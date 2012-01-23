package scalax.collection
package immutable

import collection.mutable.{Set => MutableSet}

import GraphEdge.{EdgeLike, EdgeCompanionBase}
import GraphPredef.EdgeLikeIn
import io._

/**
 * Implements a graph prioritizing minimal memory footprint.
 * This is achieved by representing the node set and the edge set by a hash set for each. 
 *   
 * @author Peter Empen
 */
trait SetsOnlyGraph[N,
                    E[X] <: EdgeLikeIn[X],
                   +This <: SetsOnlyGraph[N, E, This] with Graph[N,E]]
  extends GraphLike[N,E,This]
  with    GraphAux [N,E]
{ selfGraph: This =>
  type NodeSetT = NodeSet
  @SerialVersionUID(8070L)
  class NodeSet extends super.NodeSet with NodeSetAux {
    type Coll = collection.mutable.HashSet[NodeT]
    protected val coll : Coll = collection.mutable.HashSet.empty
    @inline final override def copy = {
      val nodeSet = new NodeSet 
      nodeSet initialize (this.toNodeInSet, null)
      nodeSet
    }
    override protected[collection]
    def initialize(nodes: collection.Iterable[N],
                   edges: collection.Iterable[E[N]])
    {
      if ((edges eq null) || edges.size == 0) {
        if (nodes ne null) coll ++= (nodes map (n => Node(n)) toSet)
      } else {
        val mSet = MutableSet[NodeT]() 
        for (e <- edges; n <- e) coll += Node(n)
        coll ++= (nodes map (n => Node(n)))
      }
    }
    override protected[collection]
    def from (nodeStreams: Iterable[NodeInputStream[N]],
              nodes:       Iterable[N],
              edgeStreams: Iterable[GenEdgeInputStream[N,E]],
              edges:       Iterable[E[N]])
    {
      for (n <- new NodeAux.NodeContStream(nodeStreams, nodes))
        coll += Node(n)
    }
    @inline final protected[SetsOnlyGraph] def  +=(node: NodeT) = coll add node
    @inline final protected[SetsOnlyGraph] def ++=(nodeSeq: Seq[NodeT]) =
      coll ++= nodeSeq
    @inline final override protected def minus(node: NodeT) { coll -= node }
    @inline final override def find(elem: N) = coll findEntry Node(elem)
    @inline final protected[SetsOnlyGraph] def asSet = coll

    @inline final def +(node: NodeT)        = coll + node
    @inline final def contains(node: NodeT) = coll.contains(node)
    @inline final def iterator: Iterator[NodeT] = coll.iterator
  }
  protected val _nodes: NodeSet 
  @inline final def nodes = _nodes

  type EdgeT = EdgeBase
  override protected def newEdge(innerEdge: E[NodeT]) = new EdgeT(innerEdge)
  type EdgeSetT = EdgeSet
  @SerialVersionUID(8071L)
  class EdgeSet extends super.EdgeSet with EdgeSetAux {
    type Coll = collection.mutable.HashSet[EdgeT]
    protected val coll : Coll = collection.mutable.HashSet.empty
    override protected[collection]
    def initialize(edges: collection.Iterable[E[N]]) {
      if (edges ne null)
        edges foreach (e => coll += Edge(e))
    }
    override protected[collection]
    def from (nodeStreams: Iterable[NodeInputStream[N]],
              nodes:       Iterable[N],
              edgeStreams: Iterable[GenEdgeInputStream[N,E]],
              edges:       Iterable[E[N]])
    {
      for (e <- new EdgeAux.EdgeContStream(edgeStreams, edges)) {
        if (e.isHyperEdge)
          selfGraph.nodes ++= e.nodeSeq
        else {
          selfGraph.nodes += e._1
          selfGraph.nodes += e._2
        }
        coll += Edge(e)
      }
    }
    @inline final override def contains(node: NodeT) = coll exists (_ contains node)
    @inline final override def find(elem: E[N]) = coll findEntry Edge(elem)
    @inline final protected[SetsOnlyGraph] def asSet = coll

    @inline final def +(edge: EdgeT)      = coll + edge
    @inline final def -(edge: EdgeT)      = coll - edge
    @inline final def contains(edge: EdgeT) = coll.contains(edge)
    @inline final def iterator      = coll.iterator

    @inline final override lazy val maxArity = super.maxArity
  }
  protected val _edges: EdgeSet 
  @inline def edges = _edges

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