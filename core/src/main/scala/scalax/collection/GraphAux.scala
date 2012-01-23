package scalax.collection

import collection.{Set, SortedSet, SortedMap}
import collection.mutable.{Set => MutableSet, Map => MutableMap, ListBuffer}

import GraphPredef.{EdgeLikeIn, GraphParamNode, NodeIn, NodeOut, EdgeOut}
import GraphEdge.{EdgeLike, EdgeCompanionBase}
import io._

/**
 * Template for stream-based operations such as instantiation through
 * a stream-based auxiliary constructor.
 * 
 * @tparam N    the user type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges (links) in this graph.
 *
 * @define NSTREAMS list of node input streams to be processed. All nodes read from any
 *         of these streams will be added to this graph. Note that only isolated nodes
 *         must be included in a stream or in `nodes`, non-isolated nodes are optional.
 * @define INNODES The isolated (and optionally any other) outer nodes that the node set of
 *         this graph is to be populated with. This parameter may be used as an alternative
 *         or in addition to `nodeStreams`.
 * @define ESTREAMS list of edge input streams, each with its own edge factory,
 *         to be processed. All edges and edge ends (nodes) read from any of these streams
 *         will be added to this graph.
 * @define INEDGES The outer edges that the edge set of this graph is to be populated with.
 *         Nodes being the end of any of these edges will be added to the node set.
 *         This parameter is meant be used as an alternative or in addition to `edgeStreams`.
 * @define INITIMPL The implementor is not allowed to reset any of the input streams so
 *         he must decide for each input stream whether it is to read by `NodeSet.initialize`
 *         or by `EdgeSet.initialize`, not both. Computing contained inner nodes/edges from
 *         input streams is facilitated by `Node.NodeContStream`/`Edge.EdgeContStream`
 *         respectively.
 *
 * @author Peter Empen
 */
trait GraphAux[N, E[X] <: EdgeLikeIn[X]]
  extends GraphBase[N,E]
{
  /**
   * Populates this graph with nodes and the edges to be created through `edgeStream`.
   * 
   * The implementing class will typically have a constructor with the same parameters
   * which is invoked through a call to `from` of the companion object.
   *
   * @param nodeStreams $NSTREAMS
   * @param nodes       $INNODES
   * @param edgeStreams $ESTREAMS
   * @param edges       $INEDGES
   */
  protected def from (nodeStreams: Iterable[NodeInputStream[N]],
                      nodes:       Iterable[N],
                      edgeStreams: Iterable[GenEdgeInputStream[N,E]],
                      edges:       Iterable[E[N]])
  {
    this.nodes.from(nodeStreams, nodes, edgeStreams, edges)
    this.edges.from(nodeStreams, nodes, edgeStreams, edges)
  }
  object NodeAux extends {
    /**
     * This class facilitates seamless iteration over the nodes supplied
     * by the constructor arguments.  
     */
    protected[collection]
    class NodeContStream(nodeStreams: Iterable[NodeInputStream[N]],
                         nodeList:    Iterable[N])
      extends Iterator[N]
    {
      val streamNodeIt =
        nodeStreams.foldLeft(Seq.empty[N].iterator)(
                             (left: Iterator[N],
                              elm: NodeInputStream[N]) => left ++ elm)
      val nodeIt = streamNodeIt ++ nodeList.iterator
      override def hasNext = nodeIt.hasNext
      override def next = nodeIt.next
    }
  }
  type NodeSetT <: NodeSetAux
  trait NodeSetAux extends NodeSet {
    /**
     * This method may be called by an auxiliary constructor. If so, it must be defined
     * by the trait responsible for the implementation of the graph representation.
     *
     * $INITIMPL
     * 
     * For parameter descriptions see `NodeAux.from`.
     */
    protected[collection]
    def from (nodeStreams: Iterable[NodeInputStream[N]],
              nodes:       Iterable[N],
              edgeStreams: Iterable[GenEdgeInputStream[N,E]],
              edges:       Iterable[E[N]]): Unit
  }
  object EdgeAux {
    import GraphEdge.  {  HyperEdgeCompanion,                     EdgeCompanion}
    import edge. WBase.{ WHyperEdgeCompanion,  WHyperEdgeBound,  WEdgeCompanion,  WEdgeBound}
    import edge. LBase.{ LHyperEdgeCompanion,  LHyperEdgeBound,  LEdgeCompanion,  LEdgeBound}
    import edge.WLBase.{WLHyperEdgeCompanion, WLHyperEdgeBound, WLEdgeCompanion, WLEdgeBound}
    /** Creates a new simple inner hyperedge without modifying the node or edge set. */
    protected[collection]
    def nodesToEdgeCont(factory: HyperEdgeCompanion[E],
                        node_1:  N,
                        node_2:  N,
                        nodes:   N *): E[NodeT] = {
        factory.from[NodeT](Edge.mkNodes(node_1, node_2, nodes: _*)).asInstanceOf[E[NodeT]]
    }
    /** Creates a new simple inner edge without modifying the node or edge set. */
    protected[collection]
    def nodesToEdgeCont(factory: EdgeCompanion[E],
                        node_1:  N,
                        node_2:  N): E[NodeT] = {
      factory.from[NodeT](Edge.mkNodes(node_1, node_2)).asInstanceOf[E[NodeT]]
    }
    /** Creates a new weighted inner hyperedge without modifying the node or edge set. */
    protected[collection]
    def nodesToWEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with WHyperEdgeBound[_,EE]]
       (factory: WHyperEdgeCompanion[EE],
        weight:  Long,
        node_1:  N,
        node_2:  N,
        nodes:   N *): E[NodeT] =
    {
        factory.from[NodeT](Edge.mkNodes(node_1, node_2, nodes: _*))(weight).asInstanceOf[E[NodeT]]
    }
    /** Creates a new weighted inner edge without modifying the node or edge set. */
    protected[collection]
    def nodesToWEdgeCont[EE[X] <: E[X] with EdgeLikeIn[X] with WEdgeBound[_,EE]]
       (factory: WEdgeCompanion[EE],
        weight:  Long,
        node_1:  N,
        node_2:  N): E[NodeT] =
    {
      factory.from[NodeT](Edge.mkNodes(node_1, node_2))(weight).asInstanceOf[E[NodeT]]
    }
    /** Creates a new labeled inner hyperedge without modifying the node or edge set. */
    protected[collection]
    def nodesToLEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with LHyperEdgeBound[_,EE], L]
       (factory: LHyperEdgeCompanion[EE],
        label:   L,
        node_1:  N,
        node_2:  N,
        nodes:   N *): E[NodeT] =
    {
        factory.from[NodeT,L](Edge.mkNodes(node_1, node_2, nodes: _*))(label).asInstanceOf[E[NodeT]]
    }
    /** Creates a new labeled inner edge  without modifying the node or edge set. */
    protected[collection]
    def nodesToLEdgeCont[EE[X] <: E[X] with EdgeLikeIn[X] with LEdgeBound[_,EE], L]
       (factory: LEdgeCompanion[EE],
        label:   L,
        node_1:  N,
        node_2:  N): E[NodeT] =
    {
      factory.from[NodeT,L](Edge.mkNodes(node_1, node_2))(label).asInstanceOf[E[NodeT]]
    }
    /** Creates a new weighted and labeled inner hyperedge without
     * modifying the node or edge set. */
    protected[collection]
    def nodesToWLEdgeCont [EE[X] <: E[X] with EdgeLikeIn[X] with WLHyperEdgeBound[_,EE], L]
       (factory: WLHyperEdgeCompanion[EE],
        weight:  Long,
        label:   L,
        node_1:  N,
        node_2:  N,
        nodes:   N *): E[NodeT] =
    {
        factory.from[NodeT,L](Edge.mkNodes(node_1, node_2, nodes: _*))(weight, label).asInstanceOf[E[NodeT]]
    }
    /** Creates a new weighted and labeled inner edge  without
     * modifying the node or edge set. */
    protected[collection]
    def nodesToWLEdgeCont[EE[X] <: E[X] with EdgeLikeIn[X] with WLEdgeBound[_,EE], L]
       (factory: WLEdgeCompanion[EE],
        weight:  Long,
        label:   L,
        node_1:  N,
        node_2:  N): E[NodeT] =
    {
      factory.from[NodeT,L](Edge.mkNodes(node_1, node_2))(weight, label).asInstanceOf[E[NodeT]]
    }
    /**
     * This class facilitates seamless iteration over the edges supplied
     * by the constructor arguments and contained inner edge creation.  
     */
    protected[collection]
    class EdgeContStream
         (edgeStreams: Iterable[GenEdgeInputStream[N,E]],
          edgeList:    Iterable[E[N]])
      extends Iterator[E[NodeT]]
    {
      val streamWorkers = edgeStreams map (new StreamWorker(_))
      val streamWorkersIt =
        streamWorkers.foldLeft(Seq.empty[E[NodeT]].iterator)(
                               (left: Iterator[E[NodeT]],
                                elem: StreamWorker) => left ++ elem)
      val edgeIt = streamWorkersIt ++ new ListWorker(edgeList)
      
      override def hasNext = edgeIt.hasNext
      override def next    = edgeIt.next

      private[EdgeContStream] class StreamWorker(genEedgeStream: GenEdgeInputStream[N,E])
        extends Iterator[E[NodeT]]
      {
        import GraphEdge.{HyperEdge, UnDiEdge}

        type    EdgeT[X] =  UnDiEdge[X] with E[X]
        type   WEdgeT[X] =  UnDiEdge[X] with E[X] with      WEdgeBound[_,E]
        type   LEdgeT[X] =  UnDiEdge[X] with E[X] with      LEdgeBound[_,E]
        type  WLEdgeT[X] =  UnDiEdge[X] with E[X] with     WLEdgeBound[_,E]
        type   HEdgeT[X] = HyperEdge[X] with E[X]
        type  WHEdgeT[X] = HyperEdge[X] with E[X] with  WHyperEdgeBound[_,E]
        type  LHEdgeT[X] = HyperEdge[X] with E[X] with  LHyperEdgeBound[_,E]
        type WLHEdgeT[X] = HyperEdge[X] with E[X] with WLHyperEdgeBound[_,E]

        val edgeStream = genEedgeStream.
                         asInstanceOf[EdgeInputStream[N,E,_,EdgeCompanionBase,EdgeAdapterBase]] 
        val eF  = edgeStream.factory match {
                    case f:   EdgeCompanion[  EdgeT] => f; case _=> null}
        val wF  = edgeStream.factory match {
                    case f:  WEdgeCompanion[ WEdgeT] => f; case _=> null}
        val lF  = edgeStream.factory match {
                    case f:  LEdgeCompanion[ LEdgeT] => f; case _=> null}
        val wlF = edgeStream.factory match {
                    case f: WLEdgeCompanion[WLEdgeT] => f; case _=> null}
        val hF  = edgeStream.factory match {
                    case f:   HyperEdgeCompanion[  HEdgeT] => f; case _=> null}
        val whF = edgeStream.factory match {
                    case f:  WHyperEdgeCompanion[ WHEdgeT] => f; case _=> null}
        val lhF = edgeStream.factory match {
                    case f:  LHyperEdgeCompanion[ LHEdgeT] => f; case _=> null}
        val wlhF= edgeStream.factory match {
                    case f: WLHyperEdgeCompanion[WLHEdgeT] => f; case _=> null}

        def   e(a: EdgeAdapterBase[N,E,_]) = a match { case a:   EdgeAdapter[N,E,_]
            => nodesToEdgeCont  ( eF, a.nodes._1, a.nodes._2) }
        def   w(a: EdgeAdapterBase[N,E,_]) = a match { case a:  WEdgeAdapter[N,E,_]
            => nodesToWEdgeCont ( wF, a.weight, a.nodes._1, a.nodes._2) }
        def   l(a: EdgeAdapterBase[N,E,_]) = a match { case a:  LEdgeAdapter[N,E,_]
            => nodesToLEdgeCont ( lF, a.label, a.nodes._1, a.nodes._2) }
        def  wl(a: EdgeAdapterBase[N,E,_]) = a match { case a: WLEdgeAdapter[N,E,_]
            => nodesToWLEdgeCont(wlF, a.weight, a.label, a.nodes._1, a.nodes._2) }
        def   h(a: EdgeAdapterBase[N,E,_]) = a match { case a:   HyperEdgeAdapter[N,E,_]
            => { val nodes = a.nodes; assert(nodes.size >= 2)
                 nodesToEdgeCont  (  hF, nodes(0), nodes(1), nodes.tail.tail: _*) }}
        def  wh(a: EdgeAdapterBase[N,E,_]) = a match { case a:  WHyperEdgeAdapter[N,E,_]
            => { val nodes = a.nodes; assert(nodes.size >= 2)
                 nodesToWEdgeCont ( whF, a.weight, nodes(0), nodes(1), nodes.tail.tail: _*) }}
        def  lh(a: EdgeAdapterBase[N,E,_]) = a match { case a:  LHyperEdgeAdapter[N,E,_]
            => { val nodes = a.nodes; assert(nodes.size >= 2)
                 nodesToLEdgeCont ( lhF, a.label, nodes(0), nodes(1), nodes.tail.tail: _*) }}
        def wlh(a: EdgeAdapterBase[N,E,_]) = a match { case a: WLHyperEdgeAdapter[N,E,_]
            => { val nodes = a.nodes; assert(nodes.size >= 2)
                 nodesToWLEdgeCont(wlhF, a.weight, a.label, nodes(0), nodes(1), nodes.tail.tail: _*) }}

        val make = edgeStream.factory match {
          case _: WLEdgeCompanion[E] => wl _
          case _:  WEdgeCompanion[E] =>  w _
          case _:  LEdgeCompanion[E] =>  l _
          case _:   EdgeCompanion[E] =>  e _
          case _: WLHyperEdgeCompanion[E] => wlh _
          case _:  WHyperEdgeCompanion[E] =>  wh _
          case _:  LHyperEdgeCompanion[E] =>  lh _
          case _:   HyperEdgeCompanion[E] =>   h _
        }
        override def hasNext = edgeStream.hasNext
        override def next    = make(edgeStream.next)
      }
      private[EdgeContStream] class ListWorker(edgeList: Iterable[E[N]])
        extends Iterator[E[NodeT]]
      {
        val it = edgeList.iterator
        override def hasNext = it.hasNext
        override def next    = edgeToEdgeCont(it.next)
      }
    }
  }
  type EdgeSetT <: EdgeSetAux
  trait EdgeSetAux extends EdgeSet {
    /**
     * This method may be called by an auxiliary constructor. If so, it must be defined
     * by the trait responsible for the implementation of the graph representation.
     *
     * $INITIMPL
     * 
     * For parameter descriptions see `NodeAux.from`.
     */
    protected[collection]
    def from (nodeStreams: Iterable[NodeInputStream[N]],
              nodes:       Iterable[N],
              edgeStreams: Iterable[GenEdgeInputStream[N,E]],
              edges:       Iterable[E[N]]): Unit
  }
}
/**
 * Template for graph companion objects supporting stream-based graph creation.
 * 
 * @tparam CC the kind of type of the graph that is to become the companion class/trait
 *         of the object extending this trait. 
 *
 * @define NSTREAMS list of node input streams to be processed. All nodes read from any
 *         of these streams will be added to this graph. Note that only isolated nodes
 *         must be included in a stream or in `nodes`, non-isolated nodes are optional.
 * @define INNODES The isolated (and optionally any other) outer nodes that the node set of
 *         this graph is to be populated with. This parameter may be used as an alternative
 *         or in addition to `nodeStreams`.
 * @define ESTREAMS list of edge input streams, each with its own edge factory,
 *         to be processed. All edges and edge ends (nodes) read from any of these streams
 *         will be added to this graph.
 * @define INEDGES The outer edges that the edge set of this graph is to be populated with.
 *         Nodes being the end of any of these edges will be added to the node set.
 *         This parameter is meant be used as an alternative or in addition to `edgeStreams`.
 * @author Peter Empen
 */
trait GraphAuxCompanion[+CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC[N,E]]]
{
  /**
   * Creates a graph with nodes and edges read in from the input streams
   * `nodeStreams`/`edgeStreams` and from `nodes`/`edges`.
   * 
   * Node/edge streams are an efficient way to populate `Graph` instances from external
   * resources such as a database. The user has to implement his input stream classes
   * deriving them from `NodeInputStream`/`EdgeInputStream`.   
   * 
   * @tparam N type of nodes.
   * @tparam E kind of type of edges.
   * @param nodeStreams $NSTREAMS
   * @param nodes       $INNODES
   * @param edgeStreams $ESTREAMS
   * @param edges       $INEDGES
   */
  def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): CC[N,E]
}