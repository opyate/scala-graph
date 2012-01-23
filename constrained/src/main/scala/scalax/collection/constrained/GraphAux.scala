package scalax.collection.constrained

import scalax.collection.GraphPredef.{EdgeLikeIn}
import scalax.collection.GraphEdge.{EdgeCompanionBase}
import scalax.collection.io._

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
 *
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
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Seq[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]             = Seq.empty[N],
      edgeStreams: Seq[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]          = Seq.empty[E[N]]): CC[N,E]
}