package scalax.collection
package generic

import collection.mutable.{Builder, ListBuffer}
import collection.generic.CanBuildFrom

import GraphEdge.{EdgeLike, EdgeCompanionBase}
import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, NodeIn}
import mutable.GraphBuilder

/** Common tag trait for companion objects in any module. */
trait GraphCompanion

/**
 * Methods common to `Graph` companion objects in the core module.
 * 
 * @define DUPLEXCL Duplicate exclusion takes place on the basis of values
 *         returned by `hashCode` of the supplied nodes and edges. The hash-code
 *         value of an edge is determined by its ends and optionally by other
 *         edge components such as `weight` or `label`. To include non-node edge
 *         components in the hash-code of an edge make use of any of the predefined
 *         key-weighted/key-labeled edges or mix `ExtendedKey` into your custom
 *         edge class. 
 * @define EDGES all edges to be included in the edge set of the graph to be
 *         created. Edge ends will be added to the node set automatically.
 */
abstract class GraphFactory[CC[N, E[X] <: EdgeLikeIn[X]] <:
                            Graph[N,E] with GraphLike[N,E,CC[N,E]]]
  extends GraphCompanion
{
  protected type Coll = CC[_,Nothing]
  /** Creates an empty `Graph` instance. */
  def empty[N, E[X] <: EdgeLikeIn[X]]: CC[N,E]
  /** Creates a `Graph` with a node set built from all nodes in `elems` including
   * edge ends and with an edge set containing all edges in `elems`.
   * $DUPLEXCL
   * 
   * @param   elems sequence of nodes and/or edges in an arbitrary order
   * @return  A new graph instance containing the nodes and edges derived from `elems`.
   */
  def apply[N, E[X] <: EdgeLikeIn[X]](elems: GraphParamIn[N,E]*): CC[N,E] =
    (newBuilder[N,E] ++= elems).result
  /**
   * Produces a graph with a node set containing all edge ends in `edges` and
   * with an edge set containing all `edges` but duplicates.
   * $DUPLEXCL
   * 
   * @param   edges $EDGES
   * @return  A new graph instance containing the nodes derived from the edge ends
   *          and `edges`.
   */
  @inline final
  def from [N, E[X] <: EdgeLikeIn[X]](edges: collection.Iterable[E[N]]): CC[N,E] =
    from (Seq.empty[N], edges)
  /**
   * Produces a graph with a node set containing all `nodes` and edge ends in `edges`
   * and with an edge set containing all `edges` but duplicates.
   * $DUPLEXCL
   * 
   * @param nodes the isolated and optionally any other non-isolated nodes to
   *        be included in the node set of the graph to be created.
   * @param edges $EDGES
   * @return  A new graph instance containing `nodes` and all edge ends
   *          and `edges`.
   */
  def from [N, E[X] <: EdgeLikeIn[X]](nodes: collection.Iterable[N],
                                      edges: collection.Iterable[E[N]]): CC[N,E]
  /**
   * Produces a graph containing the results of some element computation a number of times.
   * $DUPLEXCL
   *
   * @param   nr  the number of elements to be contained in the graph.
   * @param   elem the element computation returning nodes or edges `nr` times.
   * @return  A graph that contains the results of `nr` evaluations of `elem`.
   */
  def fill[N, E[X] <: EdgeLikeIn[X]] (nr: Int)(elem: => GraphParamIn[N,E]): CC[N,E] =
  {
    val gB = newBuilder[N,E].asInstanceOf[GraphBuilder[N,E,CC]]
    gB.sizeHint(nr)
    var i = 0
    while (i < nr) {
      gB += elem
      i += 1
    }
    gB.result
  }
  def newBuilder[N, E[X] <: EdgeLikeIn[X]]: Builder[GraphParamIn[N,E], CC[N,E]] =
    new GraphBuilder[N,E,CC](this)
  class GraphCanBuildFrom[N, E[X] <: EdgeLikeIn[X]]
    extends CanBuildFrom[Coll, GraphParamIn[N,E], CC[N,E]]
  {
    def apply(from: Coll) = newBuilder[N,E]
    def apply() = newBuilder[N,E]
  }
}
abstract class MutableGraphFactory[CC[N, E[X] <: EdgeLikeIn[X]] <:
                                   mutable.Graph[N,E] with mutable.GraphLike[N,E,CC[N,E]]]
  extends GraphFactory[CC]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]]: Builder[GraphParamIn[N,E], CC[N,E]] =
    new GraphBuilder[N,E,CC](this)
}
abstract class ImmutableGraphFactory[CC[N, E[X] <: EdgeLikeIn[X]] <:
                                     immutable.Graph[N,E] with GraphLike[N,E,CC[N,E]]]
  extends GraphFactory[CC]
