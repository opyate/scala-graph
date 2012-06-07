package scalax.collection.constrained
package generic

import collection.Iterable
import collection.mutable.{Builder, ListBuffer}
import collection.generic.CanBuildFrom

import scalax.collection.GraphEdge.EdgeLike
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut, NodeIn}
import scalax.collection.generic.GraphCompanion

import constraints.NoneConstraint
import mutable.GraphBuilder

/**
 * Methods common to `Graph` companion objects in the constrained module.
 * 
 * @define NONEC `cFactory` equaling to [[package scalax.collection.constrained.NoneConstraint]] 
 */
abstract class GraphFactory[GC[N,E[X]<:EdgeLikeIn[X]] <:
                            Graph[N,E] with GraphLike[N,E,GC[N,E]]]
  extends scalax.collection.generic.GraphFactory[GC]
{
  override type Coll = GC[_,Nothing]
  /**
   * Same as `empty(cFactory)` but with $NONEC. */
  final def empty[N, E[X] <: EdgeLikeIn[X]] = empty(NoneConstraint)
  /**
   * Creates an empty `Graph` instance with the constraint yielded by `cFactory.apply`. */
  def empty[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint]): GC[N,E]
  /**
   * Same as `apply(cFactory)(elems)` but with $NONEC. */
  override final def apply[N, E[X] <: EdgeLikeIn[X]](elems: GraphParamIn[N,E]*) =
    apply[N,E](NoneConstraint)(elems: _*)
  /**
   * Creates a `Graph` with a node set built from all nodes in `elems` including
   * edge ends, with an edge set containing all edges in `elems` and with the constraint
   * yielded by `cFactory.apply`.
   * $DUPLEXCL
   * 
   * @param cFactory the constraint factory
   * @param elems sequence of nodes and/or edges in an arbitrary order
   * @return A new graph instance containing the nodes and edges derived from `elems`.
   */
  def apply[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                      (elems   : GraphParamIn[N,E]* ): GC[N,E]
    = (newBuilder[N,E](cFactory) ++= elems).result
  /**
   * Same as `from(cFactory)(nodes, edges)` but with $NONEC. */
  final def from [N, E[X] <: EdgeLikeIn[X]](nodes: collection.Iterable[N],
                                            edges: collection.Iterable[E[N]]) =
    from[N,E](NoneConstraint)(nodes, edges)
  /**
   * Produces a graph with a node set containing all `nodes` and edge ends in `edges`,
   * with an edge set containing all `edges` and with the constraint yielded by
   * `cFactory.apply`.
   * $DUPLEXCL
   * 
   * @param cFactory the constraint factory
   * @param nodes the isolated and optionally any other non-isolated nodes to
   *        be included in the node set of the graph to be created.
   * @param edges $EDGES
   * @return  A new graph instance containing `nodes` and all edge ends
   *          and `edges`.
   */
  def from [N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                      (nodes:    Iterable[N],
                                       edges:    Iterable[E[N]]): GC[N,E]
  /**
   * Same as `from(cFactory)(nodes, edges)` with all checks constraint suppressed. */
  protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                              (nodes:    Iterable[N],
                                               edges:    Iterable[E[N]]): GC[N,E]
  /**
   * Same as `fill(cFactory)(nodes, edges)` but with $NONEC. */
  override final def fill[N, E[X] <: EdgeLikeIn[X]] (nr: Int)(elem: => GraphParamIn[N,E]) =
    fill[N,E](NoneConstraint)(nr)(elem)
  /**
   * Produces a graph containing the results of some element computation a number of times.
   * $DUPLEXCL
   *
   * @param nr the number of elements to be contained in the graph.
   * @param elem the element computation.
   * @return A graph that contains the results of `nr` evaluations of `elem`.
   */
  def fill[N, E[X] <: EdgeLikeIn[X]](cFactory: ConstraintCompanion[Constraint])
                                    (nr: Int)
                                    (elem: => GraphParamIn[N,E]) = {
    val gB = newBuilder[N,E](cFactory).asInstanceOf[GraphBuilder[N,E,GC]]
    gB.sizeHint(nr)
    var i = 0
    while (i < nr) {
      gB += elem
      i += 1
    }
    gB.result
  }
  override final def newBuilder[N, E[X] <: EdgeLikeIn[X]] =
    newBuilder[N,E](NoneConstraint)
  def newBuilder[N, E[X] <: EdgeLikeIn[X]]
                (cFactory: ConstraintCompanion[Constraint]): Builder[GraphParamIn[N,E], GC[N,E]]
    = new GraphBuilder[N,E,GC](this, cFactory)
  class GraphCanBuildFrom[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    extends CanBuildFrom[Coll, GraphParamIn[N,E], GC[N,E]]
  {
    override def apply(from: Coll) = apply
    override def apply             = newBuilder[N,E](cFactory)
  }
}
abstract class MutableGraphFactory[GC[N,E[X]<:EdgeLikeIn[X]] <:
                                   mutable.Graph[N,E] with mutable.GraphLike[N,E,GC[N,E]]]
  extends GraphFactory[GC]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
    = new GraphBuilder[N,E,GC](this, cFactory)
}
abstract class ImmutableGraphFactory[GC[N,E[X]<:EdgeLikeIn[X]] <:
                                     immutable.Graph[N,E] with GraphLike[N,E,GC[N,E]]]
  extends GraphFactory[GC]
