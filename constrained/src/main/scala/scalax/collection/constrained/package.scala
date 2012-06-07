package scalax.collection

import GraphPredef._, GraphEdge._

/**
 * Traits enabling to implement constraints and use constrained graphs.
 * 
 * Graphs may be constrained dynamically or statically.
 *
 * ''Dynamically constrained'' means that a constraint is bound to a constrained `Graph`
 * instance at initialization time. The constrained `Graph` will then delegate all calls
 * to the methods of `ConstraintMethods` and `ConstraintHandlerMethods` to the
 * corresponding methods of the constraint bound to it.
 * The immutable and mutable factories `Graph` in this package yield dynamically
 * constrained graphs.
 * 
 * To make use of dynamically constrained graphs you may make use of the predefined
 * constraints or provide an own implementation of `Constraint` along with its companion
 * object. To initialize a graph with one or several combined constraints just call
 * the graph factory methods of the `constraint` package passing.  
 *
 * ''Statically constrained'' means that the graph class directly implements
 * the methods declared in `ConstraintMethods`.
 * 
 * @author Peter Empen
 */
package object constrained {
  import io._
  import constraints._ 
  import generic._

  /** Enables to quickly assemble immutable constrained graph companion modules. Example:
   *  {{
   *  import scalax.collection.constrained.CompanionAlias
   *  import scalax.collection.constrained.constraints.Acyclic
   *  
   *  object DAG extends CompanionAlias[DiEdge](Acyclic withStringPrefix "DAG")
   *  }}
   */
  abstract class CompanionAlias[E[X] <: EdgeLikeIn[X]]
                (constraint: ConstraintCompanion[Constraint]) {
    def apply[N](elems: GraphParamIn[N,E]*) = Graph(constraint)(elems: _*)
    def from [N](nodes: Iterable[N],
                 edges: Iterable[E[N]])     = Graph.from(constraint)(nodes, edges)
    def fromStream [N]
       (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
        nodes:       Iterable[N]                  = Seq.empty[N],
        edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
        edges:       Iterable[E[N]]               = Seq.empty[E[N]])
      = Graph.fromStream(constraint)(nodeStreams, nodes, edgeStreams, edges)
  }
  /** Constraint representing a DAG. */
  def dagConstraint = Acyclic withStringPrefix "DAG"
  /** Default (immutable) directed acyclic `Graph`. */
  type DAG[N] = Graph[N,DiEdge]
  /** Companion module for default (immutable) directed acyclic `Graph`. */
  object DAG extends CompanionAlias[DiEdge](dagConstraint)

  /** Constraint representing a forest. */
  def forestConstraint = Acyclic withStringPrefix "Forest"
  /** Default (immutable) undirected acyclic `Graph`. */
  type Forest[N] = Graph[N,UnDiEdge]
  /** Companion module for default (immutable) undirected acyclic `Graph`. */
  object Forest extends CompanionAlias[UnDiEdge](forestConstraint)

  /** Constraint representing an undirected tree. */
  def treeConstraint = Connected && Acyclic withStringPrefix "Tree"
  /** Default (immutable) undirected connected acyclic `Graph`. */
  type Tree[N] = Graph[N,UnDiEdge]
  /** Companion module for default (immutable) undirected connected acyclic `Graph`. */
  object Tree extends CompanionAlias[UnDiEdge](treeConstraint)
}
