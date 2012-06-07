package scalax.collection.constrained

/**
 * Mutable constrained graph templates.
 * 
 * @author Peter Empen
 */
package object mutable {
  import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
  import scalax.collection.io._
  import constraints._ 
  import generic._

  /** Enables to quickly assemble mutable constrained graph companion modules. Example:
   *  {{
   *  import scalax.collection.constrained.mutable.CompanionAlias
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
  /** Mutable directed acyclic `Graph`. */
  type DAG[N] = Graph[N,DiEdge]
  /** Companion module for mutable directed acyclic `Graph`. */
  object DAG extends CompanionAlias[DiEdge](dagConstraint)

  /** Mutable undirected acyclic `Graph`. */
  type Forest[N] = Graph[N,UnDiEdge]
  /** Companion module for mutable undirected acyclic `Graph`. */
  object Forest extends CompanionAlias[UnDiEdge](forestConstraint)

  /** Mutable undirected connected acyclic `Graph`. */
  type Tree[N] = Graph[N,UnDiEdge]
  /** Companion module for mutable undirected connected acyclic `Graph`. */
  object Tree extends CompanionAlias[UnDiEdge](treeConstraint)
}
