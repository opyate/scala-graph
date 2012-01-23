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
  import constraints._ 

  /** Default (immutable) directed acyclic `Graph`. */
  type DAG[N] = Graph[N,DiEdge]
  object DAG {
    def apply[N](elems: GraphParamIn[N,DiEdge]*) = Graph(Acyclic)(elems: _*)
  }
  type Forest[N] = Graph[N,UnDiEdge]
  object Forest {
    def apply[N](elems: GraphParamIn[N,UnDiEdge]*) = Graph(Acyclic)(elems: _*)
  }
  type Tree[N] = Graph[N,UnDiEdge]
  object Tree {
    def apply[N](elems: GraphParamIn[N,UnDiEdge]*) = Graph(Connected && Acyclic)(elems: _*)
  }
}
