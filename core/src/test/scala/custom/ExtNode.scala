package custom

import scala.collection.generic.CanBuildFrom

import scalax.collection._
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._

// here goes the real extension -----------------------------------------------

/**	This trait extends the inner node type of Graph by inheritance.
 *  Inner nodes will have an additional method `helloAdjacents`.
 */
trait MyExtGraphLike[N,
                     E[X]  <: EdgeLikeIn[X],
                     +This <: MyExtGraphLike[N,E,This] with Set[GraphParam[N,E]] with Graph[N,E]]
  extends GraphLike[N,E,This]
{
  trait InnerNodeLike extends super.InnerNodeLike {
    def helloSuccessors = "Hello " + (sorted(diSuccessors) mkString ",") + "!"

    /* we need the following sorting for test purposes only;
     * it ensures that the elements in the returned string
     * are in the same order as any expected test result.
     */
    protected def sorted(nodes: collection.Set[NodeT]) =
      (nodes map (_.toString) toList).sorted
  }
}

/* the rest is a lot but almost just copy & paste -----------------------------
 * Here the components must be assembled anew to include the extension.
 * I'm going to elaborate a more elegant alternative...
 */
package immutable {
  import scalax.collection.immutable.{Graph => ImmutableGraph, AdjacencyListGraph}
  import scalax.collection.generic.ImmutableGraphFactory

  /** Custom immutable Graph based on the default implementation
   *  and extended by `MyExtGraphLike`.
   *  You just need to replace the occurrences of `MyExtGraphLike`.  
   */
  class MyExtGraph[N, E[X] <: EdgeLikeIn[X]]
      ( iniNodes: Iterable[N]    = Set[N](),
        iniEdges: Iterable[E[N]] = Set[E[N]]() )
    extends ImmutableGraph[N,E]
    with    AdjacencyListGraph[N,E,MyExtGraph[N,E]]
    with    GraphTraversalImpl[N,E]
    with    Serializable
    with    MyExtGraphLike[N,E,MyExtGraph[N,E]] // extension trait
  {
    def graphCompanion = MyExtGraph
    protected val _nodes = new NodeSet 
    protected val _edges = new EdgeSet
    initialize(iniNodes, iniEdges)
  
    @inline final override def empty = MyExtGraph.empty[N,E]
    @inline final override def clone = MyExtGraph.from [N,E](_nodes.toNodeInSet,
                                                             _edges.toEdgeInSet)
    @inline final override def copy(nodes: Iterable[N],
                                    edges: Iterable[E[N]])=
      MyExtGraph.from[N,E](nodes, edges)
    final protected class NodeBase(override val value: N)
      extends super.NodeBase(value)
      with    InnerNodeImpl
      with    InnerNodeLike // inner class of  extension trait
      with    InnerNodeTraversalImpl
    type NodeT = NodeBase
    @inline final protected def newNode(n: N) = new NodeT(n)
  }
  object MyExtGraph extends ImmutableGraphFactory[MyExtGraph]
  {
    def empty[N, E[X] <: EdgeLikeIn[X]] = new MyExtGraph[N,E]
    override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                                 edges: Iterable[E[N]])=
      new MyExtGraph[N,E](nodes, edges)
    implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], MyExtGraph[N,E]] =
      new GraphCanBuildFrom[N,E]
  }
}
package mutable {
  import scalax.collection.mutable.{Graph => MutableGraph, AdjacencyListGraph} 
  import scalax.collection.generic.MutableGraphFactory

  /** The mutable variant of `MyExtGraph`. 
   *  You just need to replace the occurrences of `MyMutableExtGraph`.  
   */
  class MyExtGraph[N, E[X] <: EdgeLikeIn[X]]
      ( iniNodes: Iterable[N]    = Set[N](),
        iniEdges: Iterable[E[N]] = Set[E[N]]() )
    extends MutableGraph[N,E]
    with    AdjacencyListGraph[N,E,MyExtGraph[N,E]]
    with    GraphTraversalImpl[N,E]
    with    Serializable
    with    MyExtGraphLike[N,E,MyExtGraph[N,E]] // extension trait
  {
    def graphCompanion = MyExtGraph
    protected val _nodes = new NodeSet 
    protected val _edges = new EdgeSet
    initialize(iniNodes, iniEdges)
  
    @inline final override def empty = MyExtGraph.empty[N,E]
    @inline final override def clone = MyExtGraph.from [N,E](_nodes.toNodeInSet,
                                                             _edges.toEdgeInSet)
    final protected class NodeBase(override val value: N)
      extends super.NodeBase(value)
      with    InnerNodeImpl
      with    InnerNodeLike // inner class of  extension trait
      with    InnerNodeTraversalImpl
    type NodeT = NodeBase
    @inline final protected def newNode(n: N) = new NodeT(n)
  }
  object MyExtGraph extends MutableGraphFactory[MyExtGraph]
  {
    def empty[N, E[X] <: EdgeLikeIn[X]] = new MyExtGraph[N,E]
    override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                                 edges: Iterable[E[N]])=
      new MyExtGraph[N,E](nodes, edges)
    implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], MyExtGraph[N,E]] =
      new GraphCanBuildFrom[N,E]
  }
}
