package scalax.collection.constrained
package immutable

import scala.collection.generic.CanBuildFrom

import scalax.collection.GraphPredef.{EdgeLikeIn, NodeIn, EdgeIn, GraphParamIn}
import scalax.collection.constrained.{Constrained, Constraint}
import scalax.collection.immutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}
import PreCheckFollowUp._

trait AdjacencyListGraph[
      N,
      E[X] <: EdgeLikeIn[X],
     +This <: AdjacencyListGraph[N,E,This] with Graph[N,E]]  
  extends GraphLike[N,E,This]
  with    SimpleAdjacencyListGraph[N,E,This]
{ this: This =>
  override protected def initialize(nodes: Iterable[N],
                                    edges: Iterable[E[N]] ) {
    withoutChecks { super.initialize(nodes, edges) }
  }
  /** generic constrained addition */
  protected def checkedAdd[G >: This]
            ( contained: => Boolean,
              preAdd:    => PreCheckResult,
              copy:      => G,
              nodes:     => Iterable[N],
              edges:     => Iterable[E[N]] ): This =
  { if (checkSuspended)
      copy.asInstanceOf[This]
    else {
      var graph = this
      if (! contained) {
        var handle = false
        val preCheckResult = preAdd
        preCheckResult.followUp match { 
          case Complete  => graph = copy.asInstanceOf[This]
          case PostCheck => graph = copy.asInstanceOf[This]
            if (! postAdd(graph, nodes, edges, preCheckResult)) {
              handle = true
              graph = this
            }
          case Abort     => handle = true
        }
        if (handle) onAdditionRefused(nodes, edges, this)
      }
      graph
    }
  }
  override def + (node: N) = 
    checkedAdd (contained = _nodes contains Node(node),
                preAdd    = preAdd(node),
                copy      = copy(_nodes.toNodeInSet.toBuffer += node,
                                 _edges.toEdgeInSet),
                nodes     = Set(node),
                edges     = Set.empty[E[N]])

  override protected def +#(edge: E[N]) =
    checkedAdd (contained = _edges contains Edge(edge),
                preAdd    = preAdd(edge),
                copy      = copy(_nodes.toNodeInSet,
                                 _edges.toEdgeInSet.toBuffer += edge),
                nodes     = Set.empty[N],
                edges     = Set(edge))
  /** generic constrained subtraction of nodes */
  protected def checkedSubtractNode[G >: This]
              ( node:       N,
                forced:     Boolean,
                copy:       (N, NodeT) => G): This =
  { _nodes find node map { innerNode =>
      def subtract = copy(node, innerNode).asInstanceOf[This]
      if (checkSuspended)
        copy(node, innerNode).asInstanceOf[This]
      else {
        var graph = this
        var handle = false
        val preCheckResult = preSubtract(innerNode.asInstanceOf[self.NodeT], forced)
        preCheckResult.followUp match { 
          case Complete  => graph = subtract
          case PostCheck => graph = subtract
            if (! postSubtract(graph, Set(node), Set.empty[E[N]], preCheckResult)) {
              handle = true
              graph = this
            }
          case Abort     => handle = true
        }
        if (handle) onSubtractionRefused(Set(innerNode), Set.empty[self.EdgeT], graph)
        graph
      }
    } getOrElse this
  }
  override def - (n: N) = checkedSubtractNode(
    n, true,
    (outeNode: N, innerNode: NodeT) =>
      copy (_nodes.toNodeInSet.toBuffer  -= outeNode,
            _edges.toEdgeInSet.toBuffer --= (innerNode.edges map (_.toEdgeIn))))
  override def -? (n: N) = checkedSubtractNode(
    n, false,
    (outerNode: N, innerNode: NodeT) => {
      var newNodes = _nodes.toNodeInSet.toBuffer
      var newEdges = _edges.toEdgeInSet.toBuffer
      nodes.subtract(innerNode,
                     false,
                     innerNode => newNodes  -= outerNode,
                     innerNode => newEdges --= (innerNode.edges map (_.toEdgeIn)))
      copy(newNodes, newEdges)
    }
  )
  /** generic constrained subtraction of edges */
  protected def checkedSubtractEdge[G >: This]
              ( edge:       E[N],
                simple:     Boolean,
                copy:       (E[N], EdgeT) => G): This =
  { _edges find edge map { innerEdge =>
      def subtract = copy(edge, innerEdge).asInstanceOf[This]
      if (checkSuspended) subtract
      else {
        var graph = this
        var handle = false
        val preCheckResult = preSubtract(innerEdge.asInstanceOf[self.EdgeT], simple)
        preCheckResult.followUp match { 
          case Complete  => graph = subtract 
          case PostCheck => graph = subtract
            if (! postSubtract(graph, Set.empty[N], Set(edge), preCheckResult)) {
              handle = true
              graph = this
            }
          case Abort     => handle = true
        }
        if (handle) onSubtractionRefused(Set.empty[self.NodeT],
                                          Set(innerEdge.asInstanceOf[self.EdgeT]), graph)
        graph
      }
    } getOrElse this
  }
  override protected def -# (e: E[N]) = checkedSubtractEdge(
    e, true, (outerEdge: E[N], innerEdge: EdgeT) =>
                copy(_nodes.toNodeInSet, _edges.toEdgeInSet.toBuffer -= outerEdge))
  override protected def -!#(e: E[N]) = checkedSubtractEdge(
    e, false,
    (outerEdge: E[N], innerEdge: EdgeT) =>
      copy (_nodes.toNodeInSet.toBuffer --= innerEdge.privateNodes map (n => n.value),
            _edges.toEdgeInSet.toBuffer -= e))
}