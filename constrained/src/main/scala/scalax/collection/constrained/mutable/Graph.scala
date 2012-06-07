package scalax.collection.constrained
package mutable

import collection.{Set, Iterable}
import collection.generic.{CanBuildFrom, Growable, Shrinkable}
import collection.mutable.{Builder, Cloneable, ListBuffer, Set => MutableSet}

import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut,
                                      GraphParamNode, NodeIn, NodeOut, EdgeIn, EdgeOut}
import scalax.collection.GraphTraversalImpl
import scalax.collection.mutable.BuilderImpl
import scalax.collection.io._
import scalax.collection.constrained.{Graph => CGraph}
import generic.GraphFactory 
import PreCheckFollowUp._

class GraphBuilder[N,
                   E[X] <: EdgeLikeIn[X],
                   GC[N,E[X] <: EdgeLikeIn[X]] <: scalax.collection.constrained.Graph[N,E] with
                                                  scalax.collection.constrained.GraphLike[N,E,GC[N,E]]]
      (graphFactory     : GraphFactory[GC],
       constraintFactory: ConstraintCompanion[Constraint]) 
  extends BuilderImpl[N,E,GC]
{
  def result: This = graphFactory.from(constraintFactory)(nodes, edges)
}
trait GraphLike[N,
                E[X] <: EdgeLikeIn[X],
               +This <: GraphLike[N, E, This] with Graph[N,E]]
	extends scalax.collection.mutable.GraphLike[N, E, This]
	with	  scalax.collection.constrained.GraphLike[N, E, This]
  with    Growable  [GraphParam[N,E]]
	with	  Shrinkable[GraphParam[N,E]] 
	with	  Cloneable [Graph[N,E]] 
  with    Mutable
{ selfGraph: This =>
  trait NodeSet extends super.NodeSet {
    /** generic constrained subtraction */
    protected def checkedRemove(node: NodeT, ripple: Boolean): Boolean = {
      def remove = withoutChecks { subtract(node, ripple,  minus, minusEdges) }
      var removed, handle = false
      if (checkSuspended) removed = remove
      else {
        val preCheckResult = preSubtract(node.asInstanceOf[self.NodeT], ripple)
        preCheckResult.followUp match { 
          case Complete  => removed = remove
          case PostCheck => removed = remove
            if (removed &&
                ! postSubtract(selfGraph, Set(node), Set.empty[E[N]], preCheckResult)) {
              handle = true
              selfGraph  += node.value
              selfGraph ++= node.edges
            }
          case Abort     => handle = true
        }
      }
      if (handle) onSubtractionRefused(Set(node.asInstanceOf[self.NodeT]),
                                       Set.empty[self.EdgeT], selfGraph)
      removed && ! handle
    }
    override def remove      (node: NodeT) = checkedRemove(node, true)
    override def removeGently(node: NodeT) = checkedRemove(node, false)
  }
  /** generic checked addition */
  protected def checkedAdd[G >: This]
            ( contained: => Boolean,
              preAdd:    => PreCheckResult,
              copy:      => G,
              nodes:     => Iterable[N],
              edges:     => Iterable[E[N]] ): This =
  { if (contained) this
    else if (checkSuspended) copy.asInstanceOf[This]
    else {
      var graph = this
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
      graph
    }
  }
  override def + (node: N) = 
    checkedAdd (contained = nodes contains Node(node),
                preAdd    = preAdd(node),
                copy      = clone += node,
                nodes     = Set(node),
                edges     = Set.empty[E[N]])

  override def ++=(elems: TraversableOnce[GraphParam[N,E]]): this.type =
  { elems match {
      case elems: Iterable[GraphParam[N,E]] => 
        val p = new GraphParam.Partitions[N,E](elems)
        val inFiltered = p.toInParams.toSet.filter(elem => ! (this contains elem)).toSeq 
        var handle = false
        val preCheckResult = preAdd(inFiltered: _*)
        if (preCheckResult.abort)
          handle = true
        else {
          withoutChecks { super.++=(elems) }
          if (preCheckResult.postCheck) {
            val (outerNodes, outerEdges) = (p.toOuterNodes, p.toOuterEdges)
            if (! postAdd(this, outerNodes, outerEdges, preCheckResult)) {
              handle = true
              withoutChecks {
                super.--=(allNodes(outerNodes, outerEdges) map (n => NodeIn(n)))
              }
            }
          }
        }
        if (handle) onAdditionRefused(p.toOuterNodes, p.toOuterEdges, this)

      case _ => throw new IllegalArgumentException("Iterable expected")
    }
    this
  } 
  override def --=(elems: TraversableOnce[GraphParam[N,E]]): this.type =
  { lazy val p = partition(elems)
    lazy val (outerNodes, outerEdges) = (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    def innerNodes =
       (outerNodes.view map (this find _) filter (_.isDefined) map (_.get) force).toSet
    def innerEdges =
       (outerEdges.view map (this find _) filter (_.isDefined) map (_.get) force).toSet

    type C_NodeT = self.NodeT
    type C_EdgeT = self.EdgeT
    var handle = false
    val preCheckResult = preSubtract(innerNodes.asInstanceOf[Set[C_NodeT]],
                                     innerEdges.asInstanceOf[Set[C_EdgeT]], true)
    preCheckResult.followUp match { 
      case Complete  => withoutChecks { super.--=(elems) }
      case PostCheck =>
        val subtractables = elems filter (this contains _)
        withoutChecks { super.--=(subtractables) }
        if (! postSubtract(this, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          withoutChecks { super.++=(subtractables) }
        }
      case Abort     => handle = true
    }
    if (handle) onSubtractionRefused(innerNodes.asInstanceOf[Set[C_NodeT]],
                                     innerEdges.asInstanceOf[Set[C_EdgeT]], this)
    this
  }
}

import scalax.collection.constrained.generic.{GraphFactory, MutableGraphFactory}

trait Graph[N, E[X] <: EdgeLikeIn[X]]
	extends	scalax.collection.mutable.Graph[N,E]
  with    scalax.collection.constrained.Graph[N,E]
	with	  GraphLike[N, E, Graph[N,E]]
{
  override def empty: Graph[N,E] = Graph.empty[N,E](constraintFactory)
}
object Graph
  extends MutableGraphFactory[Graph]
  with    GraphAuxCompanion  [Graph]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]]
                    (cFactory: ConstraintCompanion[Constraint]): Graph[N,E]
    = DefaultGraphImpl.empty[N,E](cFactory)
  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                              (nodes:    Iterable[N],
                                               edges:    Iterable[E[N]]): Graph[N,E]
    = DefaultGraphImpl.fromUnchecked[N,E](cFactory)(nodes, edges)
  override def from [N, E[X] <: EdgeLikeIn[X]]
                    (cFactory: ConstraintCompanion[Constraint])
                    (nodes:   collection.Iterable[N],
                     edges:   collection.Iterable[E[N]]): Graph[N,E]
    = DefaultGraphImpl.from[N,E] (cFactory)(nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]               = Seq.empty[E[N]]): Graph[N,E]
    = DefaultGraphImpl.fromStream[N,E](cFactory)(
                                     nodeStreams, nodes, edgeStreams, edges)
//	implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
//		new GraphCanBuildFrom[N,E]
}
abstract class DefaultGraphImpl[N, E[X] <: EdgeLikeIn[X]]
                               (iniNodes: Iterable[N]    = Set[N](),
                                iniEdges: Iterable[E[N]] = Set[E[N]]())
  extends Graph[N,E]
  with    AdjacencyListGraph[N,E,DefaultGraphImpl[N,E]]
  with    GraphTraversalImpl[N,E]
{
  class NodeSet extends super[AdjacencyListGraph].NodeSet with super[Graph].NodeSet
  protected val _nodes = new NodeSet 
  protected val _edges = new EdgeSet 
  initialize(iniNodes, iniEdges)

  @inline final override def empty = DefaultGraphImpl.empty(constraintFactory)
  @inline final override def clone(): this.type = {
    constrainedCompanion.getOrElse(throw new UnsupportedOperationException).
      from[N,E](constraintFactory)(
                nodes.toNodeInSet, edges.toEdgeInSet).asInstanceOf[this.type]
  }
  @SerialVersionUID(8082L)
  protected class NodeBase(override val value: N)
    extends super.NodeBase(value)
    with    InnerNodeImpl
    with    InnerNodeTraversalImpl
  type NodeT = NodeBase
  @inline final protected def newNode(n: N) = new NodeT(n)
}
object DefaultGraphImpl
  extends MutableGraphFactory[DefaultGraphImpl]
  with    GraphAuxCompanion  [DefaultGraphImpl]
{
  override def empty[N, E[X] <: EdgeLikeIn[X]]
                    (cFactory: ConstraintCompanion[Constraint])
    = from (cFactory)(Set.empty[N], Set.empty[E[N]])

  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                              (nodes:    Iterable[N],
                                               edges:    Iterable[E[N]]): DefaultGraphImpl[N,E]
    = new UserConstrainedGraphImpl[N,E](cFactory)(nodes, edges)

  override def from [N, E[X] <: EdgeLikeIn[X]](cFactory: ConstraintCompanion[Constraint])
                                              (nodes: Iterable[N],
                                               edges: Iterable[E[N]]): DefaultGraphImpl[N,E] =
  { val existElems = nodes.nonEmpty || edges.nonEmpty 
    var preCheckResult = PreCheckResult(Abort)
    if (existElems) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      preCheckResult = constraint.preCreate(nodes, edges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (nodes, edges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = fromUnchecked[N,E](cFactory)(nodes, edges)
    if (existElems) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      var handle = false
      preCheckResult.followUp match {
        case Complete  =>
        case PostCheck => handle = ! constraint.postAdd(newGraph, nodes, edges, preCheckResult)
        case Abort     => handle = true
      }
      if (handle) {
        constraint.onAdditionRefused(nodes, edges, newGraph)
        emptyGraph
      } else
        newGraph
    } else
      newGraph
  }
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      iniNodes:    Iterable[N]                  = Seq.empty[N],
      edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      iniEdges:    Iterable[E[N]]               = Seq.empty[E[N]]): DefaultGraphImpl[N,E] =
  {
    var preCheckResult = PreCheckResult(Abort)
    if (iniNodes.nonEmpty || iniEdges.nonEmpty) {
      val emptyGraph = empty[N,E](cFactory)
      val constraint = cFactory(emptyGraph)
      preCheckResult = constraint.preCreate(iniNodes, iniEdges)
      if (preCheckResult.abort) { 
        constraint onAdditionRefused (iniNodes, iniEdges, emptyGraph) 
        return emptyGraph
      }
    }
    val newGraph = new UserConstrainedGraphImpl[N,E](cFactory)() {
      from(nodeStreams, iniNodes, edgeStreams, iniEdges)
    }
    var handle = false
    preCheckResult.followUp match {
      case Complete  =>
      case PostCheck => handle = ! newGraph.postAdd(newGraph, iniNodes, iniEdges, preCheckResult)
      case Abort     => handle = true
    }
    if (handle) {
      newGraph.onAdditionRefused(iniNodes, iniEdges, newGraph)
      empty[N,E](cFactory)
    } else
      newGraph
  }
  //implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], DefaultGraphImpl[N,E]] =
  //  new GraphCanBuildFrom[N,E]
}
class UserConstrainedGraphImpl[N, E[X] <: EdgeLikeIn[X]]
                              (cFactory: ConstraintCompanion[Constraint])
                              (iniNodes: Iterable[N]    = Set.empty[N],
                               iniEdges: Iterable[E[N]] = Set.empty[E[N]])
  extends DefaultGraphImpl    [N,E](iniNodes, iniEdges)
  with    UserConstrainedGraph[N,E]
{
  final override def graphCompanion = DefaultGraphImpl
  final override val self = this
  final override val constraintFactory = cFactory
  final override val constraint  = cFactory(this)
}
