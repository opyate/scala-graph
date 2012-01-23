package scalax.collection.constrained

import collection.{Set, SetLike, GenTraversableOnce}

import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam,
       GraphParamIn, GraphParamOut, seqToGraphParam, NodeIn, NodeOut, EdgeIn, EdgeOut}
import scalax.collection.GraphEdge.{EdgeLike, EdgeCompanionBase}
import scalax.collection.{GraphLike => SimpleGraphLike, Graph => SimpleGraph}
import scalax.collection.io._

import generic.GraphFactory

/**
 * A template trait for graphs.
 * 
 * This trait provides the common structure and operations of immutable graphs independently of its representation.
 * 
 * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
 * 
 * @tparam N    the user type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges (links) in this graph.
 * @tparam This the type of the graph itself.
 *
 * @author Peter Empen
 */
trait GraphLike[N,
                E[X]  <: EdgeLikeIn[X],
                +This <: GraphLike[N,E,This] with Set[GraphParam[N,E]] with Graph[N,E]]
  extends SimpleGraphLike[N,E,This]
  with    Constrained[N,E]
{ this: This =>
  /** Downcasts `graphCompanion` to a companion of the constrained module. */
  final def constrainedCompanion: Option[GraphFactory[Graph]] =
    Option(graphCompanion match { case f: GraphFactory[Graph] => f
                                        case _ => null })
  override protected def plusPlus(newNodes: Iterable[N], newEdges: Iterable[E[N]]): This =
    constrainedCompanion.getOrElse(throw new UnsupportedOperationException).
    fromUnchecked[N,E](constraintFactory)(
                       nodes.toNodeInSet ++ newNodes,
                       edges.toEdgeInSet ++ newEdges).asInstanceOf[This]
  override protected def minusMinus(delNodes: Iterable[N], delEdges: Iterable[E[N]]): This = {
    val delNodesEdges = minusMinusNodesEdges(delNodes, delEdges)
    constrainedCompanion.getOrElse(throw new UnsupportedOperationException).
      fromUnchecked[N,E](constraintFactory)(
                         delNodesEdges._1, delNodesEdges._2).asInstanceOf[This]
  }
  /** This flag is used to prevent constraint checking for single additions and
   * subtractions triggered by a multiple addition/subtraction such as `++=`.
   */
  @transient protected var checkSuspended = false
  protected final def withoutChecks[R] (exec: => R): R = {
    val oldSuspended = checkSuspended
    checkSuspended = true
      val res = exec
    checkSuspended = oldSuspended
    res
  }

  import PreCheckFollowUp._
  override def ++ (elems: GenTraversableOnce[GraphParam[N,E]]): this.type =
  { var graph = this.asInstanceOf[This]
    val it = elems match {
      case x: Iterable       [GraphParam[N,E]] => x
      case x: TraversableOnce[GraphParam[N,E]] => x.toIterable
      case _ => throw new IllegalArgumentException("TraversableOnce expected.")
    }
    val p = new GraphParam.Partitions[N,E](it filter (elm => !(this contains elm)))
    val inFiltered = p.toInParams.toSet.toSeq
    val (outerNodes, outerEdges) = (p.toOuterNodes, p.toOuterEdges)
    var handle = false
    val preCheckResult = preAdd(inFiltered: _*)
    preCheckResult.followUp match { 
      case Complete  => graph = plusPlus(outerNodes, outerEdges)
      case PostCheck => graph = plusPlus(outerNodes, outerEdges)
        if (! postAdd(graph, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          graph = this.asInstanceOf[This]
        }
      case Abort     => handle = true
    }
    if (handle) onAdditionRefused(outerNodes, outerEdges, graph)

    graph.asInstanceOf[this.type]
  } 
  override def -- (elems: GenTraversableOnce[GraphParam[N,E]]) =
  { var graph = this.asInstanceOf[This]

    lazy val p = partition(elems)
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
      case Complete  => graph = minusMinus(outerNodes, outerEdges)
      case PostCheck => graph = minusMinus(outerNodes, outerEdges)
        if (! postSubtract(graph, outerNodes, outerEdges, preCheckResult)) {
          handle = true
          graph = this.asInstanceOf[This]
        }
      case Abort     => handle = true
    }
    if (handle) onSubtractionRefused(innerNodes, innerEdges, graph)

    graph.asInstanceOf[this.type]
  }
}

// ----------------------------------------------------------------------------
import collection.generic.CanBuildFrom

import generic.GraphFactory
/**
 * A trait for dynamically constrained graphs.
 * 
 * @tparam N    the type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges in this graph. 
 *
 * @author Peter Empen
 */
trait Graph[N, E[X] <: EdgeLikeIn[X]]
  extends Set        [GraphParam[N,E]]
  with    SimpleGraph[N,E]
  with    GraphLike  [N,E,Graph[N,E]]
{
  override def empty: Graph[N,E] = Graph.empty[N,E](constraintFactory)
  val constraintFactory: ConstraintCompanion[Constraint]
}
/**
 * Default factory for constrained graphs.
 * Graph instances returned from this factory will be immutable.
 * 
 * @author Peter Empen
 */
object Graph
  extends GraphFactory[Graph]
  with    GraphAuxCompanion[Graph]
{
  override def newBuilder[N, E[X] <: EdgeLikeIn[X]]
                         (cFactory: ConstraintCompanion[Constraint])
    = immutable.Graph.newBuilder[N,E](cFactory)
  override def empty     [N, E[X] <: EdgeLikeIn[X]]
                         (cFactory: ConstraintCompanion[Constraint]): Graph[N,E]
    = immutable.Graph.empty[N,E](cFactory)
  override def from      [N, E[X] <: EdgeLikeIn[X]]
                         (cFactory: ConstraintCompanion[Constraint])
                         (nodes: Iterable[N],
                          edges: Iterable[E[N]]): Graph[N,E]
    = immutable.Graph.from[N,E](cFactory)(nodes, edges)
  override protected[collection]
  def fromUnchecked[N, E[X] <: EdgeLikeIn[X]] (cFactory: ConstraintCompanion[Constraint])
                                              (nodes:    Iterable[N],
                                               edges:    Iterable[E[N]]): Graph[N,E]
    = immutable.Graph.fromUnchecked[N,E](cFactory)(nodes, edges)
  override def fromStream [N, E[X] <: EdgeLikeIn[X]]
     (cFactory: ConstraintCompanion[Constraint])
     (nodeStreams: Seq[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
      nodes:       Iterable[N]             = Seq.empty[N],
      edgeStreams: Seq[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
      edges:       Iterable[E[N]]          = Seq.empty[E[N]]): Graph[N,E]
    = immutable.Graph.fromStream[N,E](cFactory)(
                                      nodeStreams, nodes, edgeStreams, edges)
//  implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]]: CanBuildFrom[Coll, GraphParamIn[N,E], Graph[N,E]] =
//    new GraphCanBuildFrom[N,E]
}
trait UserConstrainedGraph[N, E[X] <: EdgeLikeIn[X]]
  extends Graph[N,E]
{
  val constraint: Constraint[N,E]
 /*
   * delegating from Constrained to Constraint;
   * asInstanceOf is safe because 'this' is set to 'constraint.self' 
   */
  private type C_NodeT = constraint.self.NodeT
  private type C_EdgeT = constraint.self.EdgeT

  override def preCreate(nodes: collection.Iterable[N],
                         edges: collection.Iterable[E[N]]) =
                                    constraint preCreate (nodes, edges)
  override def preAdd(node: N   ) = constraint preAdd node
  override def preAdd(edge: E[N]) = constraint preAdd edge
  override def preAdd(elems: GraphParamIn[N,E]*) = constraint preAdd (elems: _*)
  override def postAdd (newGraph   : Graph[N,E],
                        passedNodes: Iterable[N],
                        passedEdges: Iterable[E[N]],
                        preCheck   : PreCheckResult) =
    constraint postAdd (newGraph, passedNodes, passedEdges, preCheck)

  override def preSubtract (node: self.NodeT, forced: Boolean) =
    constraint preSubtract (node.asInstanceOf[C_NodeT], forced)
  override def preSubtract (edge: self.EdgeT, simple: Boolean) =
    constraint preSubtract (edge.asInstanceOf[C_EdgeT], simple)
  override def preSubtract (nodes: => Set[self.NodeT],
                            edges: => Set[self.EdgeT], simple: Boolean) =
    constraint preSubtract (nodes.asInstanceOf[Set[C_NodeT]],
                            edges.asInstanceOf[Set[C_EdgeT]],
                            simple)
  override def postSubtract(newGraph   : Graph[N,E],
                            passedNodes: Iterable[N],
                            passedEdges: Iterable[E[N]],
                            preCheck   : PreCheckResult) =
    constraint postSubtract (newGraph, passedNodes, passedEdges, preCheck)

  override def onAdditionRefused   (refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E]) =
    constraint onAdditionRefused   (refusedNodes, refusedEdges, graph)
  override def onSubtractionRefused(refusedNodes: Iterable[Graph[N,E]#NodeT],
                                    refusedEdges: Iterable[Graph[N,E]#EdgeT],
                                    graph:        Graph[N,E]) =
    constraint onSubtractionRefused(refusedNodes.asInstanceOf[Iterable[C_NodeT]],
                                    refusedEdges.asInstanceOf[Iterable[C_EdgeT]],
                                    graph)
}
