package scalax.collection

import collection.mutable.ListBuffer
import GraphPredef.{EdgeLikeIn, GraphParamOut, NodeOut, EdgeOut}

/**
 * Defines traversal-related algorithmic interfaces.
 *
 * Graph traversal means to navigate from node to node based on incident edges.
 * Another kind of navigation is to iterate over nodes/edges based on the node-/edge-set.
 *
 * @define INTOACC taking optional filters and visitors into account.
 * @define VISITORS Node/edge visitor functions allow arbitrary user-defined computation
 *         during the traversal. 
 * @define DIRECTION Determines which connected nodes the traversal has to follow.
 *         Defaults to `Successors`.
 * @define NODEFILTER Predicate to filter the nodes to be visited during traversal.
 *         Defaults to `anyNode`, that is no filtering.
 *         A return of `true` signals that the traversal is to be canceled.
 * @define EDGEFILTER Predicate to filter the edges to be visited during traversal.
 *         Defaults to `anyEdge` that is no filtering.
 * @define NODEVISITOR Function called on visiting  a node.
 *         It can mutate the node or carry out any other side effect.
 *         Defaults to the empty function noNodeAction. 
 * @define EDGEVISITOR Function called on visiting an edge.
 *         It can mutate the node or carry out any other side effect.
 *         Defaults to the empty function noEdgeAction.
 * @define BREADTHFIRST If `true` the traversal is based on a breath first
 *         (BFS, layer-for-layer) search, otherwise on a depth first search (DFS).
 *         Default is BFS.
 * @define MAXDEPTH `0` - the default - indicates that the traversal should have
 *         an unlimited depth meaning that it will be continued either until
 *         it is cancelled by `nodeVisitor` or, in absence of a cancelation,
 *         until all nodes have been visited. A positiv value limits the number
 *         of layers for BFS respectively the number of consecutive child visits
 *         before siblings are are visited for DFS. 
 * @define RESULT Tuple of the node found and the set of all visited nodes.
 * @define ROOT the node to start the traversal from.
 * @define PRED the predicate which must hold true to stop traversing
 *         and return the node found. Defaults to `noNode` causing a full traversal.  
 */
trait GraphTraversal[N, E[X] <: EdgeLikeIn[X]] extends GraphBase[N,E]
{
  import GraphTraversal.VisitorReturn._
  import GraphTraversal._

  /** Whether `this` graph is connected if it is undirected or
   *  weakly connected if it is directed.
   */
  def isConnected = nodes.headOption map { head =>
    var cnt = 0
    head.traverseNodes(direction = AnyConnected, breadthFirst = false) { n =>
      cnt += 1
      Continue
    }
    cnt == nodes.size
  } getOrElse true
  /**
   * List of sets of nodes with each set containing connected nodes $INTOACC.
   * If `this` graph is connected, the list has one element containing all nodes
   * otherwise more than one elements.   
   * 
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param nodeVisitor $NODEVISITOR
   * @param edgeVisitor $EDGEVISITOR
   *//*
  def components (nodeFilter : (NodeT) => Boolean       = anyNode,
                  edgeFilter : (EdgeT) => Boolean       = anyEdge,
                  nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                  edgeVisitor: (EdgeT) => Unit          = noEdgeAction): List[Set[NodeT]]*/
  /** Same as `components(...)` with default arguments. *//*
  @inline final def components: List[Set[NodeT]] = components()*/
  /**
   * Whether `this` graph has at least one cycle.
   */
  @inline final def isCyclic: Boolean = findCycle isDefined
  /**
   * Whether `this` graph has no cycle.
   */
  @inline final def isAcyclic: Boolean = ! isCyclic
  /**
   * Finds a cycle in `this` graph $INTOACC, if any.
   * 
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param maxDepth   $MAXDEPTH
   * @param nodeVisitor $NODEVISITOR
   * @param edgeVisitor $EDGEVISITOR
   * @return A cycle or None if either
   *         a) there exists no cycle in `this` graph or
   *         b) there exists a cycle in `this` graph but due to the given
   *            filtering conditions or a `Cancel` return by a visitor this cycle
   *            had to be disregarded.
   */
  def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                edgeFilter : (EdgeT) => Boolean       = anyEdge,
                maxDepth   :  Int                     = 0,
                nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                edgeVisitor: (EdgeT) => Unit          = noEdgeAction): Option[Cycle]
  /** Same as `findCycle(...)` with default arguments. */
  @inline final def findCycle: Option[Cycle] = findCycle()
  /**
   * Represents a path in this graph listing the nodes and connecting edges on it
   * with the following syntax:
   * 
   * `path ::= ''node'' { ''edge'' ''node'' }`
   * 
   * All nodes and edges on the path are distinct. A path contains at least
   * one node followed by any number of consecutive pairs of an edge and a node.
   * The first element is the start node, the second is an edge with its tail
   * being the start node and its head being the third element etc.   
   */
  trait Path extends Iterable[GraphParamOut[N,E]]
  {
    override def stringPrefix = "Path"
    /**
     * Iterator over the nodes of this path. The result is chached
     * on the first call, so consecutive calls of this method are cheep.
     *  
     * @return Iterator over all nodes of this path in proper order.
     */
    def nodeIterator = (iterator filter (_.isNode)).asInstanceOf[Iterator[NodeT]]
    /**
     * Iterator over the edges of this path. The result is chached
     * on the first call, so consecutive calls of this method are cheep.
     *  
     * @return Iterator over all edges of this path in proper order.
     */
    def edgeIterator = (iterator filter (_.isEdge)).asInstanceOf[Iterator[EdgeT]]
    /** List containing all nodes of this path in proper order. */
    def nodes = nodeIterator.toList
    /** List containing all edges of this path in proper order. */
    def edges = edgeIterator.toList
    /** The cumulated weight of all edges on this path. */
    def weight = { var sum = 0L; edgeIterator foreach {sum += _.weight}; sum }  
    /** The number of edges on this path. */
    def length = edgeIterator.size
    def startNode: NodeT
    def endNode:   NodeT
    /**
     * Returns whether the contents of this path are valid with respect
     * to path semantics. This check is appropriate whenever there may be
     * any doubt about the correctness of the result of an algorithm. 
     */
    def isValid: Boolean
  }
  /**
   * Whether all nodes are pairwise adjacent.
   * 
   * @return `true` if this graph is complete, `false` if this graph contains any
   * independent nodes. 
   */
  /**
   * Represents a cycle in this graph listing the nodes and connecting edges on it
   * with the following syntax:
   * 
   * `cycle ::= ''start-end-node'' { ''edge'' ''node'' } ''edge'' ''start-end-node''`
   * 
   * All nodes and edges on the path are distinct except the start and end nodes that
   * are equal. A cycle contains at least a start node followed by any number of
   * consecutive pairs of an edge and a node and the end node equaling to the start node.
   * The first element is the start node, the second is an edge with its tail
   * being the start node and its head being the third element etc.   
   */
  trait Cycle extends Path {
    override def stringPrefix = "Cycle"
    /**
     * Semantically compares `this` cycle with `that` cycle. While `==` returns `true`
     * only if the cycles contain the same elements in the same order, this comparison
     * returns also `true` if the elements of `that` cycle can be shifted and optionally
     * reversed such that their elements have the same order.
     * 
     * For instance, given
     * 
     * `c1 = Cycle(1-2-3-1)`, `c2 = Cycle(2-3-1-2)` and `c3 = Cycle(2-1-3-2)`
     * 
     * the following expressions hold:
     * 
     * `c1 != c2`, `c1 != c3` but `c1 sameAs c2` and `c1 sameAs c3`.  
     */
    def sameAs(that: GraphTraversal[N,E]#Cycle): Boolean
  }

  def isComplete = {
    val orderLessOne = order - 1
    nodes forall (_.diSuccessors.size == orderLessOne)
  }
  /** Default node filter letting through all nodes. */
  @inline final def  anyNode = (n: NodeT) => true
  /** Node predicate always returning `false`. */
  @inline final def  noNode = (n: NodeT) => false
  /** Default edge filter letting through all edges. */
  @inline final def  anyEdge = (e: EdgeT) => true
  //@inline final def  noNodeSort   = (n1: NodeT, n2: NodeT) => true
  //@inline final def  noEdgeSort   = (e1: EdgeT, e2: EdgeT) => true
  /** Default node visitor doing nothing. */
  @inline final def  noNodeAction = (n: NodeT) => Continue
  /** Default edge visitor doing nothing. */
  @inline final def  noEdgeAction = (e: EdgeT) => {}
  /** Returns true if `filter` is not equivalent to `anyNode`. */ 
  @inline final def isCustomNodeFilter(filter: (NodeT) => Boolean)  = filter ne anyNode   
  /** Returns true if `filter` is not equivalent to `anyEdge`. */ 
  @inline final def isCustomEdgeFilter(filter: (EdgeT) => Boolean)  = filter ne anyEdge   
//  @inline final def isCustomNodeSort    = nodeSort ne NoNodeSort   
//  @inline final def isCustomEdgeSort    = nodeSort ne NoEdgeSort   
  /** Returns true if `visitor` is not equivalent to `noNodeAction`. */ 
  @inline final def isCustomNodeVisitor(visitor: (NodeT) => VisitorReturn) = visitor ne noNodeAction   
  /** Returns true if `visitor` is not equivalent to `noEdgeAction`. */ 
  @inline final def isCustomEdgeVisitor(visitor: (EdgeT) => Unit         ) = visitor ne noEdgeAction   
//  /**
//   * Contains methods that return more context information on the current
//   * traversal and may be called by a traversal visitor. 
//   */
//  trait NavContext {
//    def depth: Int
//    def path: Path
//    def nrOfVisitedNodes: Int
//    def visitedNodes: Set[NodeT]
//  }
  type NodeT <: InnerNodeLike
  trait InnerNodeLike extends super.InnerNodeLike
  {
    /**
     * Finds a successor of this node for which the predicate `pred` holds $INTOACC.
     * $VISITORS
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several successors exist the algorithm selects the first of them it founds.
     * 
     * @param pred The predicate which must hold true for the resulting node.
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return A node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     *         c) there exists a path to such a node but due to
     *            user filtering or canceling the traversal this path had to be disregarded.
     */
    def findSuccessor(pred: (NodeT) => Boolean,
                      nodeFilter : (NodeT) => Boolean        = anyNode,
                      edgeFilter : (EdgeT) => Boolean        = anyEdge,
                      nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                      edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[NodeT]
    /**
     * Checks whether `potentialSuccessor` is a successor of this node $INTOACC.
     * $VISITORS
     * Same as `isPredecessorOf`. 
     *
     * @param potentialSuccessor The node which is potentially a successor of this node. 
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return `true` if a path exists from this node to `potentialSuccessor` and
     *         it had not to be excluded due to user filtering or canceling the traversal.
     */
    @inline final
    def hasSuccessor(potentialSuccessor: NodeT,
                     nodeFilter : (NodeT) => Boolean        = anyNode,
                     edgeFilter : (EdgeT) => Boolean        = anyEdge,
                     nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                     edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Boolean =
      findSuccessor(_ eq potentialSuccessor,
                    nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).isDefined
    /** Same as `hasSuccessor`. */
    @inline final
    def isPredecessorOf(potentialSuccessor: NodeT,
                        nodeFilter : (NodeT) => Boolean        = anyNode,
                        edgeFilter : (EdgeT) => Boolean        = anyEdge,
                        nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                        edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Boolean =
      hasSuccessor(potentialSuccessor,
                   nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
    /**
     * Finds a predecessor of this node for which the predicate `pred` holds $INTOACC.
     * $VISITORS
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several predecessors exist the algorithm selects the first of them found.
     * 
     * @param pred The predicate which must hold true for the resulting node.
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return A node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path from such a node to this node at all or
     *         c) there exists a path from such a node to this node but due to
     *            user filtering or canceling the traversal this path had to be disregarded.
     */
    def findPredecessor(pred: (NodeT) => Boolean,
                        nodeFilter : (NodeT) => Boolean        = anyNode,
                        edgeFilter : (EdgeT) => Boolean        = anyEdge,
                        nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                        edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[NodeT]
    /**
     * Checks whether `potentialPredecessor` is a predecessor of this node $INTOACC.
     * $VISITORS
     * Same as `isSuccessorOf`. 
     *
     * @param potentialPredecessor The node which is potentially a predecessor of this node. 
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return `true` if a path exists from `potentialPredecessor` to this node and
     *         it had not to be excluded due to user filtering or canceling the traversal.
     */
    @inline final
    def hasPredecessor(potentialPredecessor: NodeT,
                       nodeFilter : (NodeT) => Boolean        = anyNode,
                       edgeFilter : (EdgeT) => Boolean        = anyEdge,
                       nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                       edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Boolean =
      findPredecessor(_ eq potentialPredecessor,
                      nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).isDefined
    /** Same as `hasPredecessor`. */
    @inline final
    def isSuccessorOf(potentialPredecessor: NodeT,
                      nodeFilter : (NodeT) => Boolean        = anyNode,
                      edgeFilter : (EdgeT) => Boolean        = anyEdge,
                      nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                      edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Boolean =
      hasPredecessor(potentialPredecessor,
                     nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
    /**
     * Finds a node (not necessarily directly) connected with this node
     * for which the predicate `pred` holds $INTOACC.
     * For directed or mixed graphs the node to be found is weekly connected with this node.
     * $VISITORS
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several connected nodes exist with `pred` the algorithm selects the first
     * of them it founds.
     * 
     * @param pred The predicate which must hold true for the resulting node.
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return A node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no connection to such a node at all
     *         c) there exists a connection to such a node but due to
     *            user filtering or canceling the traversal this connection had to be disregarded.
     */
    def findConnected(pred: (NodeT) => Boolean,
                      nodeFilter : (NodeT) => Boolean        = anyNode,
                      edgeFilter : (EdgeT) => Boolean        = anyEdge,
                      nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                      edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[NodeT]
    /**
     * Checks whether `potentialConnected` is a node (not necessarily directly)
     * connected with this node $INTOACC.
     * For directed or mixed graphs it is satisfactory that `potentialConnected` is
     * weekly connected with this node.
     * $VISITORS
     *
     * @param potentialConnected The node which is potentially connected with this node. 
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return `true` if a path exists from this node to `potentialConnected` and
     *         it had not to be excluded due to user filtering or canceling the traversal.
     */
    @inline final
    def isConnectedWith(potentialConnected: NodeT,
                        nodeFilter : (NodeT) => Boolean        = anyNode,
                        edgeFilter : (EdgeT) => Boolean        = anyEdge,
                        nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                        edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Boolean =
      findConnected(_ eq potentialConnected,
                    nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).isDefined
    /**
     * Finds a path from this node to a successor of this node for which the predicate
     * `pred` holds $INTOACC.
     *
     * This node itself does not count as a match. This is also true if it has a hook.
     * If several successors exist the algorithm selects any first matching node.
     * 
     * @param pred The predicate which must hold true for the successor. 
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return A path to a node with the predicate `pred` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     *         c) there exists a path to such a node but due to the given filter
     *            conditions this path had to be disregarded.
     */
    def pathUntil(pred: (NodeT) => Boolean,
                  nodeFilter : (NodeT) => Boolean        = anyNode,
                  edgeFilter : (EdgeT) => Boolean        = anyEdge,
                  nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                  edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[Path]
    /**
     * Finds a path from this node to `potentialSuccessor`.
     *
     * @param potentialSuccessor The node a path is to be found to.
     * @return A path to `potentialSuccessor` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     */
    @inline final
    def pathTo(potentialSuccessor: NodeT): Option[Path] =
      pathUntil(_ eq potentialSuccessor,
                anyNode, anyEdge, noNodeAction, noEdgeAction)
    /**
     * Finds a path from this node to `potentialSuccessor` $INTOACC.
     *
     * @param potentialSuccessor The node a path is to be found to.
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return A path to `potentialSuccessor` or None if either
     *         a) there is no node with `pred` or
     *         b) there exists no path to such a node at all
     *         c) there exists a path to such a node but due to the given filter
     *         conditions this path had to be disregarded.
     */
    @inline final
    def pathTo(potentialSuccessor: NodeT,
               nodeFilter : (NodeT) => Boolean        = anyNode,
               edgeFilter : (EdgeT) => Boolean        = anyEdge,
               nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
               edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[Path] =
      pathUntil(_ eq potentialSuccessor,
                nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
    /**
     * Finds the shortest path from this node to `potentialSuccessor`.
     * 
     * The calculation is based on the weight of the edges on the path. As a default,
     * edges have a weight of 1 what can be overridden by custom edges. 
     *
     * @param potentialSuccessor The node the shortest path is to be found to.
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return The shortest path to `potentialSuccessor` or None if either
     *         a) there exists no path to `potentialSuccessor` or
     *         b) there exists a path to `potentialSuccessor` but due to the given
     *            filtering conditions this path had to be disregarded.
     */
    def shortestPathTo(potentialSuccessor: NodeT,
                       nodeFilter : (NodeT) => Boolean        = anyNode,
                       edgeFilter : (EdgeT) => Boolean        = anyEdge,
                       nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                       edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[Path]
    /**
     * Finds a cycle starting the search at this node $INTOACC, if any.
     * The resulting cycle may start at any node connected with `this` node.
     * 
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param maxDepth   $MAXDEPTH
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     * @return A cycle or None if either
     *         a) there exists no cycle in the component `this` node belongs to or
     *         b) there exists a cycle in the component but due to the given
     *            filtering conditions or a `Cancel` return by a visitor this cycle
     *            had to be disregarded.
     */
    def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                  edgeFilter : (EdgeT) => Boolean       = anyEdge,
                  maxDepth   :  Int                     = 0,
                  nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                  edgeVisitor: (EdgeT) => Unit          = noEdgeAction): Option[Cycle]
    /** Same as `findCycle(...)` with default arguments. */
    @inline final def findCycle: Option[Cycle] = findCycle()
    /**
     * Traverses this graph from this (root) node for side-effects allowing
     * 
     * a) to filter nodes and/or edges,
     * b) to carry out any side effect at visited nodes and/or edges and
     * c) to cancel the traversal at any node. 
     * 
     * @param direction $DIRECTION
     * @param nodeFilter $NODEFILTER
     * @param edgeFilter $EDGEFILTER
     * @param breadthFirst $BREADTHFIRST
     * @param maxDepth     $MAXDEPTH
     * @param nodeVisitor $NODEVISITOR
     * @param edgeVisitor $EDGEVISITOR
     */
    def traverse (direction  : Direction          = Successors,
                  nodeFilter : (NodeT) => Boolean = anyNode,
                  edgeFilter : (EdgeT) => Boolean = anyEdge,
                  breadthFirst:Boolean            = true,
                  maxDepth   :  Int               = 0)
                 (nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                  edgeVisitor: (EdgeT) => Unit           = noEdgeAction)
    /**
     * Shortcut for calling 'traverse' with a non-default `nodeVisitor`
     * but the default `edgeVisitor` allowing a `foreach`-like call syntax:
     * {{{
     * rootNode traverseNodes() {
     *   print("d" + _.degree)
     *   Continue
     * }
     * }}} 
     */
    @inline final
    def traverseNodes(direction  : Direction          = Successors,
                      nodeFilter : (NodeT) => Boolean = anyNode,
                      edgeFilter : (EdgeT) => Boolean = anyEdge,
                      breadthFirst:Boolean            = true,
                      maxDepth   :  Int               = 0)
                     (nodeVisitor: (NodeT) => VisitorReturn) =
      traverse(direction, nodeFilter, edgeFilter, breadthFirst, maxDepth)(nodeVisitor = nodeVisitor)   
    /**
     * Shortcut for calling 'traverse' with a non-default `edgeVisitor`
     * but the default `nodeVisitor` allowing a `foreach`-like call syntax:
     * {{{
     * rootNode traverseEdges() {
     *   print( if(_.directed) "d" else "u" )
     *   Continue
     * }
     * }}} 
     */
    @inline final
    def traverseEdges(direction  : Direction          = Successors,
                      nodeFilter : (NodeT) => Boolean = anyNode,
                      edgeFilter : (EdgeT) => Boolean = anyEdge,
                      breadthFirst:Boolean            = true,
                      maxDepth   :  Int               = 0)
                     (edgeVisitor: (EdgeT) => Unit       ) =
      traverse(direction, nodeFilter, edgeFilter, breadthFirst, maxDepth)(edgeVisitor = edgeVisitor)   
  }
  /** Abstract class for functional traversals.
   * 
   * In addition to the `traverse` methods defined for nodes, this concept supports
   * repeated traversals with constant direction, filters and visitors.
   * Call `newTraversal` to create an instance and call any subsequent traversals
   * on that instance.     
   * 
   * @author Peter Empen
   */
  abstract class Traversal(direction  : Direction,
                           nodeFilter : (NodeT) => Boolean,
                           edgeFilter : (EdgeT) => Boolean,
                           nodeVisitor: (NodeT) => VisitorReturn,
                           edgeVisitor: (EdgeT) => Unit)
  {
    final val noAction   = (n: NodeT) => {}
    final val notVisited = (n: NodeT) => false

    /** Computes the filtered direct successors of `node`. 
     * It also calls `edgeVisitor` but does '''not''' call `nodeVisitor`.
     * 
     * @param node the node the direct successors are to be calculated of
     * @param isVisited function returning whether a node has already been
     *        visited and must therefore be excluded
     */
    protected[collection]
    def filteredDiSuccessors(node     :  NodeT,
                             isVisited: (NodeT) => Boolean): Iterable[NodeT]
    /** Computes the filtered direct predecessors of `node`. 
     * It also calls `edgeVisitor` but does '''not''' call `nodeVisitor`.
     * 
     * @param node the node the direct predecessors are to be calculated of
     * @param isVisited function returning whether a node has already been
     *        visited and must therefore be excluded
     */
    protected[collection]
    def filteredDiPredecessors(node     :  NodeT,
                               isVisited: (NodeT) => Boolean): Iterable[NodeT]
    /** Computes the filtered neighbors of `node`. 
     * It also calls `edgeVisitor` but does '''not''' call `nodeVisitor`.
     * 
     * @param node the node the adjacent are to be calculated of
     * @param isVisited function returning whether a node has already been
     *        visited and must therefore be excluded
     */
    protected[collection]
    def filteredNeighbors(node     :  NodeT,
                          isVisited: (NodeT) => Boolean): Iterable[NodeT]

    /**
     * Traversal return value.
     * 
     * @param found the node found if a search was required 
     * @param visited the set of all traversed nodes (As an exception, third-party
     *        implementations may return an empty set instead to minimize computational
     *        overhead in which case this must be indicated in the documentation.) 
     */
    protected[collection]
    case class Result(val found: Option[NodeT],
                      val visited: Iterable[NodeT])
    /**
     * Traverses this graph from `root` for side-effects allowing
     * 
     * a) to filter nodes and/or edges,
     * b) to carry out any side effect at visited nodes and/or edges and
     * c) to cancel the traversal at any node. 
     * 
     * @param root         $ROOT
     * @param pred         $PRED
     * @param breadthFirst $BREADTHFIRST
     * @param maxDepth     $MAXDEPTH
     * @return $RESULT 
     */
    def apply(root        : NodeT,
              pred        : (NodeT) => Boolean = noNode,
              breadthFirst: Boolean            = true,
              maxDepth    : Int                = 0): Result
    /**
     * Starting at `root`, functionally traverses this graph up to `maxDepth` layers
     * using the depth first search algorithm and all filters, visitors etc.
     * passed to the encapsulating `Traversal` instance.
     * 
     * @param root $ROOT
     * @param pred $PRED
     * @return $RESULT 
     */
    def depthFirstSearch (root      :  NodeT,
                          pred      : (NodeT) => Boolean = noNode,
                          onPopFound: (NodeT) => Unit    = noAction): Result
    /** Synonym for `depthFirstSearch` */
    @inline final def dfs(root      :  NodeT,
                          pred      : (NodeT) => Boolean = noNode,
                          onPopFound: (NodeT) => Unit    = noAction) =
      depthFirstSearch(root, pred, onPopFound)
    /**
     * Starting at `root`, functionally traverses this graph up to `maxDepth` layers
     * using the breadth first search algorithm and all filters, visitors etc.
     * passed to the encapsulating `Traversal` instance.
     * 
     * @param root $ROOT
     * @param pred $PRED
     * @return $RESULT 
     */
    def breadthFirstSearch(root    : NodeT,
                           pred    : (NodeT) => Boolean = noNode,
                           maxDepth: Int                = 0): Result
    /** Synonym for `breadthFirstSearch` */ 
    @inline final def bfs(root    : NodeT,
                          pred    : (NodeT) => Boolean = noNode,
                          maxDepth: Int                = 0) =
      breadthFirstSearch(root, pred, maxDepth)
  }
  /**
   * Creates a `Traversal` instance allowing subsequent traversals with
   * constant filters and visitors.
   * 
   * @param direction $DIRECTION
   * @param nodeFilter $NODEFILTER
   * @param edgeFilter $EDGEFILTER
   * @param nodeVisitor $NODEVISITOR
   * @param edgeVisitor $EDGEVISITOR
   */
  def newTraversal(direction  : Direction                 = Successors,
                   nodeFilter : (NodeT) => Boolean        = anyNode,
                   edgeFilter : (EdgeT) => Boolean        = anyEdge,
                   nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                   edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Traversal
}
object GraphTraversal {
  object VisitorReturn extends Enumeration {
    type VisitorReturn = Value
    val Continue, Cancel = Value
  }
  sealed trait Direction  
  object Successors   extends Direction 
  object Predecessors extends Direction
  object AnyConnected extends Direction
}
