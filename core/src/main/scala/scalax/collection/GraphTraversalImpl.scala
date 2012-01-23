package scalax.collection

import annotation.tailrec
import collection.mutable.{ListBuffer, Set => MutableSet, Map => MutableMap,
                           Queue, PriorityQueue, Stack}

import GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn, GraphParamOut,
                    NodeIn, NodeOut, EdgeIn, EdgeOut}
import GraphEdge.{EdgeLike}

trait GraphTraversalImpl[N, E[X] <: EdgeLikeIn[X]] extends GraphTraversal[N,E]
{
  import GraphTraversal.VisitorReturn._
  import GraphTraversal._
  /*
  override def components(nodeFilter : (NodeT) => Boolean       = anyNode,
                          edgeFilter : (EdgeT) => Boolean       = anyEdge,
                          nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                          edgeVisitor: (EdgeT) => Unit          = noEdgeAction) =
    if (order == 0) List.empty[Set[NodeT]]
    else {
      val all = nodes filter (nodeFilter(_))
      val collected = MutableSet.empty[NodeT]
      val traversal = new Traversal(
          AnyConnected, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
      var next = nodes.head 
      while (! (collected contains next))
        traversal.depthFirstSearch(next).found.isDefined)
      ...
    }
  */
  override def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                         edgeFilter : (EdgeT) => Boolean       = anyEdge,
                         maxDepth   :  Int                     = 0,
                         nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                         edgeVisitor: (EdgeT) => Unit          = noEdgeAction): Option[Cycle] =
    if (order == 0) None
    else {
      val path = CycleBuffer(nodeFilter, edgeFilter)
      val traversal =
        new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
      val visited = MutableSet.empty[NodeT]
      for (node <- nodes if ! visited(node)) {
        val res = traversal.depthFirstSearchWGB(
                    node,
                    onPopFound = (n: NodeT) => {
                      if (path.isEmpty) path. +=: (n)
                      else              path. +=: (n, path.firstEdge _) 
                    }) 
        if (res.found.isDefined)
          return Some(path)
        else
          visited ++= res.visited
      }
      None
    }
  trait InnerNodeTraversalImpl extends super.InnerNodeLike
  { this: NodeT =>

    override def findSuccessor(pred: (NodeT) => Boolean,
                               nodeFilter : (NodeT) => Boolean        = anyNode,
                               edgeFilter : (EdgeT) => Boolean        = anyEdge,
                               nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                               edgeVisitor: (EdgeT) => Unit           = noEdgeAction) =
    {
      new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).
          depthFirstSearch(this, pred).found
    }
    override def findPredecessor(pred: (NodeT) => Boolean,
                                 nodeFilter : (NodeT) => Boolean        = anyNode,
                                 edgeFilter : (EdgeT) => Boolean        = anyEdge,
                                 nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                                 edgeVisitor: (EdgeT) => Unit           = noEdgeAction) =
    {
      new Traversal(Predecessors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).
          depthFirstSearch(this, pred).found
    }
    override def findConnected(pred: (NodeT) => Boolean,
                               nodeFilter : (NodeT) => Boolean        = anyNode,
                               edgeFilter : (EdgeT) => Boolean        = anyEdge,
                               nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                               edgeVisitor: (EdgeT) => Unit           = noEdgeAction) =
    {
      new Traversal(AnyConnected, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).
          depthFirstSearch(this, pred).found
    }
    override def pathUntil(pred: (NodeT) => Boolean,
                           nodeFilter : (NodeT) => Boolean        = anyNode,
                           edgeFilter : (EdgeT) => Boolean        = anyEdge,
                           nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                           edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[Path] = {
      val path = PathBuffer(nodeFilter, edgeFilter)
      if (pred(this))
        Some(path. +=: (this))
      else
        if (new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).
            depthFirstSearch(this,
                             pred,
                             onPopFound = (n: NodeT) => {
                               if (path.isEmpty) path. +=: (n)
                               else              path. +=: (n, path.firstEdge _) 
                             }).found.isDefined)
          Some(path.+=:(this, path.firstEdge _))
        else
          None
    }
    override def shortestPathTo(to: NodeT,
                                nodeFilter : (NodeT) => Boolean        = anyNode,
                                edgeFilter : (EdgeT) => Boolean        = anyEdge,
                                nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                                edgeVisitor: (EdgeT) => Unit           = noEdgeAction): Option[Path] =
    {
      type NodeWeight    = Tuple2[NodeT,Long]
      lazy val visited   = MutableSet[NodeT]()
      def isVisited      = (n: NodeT) => visited(n)
      lazy val dest      = MutableMap[NodeT,Long](this->0L)
      lazy val mapToPred = MutableMap[NodeT,NodeT]()
      lazy val traversal = new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor) 
      lazy val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
      // not implicit - see ticket #4405 and #4407
      object ordNodeWeight extends Ordering[NodeWeight] {
        def compare(x: NodeWeight,
                    y: NodeWeight) = y._2.compare(x._2)
      }
      val qNodes = new PriorityQueue[NodeWeight]()(ordNodeWeight) += ((this->0L))

      def sortedAdjacentsNodes(node: NodeT): Option[PriorityQueue[NodeWeight]] = 
        traversal.filteredDiSuccessors(node, isVisited) match {
          case adj if adj.nonEmpty =>
            Some(adj.
                 foldLeft(new PriorityQueue[NodeWeight]()(ordNodeWeight))(
                   (q,n) => q += ((n, node.outgoingTo(n).filter(edgeFilter(_)).
                                      min(Edge.WeightOrdering).weight))))
          case _ => None
        }
      def relax(pred: NodeT, succ: NodeT) {
        val cost = dest(pred) + pred.outgoingTo(succ).filter(edgeFilter(_)).
                                min(Edge.WeightOrdering).weight
        if(!dest.isDefinedAt(succ) || cost < dest(succ)) {
          dest      += (succ->cost)
          mapToPred += (succ->pred)
        }
      }
      var canceled = false
      @tailrec
      def rec(pq: PriorityQueue[NodeWeight]) {
        if(pq.nonEmpty && (pq.head._1 ne to)) { 
          val nodeWeight = pq.dequeue
          val node = nodeWeight._1 
          sortedAdjacentsNodes(node) match {
            case Some(ordNodes) =>
              if (ordNodes.nonEmpty) pq ++=(ordNodes)
              @tailrec
              def loop(pq2: PriorityQueue[NodeWeight]) {
                if(pq2.nonEmpty) {
                  relax(node, pq2.dequeue._1)
                  loop(pq2)
                }
              }
              loop(ordNodes)
            case None =>
          }
          visited += node
          if (doNodeVisitor) 
            if (nodeVisitor(node) == Cancel) {
              canceled = true
              return
            }
          rec(pq) 
        }
      }
      rec(qNodes) 
      def traverseMapNodes(map: MutableMap[NodeT,NodeT]): Option[Path] = {
        val path = PathBuffer(nodeFilter, edgeFilter).+=:(to)
        if(map.isDefinedAt(to)) {
          @tailrec
          def loop(k: NodeT) {
            path.+=:(k, path.minWeightEdge _)
            if(map.isDefinedAt(k))
              loop(map.get(k).get) 
          }
          loop(map.get(to).get);
          Some(path)
        } else if(this eq to) Some(path) else None
      }
      if (canceled) None
      else traverseMapNodes(mapToPred)
    }
    override def findCycle(nodeFilter : (NodeT) => Boolean       = anyNode,
                           edgeFilter : (EdgeT) => Boolean       = anyEdge,
                           maxDepth   :  Int                     = 0,
                           nodeVisitor: (NodeT) => VisitorReturn = noNodeAction,
                           edgeVisitor: (EdgeT) => Unit          = noEdgeAction) =
    {
      val path = CycleBuffer(nodeFilter, edgeFilter)
      if (new Traversal(Successors, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor).
          depthFirstSearchWGB (this,
                               onPopFound = (n: NodeT) => {
                                 if (path.isEmpty) path. +=: (n)
                                 else              path. +=: (n, path.firstEdge _) 
                               }).found.isDefined)
        Some(path)
      else
        None
    }
    final override
    def traverse (direction  : Direction          = Successors,
                  nodeFilter : (NodeT) => Boolean = anyNode,
                  edgeFilter : (EdgeT) => Boolean = anyEdge,
                  breadthFirst:Boolean            = true,
                  maxDepth   :  Int               = 0)
                 (nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                  edgeVisitor: (EdgeT) => Unit           = noEdgeAction)
    {
      new Traversal(direction, nodeFilter,  edgeFilter, nodeVisitor, edgeVisitor).
        apply(this, noNode, breadthFirst, maxDepth)
    }
  }
  class Traversal(direction  : Direction,
                  nodeFilter : (NodeT) => Boolean,
                  edgeFilter : (EdgeT) => Boolean,
                  nodeVisitor: (NodeT) => VisitorReturn,
                  edgeVisitor: (EdgeT) => Unit)
    extends super.Traversal(direction, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
  {
    final val addMethod = direction match {
      case Successors   => Node.addDiSuccessors _
      case Predecessors => Node.addDiPredecessors _
      case AnyConnected => Node.addNeighbors _
    }
    final val addFilteredMethod = direction match {
      case Successors   => filteredDiSuccessors _
      case Predecessors => filteredDiPredecessors _
      case AnyConnected => filteredNeighbors _
    }
    final val edgesMethod = direction match {
      case Successors   => (n: NodeT) => n.outgoing
      case Predecessors => (n: NodeT) => n.incoming
      case AnyConnected => (n: NodeT) => n.edges
    }
    final val doNodeFilter = isCustomNodeFilter(nodeFilter)
    final val doEdgeFilter = isCustomEdgeFilter(edgeFilter)
    final val doFilter = doNodeFilter || doEdgeFilter
    final val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
    final val doEdgeVisitor = isCustomEdgeVisitor(edgeVisitor)

    protected[collection]
    def filteredDi(direction: Direction,
                   node           :  NodeT,
                   isVisited      : (NodeT) => Boolean): Iterable[NodeT] =
    {
      var succ = MutableSet[NodeT]()
      if (doFilter) {
        def addFilteredNeighbors(edge: EdgeT) {
          addMethod(node, edge,
                   (n: NodeT) => if (nodeFilter(n) && ! isVisited(n))
                                 succ += n)
          if (doEdgeVisitor) edgeVisitor(edge)
        }
        if (doEdgeFilter)
          edgesMethod(node) foreach { e => if (edgeFilter(e)) addFilteredNeighbors(e) }
        else
          edgesMethod(node) foreach { addFilteredNeighbors(_) }
        succ
      } else {
        edgesMethod(node) foreach { (e: EdgeT) =>
          addMethod(node, e,
                    (n: NodeT) => if (! isVisited(n)) succ += n)
          if (doEdgeVisitor) edgeVisitor(e)
        }
      }
      succ
    }
    @inline final override protected[collection]
    def filteredDiSuccessors(node     :  NodeT,
                             isVisited: (NodeT) => Boolean): Iterable[NodeT] =
      filteredDi(Successors, node, isVisited)
    @inline final override protected[collection]
    def filteredDiPredecessors(node     :  NodeT,
                               isVisited: (NodeT) => Boolean): Iterable[NodeT] =
      filteredDi(Predecessors, node, isVisited)
    @inline final override protected[collection]
    def filteredNeighbors(node     :  NodeT,
                          isVisited: (NodeT) => Boolean): Iterable[NodeT] =
      filteredDi(AnyConnected, node, isVisited)

    override def apply(root        : NodeT,
                       pred        : (NodeT) => Boolean = noNode,
                       breadthFirst: Boolean            = true,
                       maxDepth    : Int                = 0): Result =
    {
      if (breadthFirst)
        breadthFirstSearch(root, pred, maxDepth)
      else {
        require(maxDepth <= 0, "Sorry, dfs with 'maxDepth > 0' is not yet implemented.")
        depthFirstSearch(root, pred)
      }
    }
    override def depthFirstSearch(root      :  NodeT,
                                  pred      : (NodeT) => Boolean = noNode,
                                  onPopFound: (NodeT) => Unit    = noAction): Result =
    {
      val stack = Stack(root)
      val visited = MutableMap(root -> root) // visited -> predecessor
      def isVisited (node: NodeT) = visited get node isDefined
      var res: Option[NodeT] = None
      @tailrec def loop {
        if(stack.nonEmpty) {
          val current = stack.pop
          if (doNodeVisitor && nodeVisitor(current) == Cancel)
              return
          if (pred(current) && (current ne root)) {
            res = Some(current)
          } else {
            for (n <- addFilteredMethod(current, isVisited) filterNot (isVisited(_))) {
              stack.push(n)
              visited += (n -> current)
            }
            loop
          }
        }
      }
      loop
      if (res.isDefined) {
        if (onPopFound ne noAction) {
          var node = res.get
          do { onPopFound(node)
               node = visited(node) // prev
          } while(node ne root)
        }
      }
      Result(res, visited.keySet)
    }
    /**
     * Tail-recursive DFS implementation for cycle detection based on the idea of
     * the white-gray-black algorithm.
     * 
     * @param root start node for the search
     * @param predicate node predicate marking an end condition for the search
     * @param onPopFound action to be carried out for every node beginning at the
     *        node found by the search and ending with `root`; this parameter is
     *        primarily used to build a path after a successful search. 
     */
    def depthFirstSearchWGB(root      :  NodeT,
                            predicate : (NodeT) => Boolean = noNode,
                            onPopFound: (NodeT) => Unit    = noAction): Result =
    {
      val stack = Stack(root)
      val path = Stack.empty[NodeT]
      val visited = MutableMap.empty[NodeT, Boolean] // visited -> gray/black

      def isWhite (node: NodeT) = nonVisited(node)
      def isGray  (node: NodeT) = visited get node map (!_) getOrElse false
      def isBlack (node: NodeT) = visited get node getOrElse false
      def setGray (node: NodeT) = visited += node -> false
      def setBlack(node: NodeT) = visited update (node, true) 

      def onNodeDown(node: NodeT) { setGray (node) } 
      def onNodeUp  (node: NodeT) { setBlack(node) }

      def isVisited (node: NodeT) = visited get node isDefined
      def nonVisited(node: NodeT) = visited get node isEmpty
      var res: Option[NodeT] = None
      /* pushed allows to track the path.
       * prev   serves the special handling of undirected edges. */
      @tailrec
      def loop(pushed: Boolean, prev: Option[NodeT]) {
        def checkNodeUp(current: NodeT) {
          if (! pushed)
            while (path.nonEmpty && (path.head ne current)) {
              val p = path.pop
              if (! isBlack(p) && (p ne root))
                onNodeUp(p) 
            }
        }
        if (res.isEmpty)
          if (stack.isEmpty)
            path foreach (setBlack(_))
          else {
            val current = stack.pop
            checkNodeUp(prev getOrElse root)
            var newPrev = prev 
            path.push(current)
            if (nonVisited(current)) onNodeDown(current)
            if (doNodeVisitor && nodeVisitor(current) == Cancel) return
            if (predicate(current) && (current ne root))
              res = Some(current)
            else {
              var pushed = false
              for (n <- addFilteredMethod(current, isBlack(_)) filterNot (isBlack(_))) { 
                if (isGray(n)) {
                  if (prev map (_ ne n) getOrElse true)
                    res = Some(n)
                } else {
                  stack.push(n)
                  pushed = true
                  newPrev = if (current.outgoingTo(n) exists (!_.directed)) Some(current)
                            else None
                }
              }
            loop(pushed, newPrev)
            }
          }
      }
      loop(true, None)

      if (res.isDefined) {
        if (onPopFound ne noAction) {
          val resNode = res.get
          onPopFound(resNode)
          var continue = true
          while(continue && path.nonEmpty) {
            val n = path.pop
            onPopFound(n)
            if (n eq resNode) continue = false
          }
        }
      }
      Result(res, visited.keySet)
    }
    override def breadthFirstSearch(root    : NodeT,
                                    pred    : (NodeT) => Boolean = noNode,
                                    maxDepth: Int                = 0): Result =
    {
      val untilDepth = if (maxDepth > 0) maxDepth else java.lang.Integer.MAX_VALUE
      var depth = 0
      val doNodeVisitor = isCustomNodeVisitor(nodeVisitor)
      val visited = MutableMap.empty[NodeT, Int]
      @inline def isVisited(node: NodeT) = visited get node isDefined
      @inline def visitAndCanceled(n: NodeT) = {
        visited += (n -> depth)
        doNodeVisitor && nodeVisitor(n) == Cancel
      }
      if (visitAndCanceled(root)) return Result(None, visited.keySet)
      val q = Queue[(NodeT, Int)](root -> depth)
      while (q.nonEmpty) { 
        val (prevNode, prevDepth) = q.dequeue
        if (prevDepth < untilDepth) {
          depth = prevDepth + 1
          for (node <- addFilteredMethod(prevNode, isVisited)) { 
            if (visitAndCanceled(node)) return Result(None, visited.keySet)
            if (pred(node)) return Result(Some(node), visited.keySet)
            q enqueue (node -> depth)  
          }
        }
      }
      Result(None, visited.keySet)
    }
  }
  override def newTraversal(direction  : Direction                 = Successors,
                            nodeFilter : (NodeT) => Boolean        = anyNode,
                            edgeFilter : (EdgeT) => Boolean        = anyEdge,
                            nodeVisitor: (NodeT) => VisitorReturn  = noNodeAction,
                            edgeVisitor: (EdgeT) => Unit           = noEdgeAction) =    
    new Traversal(direction, nodeFilter, edgeFilter, nodeVisitor, edgeVisitor)
  /**
   * Serves as a buffer to be populated by graph algorithms yielding a `Path`.
   * The provided functionality is meant to help graph algorithm
   * developers by providing abstractions over ListBuffer.  
   * 
   * @author Peter Empen
   */
  class PathBuffer(val nodeFilter : (NodeT) => Boolean = anyNode,
                   val edgeFilter : (EdgeT) => Boolean = anyEdge)
    extends Path
  {
    val buf = ListBuffer[GraphParamOut[N,E]]()
    def iterator = buf.iterator
    def startNode = buf.head.asInstanceOf[NodeT]
    def endNode   = buf.tail.asInstanceOf[NodeT]
    /** Prepends a node to this buffer. The caller is responsible for correct order. */
    @inline final def +=: (node: NodeT) = { buf.+=:(node); this }
    /** Prepends an edge to this buffer. The caller is responsible for correct order. */
    @inline final def +=: (edge: EdgeT) = { buf.+=:(edge); this }
    /** Prepends a (node, edge) pair to this buffer. */
    def +=: (node: NodeT, edge: EdgeT): this.type = {
      buf.+=:(edge); buf.+=:(node); this
    }
    /**
     * The first edge found having its tail at `from` and its head at `to`
     * and satisfying `navigation.edgeFilter`.
     * The caller must guarantee that there is at least one connecting edge
     * between `from` and `to` with `navigation.edgeFilter`.
     * This simplest select method can be passed to `+=: (node, selectEdge)`
     * whenever any of possibly several edges between the two nodes
     * satisfies the needs of the algorithm.
     */
    def firstEdge (from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        from outgoingTo to filter edgeFilter head
      else
        from findOutgoingTo to get
    /**
     * The edge with minimal weight having its tail at `from` and its head at `to`
     * and satisfying `navigation.edgeFilter`.
     * The caller must guarantee that there is at least one connecting edge
     * between `from` and `to` with `navigation.edgeFilter`.
     * This select method can be passed to `+=: (node, selectEdge)`
     * whenever the edge with the minimal weight between the two nodes
     * should be selected.
     */
    def minWeightEdge (from: NodeT, to: NodeT): EdgeT =
      if (isCustomEdgeFilter(edgeFilter))
        from outgoingTo to filter edgeFilter min(Edge.WeightOrdering)
      else
        from outgoingTo to min(Edge.WeightOrdering)
    /**
     * Prepends a pair of (node, edge) to this buffer. The edge is selected
     * calling `selectEdge` with the arguments `node` and `startNode` of
     * the correct buffer. 
     */
    def +=: (node      : NodeT,
             selectEdge: (NodeT, NodeT) => EdgeT): this.type =
      this. +=: (node, selectEdge(node, startNode))
    def +=: (node      : NodeT,
             selectEdge: (NodeT, NodeT, (EdgeT) => Boolean) => EdgeT): this.type =
      this. +=: (node, selectEdge(node, startNode, edgeFilter))
    def isValid = {
      var mustBeNode = false
      var size = 0
      assert(
        iterator forall {elm =>
          mustBeNode = ! mustBeNode
          size += 1
          elm match { case _:Node => mustBeNode
                      case _:Edge => ! mustBeNode
                      case _      => false}} )
      size >= 1 && size % 2 == 1
    } 
    override def canEqual(that: Any) = that.isInstanceOf[GraphTraversalImpl[N,E]#PathBuffer]
    override def equals(other: Any) = other match {
      case that: GraphTraversalImpl[N,E]#PathBuffer => 
        (this eq that) ||
        (that canEqual this) &&
        (that.buf sameElements this.buf)
      case _ => false
    }
    override def hashCode = buf ##  
  }
  object PathBuffer
  {
    def apply(nodeFilter : (NodeT) => Boolean = anyNode,
              edgeFilter : (EdgeT) => Boolean = anyEdge) =
      new PathBuffer(nodeFilter, edgeFilter)
    /**
     * Enables to treat an instance of PathBuffer as if it was a ListBuffer. 
     * @param pathBuf The PathBuffer to convert.
     * @return The buffer contained in `pathBuf`
     */
    @inline final
    implicit def toBuffer(pathBuf: PathBuffer): ListBuffer[GraphParamOut[N,E]] = pathBuf.buf 
  }
  class CycleBuffer(nodeFilter : (NodeT) => Boolean = anyNode,
                    edgeFilter : (EdgeT) => Boolean = anyEdge)
    extends PathBuffer(nodeFilter, edgeFilter)
    with    Cycle
  {
    def sameAs(that: GraphTraversal[N,E]#Cycle) =
      this == that || ( that match {
        case that: GraphTraversalImpl[N,E]#CycleBuffer =>
          this.size == that.size && {
            val idx = this.buf.indexOf(that.head)
            if (idx > 0) {
              val thisDoubled = this.buf.toList ++ (this.buf.tail)
              (thisDoubled startsWith (that.buf        , idx)) ||
              (thisDoubled startsWith (that.buf.reverse, idx))
            }
            else false
          }
        case _ => false
      })
  }
  object CycleBuffer
  {
    def apply(nodeFilter : (NodeT) => Boolean = anyNode,
              edgeFilter : (EdgeT) => Boolean = anyEdge) =
      new CycleBuffer(nodeFilter, edgeFilter)
    /**
     * Enables to treat an instance of CycleBuffer as if it was a ListBuffer. 
     * @param cycleBuf The CycleBuffer to convert.
     * @return The buffer contained in `cycleBuf`
     */
    @inline final
    implicit def toBuffer(cycleBuf: CycleBuffer): ListBuffer[GraphParamOut[N,E]] = cycleBuf.buf 
  }
}
