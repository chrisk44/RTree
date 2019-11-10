import scala.collection.immutable.ListSet
import Array._

class Node(var D: Int, var parent: Node, var bucket: ListSet[Point] = new ListSet[Point]() ) {
  val MAX_BUCKET_SIZE = 5
  val MIN_NODES = 2
  val MAX_NODES = 4

  var range: Range = _
  var nodes: ListSet[Node] = new ListSet[Node]()

  if(isRoot) addNode(new Node(D, this))
  calculateRange()

  def addPoint(point: Point): Node = {
    if(isRoot) throw new Exception("Called addPoint on root")

    // Check if point exists
    if( bucket.exists( (p: Point) => p.equals(point) ) ) throw new Exception("Point already exists")

    // Add in bucket
    bucket += point
    if(bucket.size <= MAX_BUCKET_SIZE){
      calculateRange()
      return getRoot
    }

    // Split the node

    // Divide bucket points into two MBRs
    var bucket1 = new ListSet[Point]()
    var bucket2 = new ListSet[Point]()

    // === Split the points into 2 MBRs ===========================================

    // Find the worst pair
    var tmp_p1: Point = point
    var tmp_p2: Point = point
    var tmp_area = 0.0

    for( p1 <- bucket ){
      for( p2 <- bucket ){
        if(p1.toRange.combine(p2.toRange).space >= tmp_area){
          tmp_p1 = p1
          tmp_p2 = p2
          tmp_area = p1.toRange.combine(p2.toRange).space
        }
      }
    }

    // Add the 2 points in each list
    bucket1 += tmp_p1
    bucket -= tmp_p1

    bucket2 += tmp_p2
    bucket -= tmp_p2

    var area1: Double = 0.0
    var area2: Double = 0.0

    // Add each point to the best-fit mbr
    for ( p <- bucket ){
      area1 = tmp_p1.toRange.combine(p.toRange).space
      area2 = tmp_p2.toRange.combine(p.toRange).space

      if(area1 < area2){
        bucket1 += p
      }else{
        bucket2 += p
      }
    }

    // === End of split =============================================================

    bucket = bucket1
    calculateRange()

    parent.addNode(new Node(D, this, bucket2))

  }
  def delete(point: Point): Node = {
    // Make sure point exists
    if ( !bucket.exists( (p: Point) => p.equals(point) ) ) throw new Exception("Node doesn't have the point to delete")

    // Remove the requested point from our bucket
    bucket -= bucket.find( (p: Point) => p.equals(point) ).get

    var root = getRoot

    // Check if bucket is empty after deletion
    if(bucket.isEmpty){
      // Tell our parent to delete us, get any points to be reinserted
      val points = parent.deleteNode(this)

      // If root has only one child, we may need to change root
      if(root.nodes.size == 1){

        val first_child = root.nodes.iterator.next()

        // If first child is not leaf node, make it our root
        if(first_child.nodes.nonEmpty){
          first_child.parent = null
          root = first_child
        }

      }else if(root.nodes.isEmpty){
        throw new Exception("root.nodes.isEmpty")
      }

      // Reinsert the points using the (new) root
      for(p <- points){
        root = root.findNodeToInsert(p).addPoint(p)
      }

    }
    root
  }
  def query(qRange: Range): ListSet[Point] = {
    val list = getNodesForQuery(qRange)
    var points = new ListSet[Point]()

    for( n <- list ){
      for( p <- n.bucket ){
        if(qRange.contains(p)) points += p
      }
    }

    points
  }

  def getNodesForQuery(qRange: Range): ListSet[Node] ={
    var list = new ListSet[Node]()

    if (range.intersects(qRange)){
      if(nodes.isEmpty){
        // Leaf node
        list += this
      }else{
        for ( n <- nodes ){
          list ++= n.getNodesForQuery(qRange)
        }
      }
    }

    list
  }

  def addNode(node: Node): Node = {
    if(nodes.size<MAX_NODES){
      // Case 1
      // Add node as our child
      node.parent = this
      nodes += node

      // Adjust our range
      calculateRange()

      getRoot
    }else{
      // We are full (root might be here)

      nodes += node

      // Divide nodes into 2 MBRs
      var nodes1 = new ListSet[Node]()
      var nodes2 = new ListSet[Node]()
      val new_node1 = new Node(D, this)
      val new_node2 = new Node(D, this)

      // === Split nodes into 2 MBRs ===========================================

      // Find the worst pair
      var node1: Node = node
      var node2: Node = node
      var tmp_area = 0.0

      for( n1 <- nodes ){
        for( n2 <- nodes ){
          if(n1.range.combine(n2.range).space > tmp_area){
            node1 = n1
            node2 = n2
            tmp_area = n1.range.combine(n2.range).space
          }
        }
      }

      // Add the 2 nodes in each list
      nodes1 += node1
      node1.parent = new_node1
      nodes -= node1

      nodes2 += node2
      node2.parent = new_node2
      nodes -= node2

      // Make sure we are covering the MIN_NODES requirement
      for( _ <- 1 until MIN_NODES ) {
        var tmp: Node = null

        tmp = nodes.minBy[Double]((n: Node) => n.range.combine(node1.range).space)
        nodes1 += tmp
        tmp.parent = new_node1
        nodes -= tmp

        tmp = nodes.minBy[Double]((n: Node) => n.range.combine(node2.range).space)
        nodes2 += tmp
        tmp.parent = new_node2
        nodes -= tmp
      }

      var area1: Double = 0.0
      var area2: Double = 0.0

      // Add each node to the best-fit mbr
      for ( n <- nodes ){
        area1 = node1.range.combine(n.range).space
        area2 = node2.range.combine(n.range).space

        if(area1 < area2){
          nodes1 += n
          n.parent = new_node1
        }else{
          nodes2 += n
          n.parent = new_node2
        }
      }

      // === End of split ======================================================

      new_node1.nodes = nodes1
      new_node2.nodes = nodes2

      new_node1.calculateRange()
      new_node2.calculateRange()

      if(isRoot){
        // Case 2
        // We are root

        // Create new root node
        val new_root = new Node(D, this)
        new_root.parent = null

        // Add new nodes to new root
        new_root.addNode(new_node1) // This should go to Case 1
        new_root.addNode(new_node2) // This should go to Case 1 - this is returned

      }else{
        // Case 3
        parent.nodes -= this
        parent.addNode(new_node1) // This should go to Case 1
        parent.addNode(new_node2) // This can go to any case - this is returned
      }
    }
  }
  def deleteNode(node: Node): ListSet[Point] = {
    // Deletes a node and returns a list with points to be reinserted - Recursive calls to parent in case of underflow

    var list = new ListSet[Point]()

    // If we are root and we only have one node, there is nothing to be done
    if(isRoot && nodes.size==1) return list

    // Remove the requested node
    nodes -= node

    // Check for underflow
    if(nodes.size<MIN_NODES){

      // Gather points to be reinserted
      if(!isRoot) {
        list ++= getAllPoints
        list ++= parent.deleteNode(this) //node doesn't exist anymore so no duplicate points will be returned here
      }

    }else{
      // Recalculate our range
      calculateRange()
    }

    list
  }


  def findNodeToInsert(point: Point): Node = {
    // Recursively find a leaf node to insert 'point'

    if (nodes.isEmpty) this
    else nodes.minBy[Double]( (n: Node) => n.range.extensionArea(point) ).findNodeToInsert(point)

  }
  def findPoint(point: Point): Node = {
    // Find the node that contains 'point' in its bucket

    for( n <- getNodesForQuery(point.toRange) ){
      if( n.bucket.exists( (p: Point) => p.equals(point) )){
        return n
      }
    }

    null
  }
  def getRoot: Node = if(isRoot) this else parent.getRoot
  def isRoot: Boolean = parent==null
  def calculateRange(): Unit = {
    // Recursively update our range and our ancestors' ranges

    val limits = ofDim[Double](D, 2)

    if(bucket.nonEmpty){

      for( i <- 0 until D){
        limits(i)(0) = bucket.minBy[Double]( (p: Point) => p.c(i) ).c(i)
        limits(i)(1) = bucket.maxBy[Double]( (p: Point) => p.c(i) ).c(i)
      }

    }else if(nodes.nonEmpty){

      for( i <- 0 until D){
        limits(i)(0) = nodes.minBy[Double]( (n: Node) => n.range.limits(i)(0) ).range.limits(i)(0)
        limits(i)(1) = nodes.maxBy[Double]( (n: Node) => n.range.limits(i)(1) ).range.limits(i)(1)
      }

    }

    range = new Range(D, limits)
    if(!isRoot) parent.calculateRange()
  }
  def getAllPoints: ListSet[Point] = {
    // Recursively collect all points from child nodes
    var list = new ListSet[Point]()

    if(bucket.nonEmpty){
      list ++= bucket
    }else {
      for (n <- nodes) {
        list ++= n.getAllPoints
      }
    }

    list
  }


  def print(level: Int): Unit ={
    for (_ <- 1 to level) {
      printf("\t")
    }
    if(isRoot) printf("[Root] ")
    println(this + " " + range)

    if(bucket.nonEmpty){
      for ( p <- bucket ) {
        for (_ <- 1 to level+1) {
          printf("\t")
        }
        println("Point " + p)
      }
    }else{
      for ( n <- nodes ) n.print(level+1)
    }
  }
  def verify: Boolean = {
    if((bucket.isEmpty && ((nodes.size<MIN_NODES && !isRoot) || nodes.size>MAX_NODES)) || bucket.size>MAX_BUCKET_SIZE) false
    else{
      for ( n <- nodes ){
        if(!n.verify) false
      }
      true
    }
  }

}
