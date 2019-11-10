import scala.collection.immutable.ListSet

class RTree (var D: Int) {

  var root: Node = new Node(D, null)

  def insert(point: Point): Unit = {
    if(point.D!=this.D) throw new IllegalArgumentException("Can't insert a " + point.D + "-dimensional object in a " + D + "-dimensional tree")
    root = root.findNodeToInsert(point).addPoint(point)
  }
  def delete(point: Point): Unit = {
    if(point.D!=this.D) throw new IllegalArgumentException("Can't delete a " + point.D + "-dimensional object from a " + D + "-dimensional tree")
    root = root.findPoint(point).delete(point)
  }
  def query(range: Range): ListSet[Point] = {
    if(range.D!=this.D) throw new IllegalArgumentException("Can't query a " + range.D + "-dimensional range in a " + D + "-dimensional tree")
    root.query(range)
  }

  def print(): Unit = {
    root.print(0)
  }

  def verify: Boolean = {
    root.verify
  }

}
