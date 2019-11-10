import scala.Array._

class Range(var D: Int, var limits: Array[Array[Double]]) {
  // limits(0, 0) = bottom
  // limits(0, 1) = top
  // limits(1, 0) = left
  // limits(1, 1) = right
  // ...


  override def toString: String = {
    // ([0,1][3,2][1,0])
    var str = "("

    for( i <- 0 until D){
      str += "[" + limits(i)(0) + "," + limits(i)(1) + "]"
    }

    str + ")"
  }
  def contains(point: Point): Boolean = {

    for (i <- 0 until D){
      if( point.c(i)<limits(i)(0) || point.c(i)>limits(i)(1) ) return false
    }
    true

  }
  def intersects(range: Range): Boolean = {

    for (i <- 0 until D){
      if( range.limits(i)(0)>this.limits(i)(1) || range.limits(i)(1)<this.limits(i)(0) ) return false
    }
    true

  }
  def space: Double = {
    var space = 1.0

    for (i <- 0 until D){
      space *= Math.abs( limits(i)(0) - limits(i)(1) )
    }

    space
  }

  def combine(range: Range): Range = {

    var limits = ofDim[Double](D, 2)

    for (i <- 0 until D){
      limits(i)(0) = Math.min(range.limits(i)(0), this.limits(i)(0))
      limits(i)(1) = Math.max(range.limits(i)(1), this.limits(i)(1))
    }

    new Range(D, limits)

  }

  def getExtendedRange(point: Point): Range = {
    // Returns an extended range that includes point

    var new_limits = limits

    for (i <- 0 until D){
      if( point.c(i)<limits(i)(0) ){
        limits(i)(0) = point.c(i)
      }else if( point.c(i)>limits(i)(1) ){
        limits(i)(1) = point.c(i)
      }
    }

    new Range(D, new_limits)
  }
  def extensionArea(point: Point): Double = {
    // Returns how much the range must be extended to include point

    getExtendedRange(point).space - this.space
  }

}
