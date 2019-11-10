import Array._

class Point(var D: Int, var c: Array[Double]){
  // c(0) = x, c(1) = y, c(2) = z, ...

  override def toString: String = {
    var str = "("

    for( i <- 0 until D){
      str += "[" + c(i) + "]"
    }

    str + ")"
  }

  override def equals(obj: Any): Boolean = {
    for (i <- 0 until D){
      if(obj.asInstanceOf[Point].c(i) != this.c(i)) return false
    }
    true
  }
  def toRange: Range = {
    var limits = ofDim[Double](D, 2)

    for (i <- 0 until D){
      limits(i)(0) = c(i)
      limits(i)(1) = c(i)
    }
    new Range(D, limits)
  }

}
