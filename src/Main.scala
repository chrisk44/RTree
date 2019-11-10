import scala.util.Random

object Main extends App {

  run_interface

  def run_interface: Unit = {

    val options = "\n" +
        "  0: Exit\n" +
        "  1: Add random points\n" +
        "  2: Add point\n" +
        "  3: Delete point\n" +
        "  4: Query\n" +
        "  5: Delete from Query\n" +
        "  6: Print tree"
    var option: Int = 0

    val dim = readInteger("Enter tree dimension: ")
    val tree: RTree = new RTree(dim)

    do{

      println(options)
      option = readInteger("Enter selection: ", true)

      option match {
        case 0 => null
        case 1 =>
          // Add random points - Point may exist
          var count = readInteger("How many points to insert? ")
          val rand = new Random()

          while(count>0){

            try{

              val tmp_array = new Array[Double](dim)
              var p: Point = null

              for( j <- 0 until dim){
                tmp_array(j) = Math.floor(rand.nextDouble() * 100000) / 100
              }
              p = new Point(dim, tmp_array)
              println("Inserting " + p)
              tree.insert(p)

              // Insertion worked, so point didn't exist, so decrease count
              count = count - 1

            }catch{
              case _: Exception => //Ignore it
            }

          }


        case 2 =>
          // Add one point
          val p: Point = readPoint(dim)
          println("Inserting " + p)
          try {
            tree.insert(p)
          }catch{
            case _: Exception => println("Something went wrong, maybe point already exists")
          }


        case 3 =>
          // Delete point
          val p: Point = readPoint(dim)
          println("Deleting " + p)
          try {
            tree.delete(p)
          }catch{
            case _: Exception => println("Something went wrong, maybe point doesn't exist")
          }

        case 4 =>
          // Query
          val query = readQuery(dim)
          if(query!=null) {
            val points = tree.query(query)

            println("Found " + points.size + " points")
            for (p <- points) {
              println(p)
            }
          }

        case 5 =>
          // Delete from Query
          val query = readQuery(dim)
          if(query!=null) {
            val points = tree.query(query)

            println("Found " + points.size + " points")
            for (p <- points) {
              println("Deleting " + p)
              try {
                tree.delete(p)
              } catch {
                case _: Exception => println("Something went wrong, no idea")
              }
            }
          }

        case 6 =>
          // Print tree
          tree.print()
      }

    }while(option!=0)
  }

  def readInteger(txt: String, allow_zero: Boolean = false): Int = {
    while(true){
      try{
        print(txt)
        val d = scala.io.StdIn.readInt()
        if(d > 0 || (d==0 && allow_zero)) return d
        else throw new NumberFormatException
      } catch {
        case _: NumberFormatException => println("Invalid number: Expected integer" + { if(!allow_zero) " >0" else "" })
        case e: Exception => throw e
      }
    }
    -1
  }
  def readDouble(txt: String): Double = {
    while(true){
      try{
        print(txt)
        return scala.io.StdIn.readDouble()
      } catch {
        case _: NumberFormatException => println("Invalid number: Expected double (use . for decimal point)")
        case e: Exception => throw e
      }
    }
    0.0
  }
  def readPoint(d: Int): Point = {
    val tmp_array = new Array[Double](d)

    for (i <- 0 until d){
      tmp_array(i) = readDouble("Enter value for dimension " + (i+1) + ": ")
    }

    new Point(d, tmp_array)
  }
  def readQuery(d: Int): Range = {
    val tmp_array = new Array[Array[Double]](d)

    for (i <- 0 until d){
      val lower_bound = readDouble("Lower bound for dimension " + (i+1) + ": ")
      val upper_bound = readDouble("Upper bound for dimension " + (i+1) + ": ")

      if(lower_bound>upper_bound){
        println("Lower bound must be lower than the upper bound")
        return null
      }
      tmp_array(i) = Array(lower_bound, upper_bound)
    }

    new Range(d, tmp_array)
  }

}
