import scalacl._

case class Matrix(data: CLArray[Float],
                  rows: Int,
                  columns: Int)
                 (implicit context: Context)
{
  def this(rows: Int, columns: Int)
          (implicit context: Context) =
    this(new CLArray[Float](rows * columns), rows, columns)

  def this(n: Int)
          (implicit context: Context) =
    this(n, n)

  def putProduct(a: Matrix, b: Matrix): Unit = {
    assert(a.columns == b.rows)
    assert(a.rows == rows)
    assert(b.columns == columns)

    kernel {
      // This block will either be converted to an OpenCL kernel or cause compilation error
      for (i <- 0 until rows;
           j <- 0 until columns) {
        // c(i, j) = sum(k, a(i, k) * b(k, j))
        data(i * columns + j) = (
          for (k <- 0 until a.columns) yield
          a.data(i * a.columns + k) * b.data(k * b.columns + j)
          ).sum
      }
    }
  }

  def putSum(a: Matrix, b: Matrix): Unit = {
    assert(a.columns == b.columns && a.columns == columns)
    assert(a.rows == b.rows && a.rows == rows)

    kernel {
      for (i <- 0 until rows; j <- 0 until columns) {
        val offset = i * columns + j
        data(offset) = a.data(offset) + b.data(offset)
      }
    }
  }
}

implicit val context = Context.best

val n = 10
val a = new Matrix(n)
val b = new Matrix(n)
val out = new Matrix(n)

out.putProduct(a, b)

println(out.data)