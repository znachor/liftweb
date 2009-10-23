trait Benchmark {
  def run(name: String, warmup: Int, count: Int)(f: => Any) = {
    println("warmup")
    repeat(warmup)(f)
    println("warmup done")
    val t = time {
      repeat(count)(f)
    }
    println(name + "\t" + t + "ms")
  }

  def repeat(count: Int)(f: => Any) = {
    var i = 0; while (i < count) {
      f
      i += 1
    }
  }

  def time(f: => Any): Long = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis - start
  }
}
