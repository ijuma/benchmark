package jsr166.scala

/**
 * Misc utilities in JSR166 performance tests
 */
object LoopHelpers {

  val staticRNG = new SimpleRandom

  // Some mindless computation to do between synchronizations...

  /**
   * generates 32 bit pseudo-random numbers.
   * Adapted from http://www.snippets.org
   */
  def compute1(x: Int): Int = {
    var lo = 16807 * (x & 0xFFFF)
    val hi = 16807 * (x >>> 16)
    lo += (hi & 0x7FFF) << 16
    if ((lo & 0x80000000) != 0) {
        lo &= 0x7fffffff
        lo += 1
    }
    lo += hi >>> 15;
    if (lo == 0 || (lo & 0x80000000) != 0) {
        lo &= 0x7fffffff
        lo += 1
    }
    lo
  }

  /**
   *  Computes a linear congruential random number a random number
   *  of times.
   */
  def compute2(p: Int): Int = {
    var x = p
    var loops = (x >>> 4) & 7;
    while (loops > 0) {
        x = (x * 2147483647) % 16807
        loops -= 1
    }
    x;
  }

  /**
   * Yet another random number generator
   */
  def compute3(x: Int): Int = {
    val t = (x % 127773) * 16807 - (x / 127773) * 2836;
    if (t > 0) t else t + 0x7fffffff
  }

  /**
   * Yet another random number generator
   */
  def compute4(x: Int): Int = x * 134775813 + 1;
    
  object SimpleRandom {
    private val multiplier = 0x5DEECE66DL;
    private val addend = 0xBL;
    private val mask: Long = (1L << 48) - 1;
    private val seq = new java.util.concurrent.atomic.AtomicLong(1)
  }

  /**
   * An actually useful random number generator, but unsynchronized.
   * Basically same as java.util.Random.
   */
  class SimpleRandom(s: Option[Long]) {
    private var seed = System.nanoTime() + SimpleRandom.seq.getAndIncrement();
    s.foreach(se => seed = se)
    
    def this() = this(None)
    def this(s: Long) = this(Some(s))
    
    def setSeed(s: Long) {
      seed = s;
    }

    def next: Int = {
      import SimpleRandom._
      val nextseed = (seed * multiplier + addend) & mask
      seed = nextseed
      return (nextseed >>> 17).toInt & 0x7FFFFFFF
    }
  }

  class BarrierTimer extends Runnable {
    @volatile var startTime: Long = _
    @volatile var endTime: Long = _
    
    def run {
      val t = System.nanoTime
      if (startTime == 0) startTime = t
      else endTime = t
    }
    
    def clear {
      startTime = 0;
      endTime = 0;
    }
    
    def getTime: Long = endTime - startTime;
  }

  def rightJustify(n: Long): String = {
    // There's probably a better way to do this...
    val field = "         ";
    val num = java.lang.Long.toString(n);
    if (num.length() >= field.length()) num
    else {
      val b = new StringBuffer(field);
      b.replace(b.length()-num.length(), b.length(), num);
      b.toString()
    }
  }
}
