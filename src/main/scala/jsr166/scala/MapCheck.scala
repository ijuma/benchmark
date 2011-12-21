/*
 * Original Java version Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 * 
 * jatran used for initial translation to Scala
 */

package jsr166.scala

import _root_.scala.collection._

/**
 * @test
 * @synopsis Times and checks basic map operations
 *
 * When run with "s" second arg, this requires file "testwords", which
 * is best used with real words.  We can't check in this file, but you
 * can create one from a real dictionary (1 line per word) and then run
 * linux "shuf" to randomize entries.
 */
object MapCheck {
  
  val Missing = new Object()
  var timer = new TestTimer()
  var eclass: Class[_] = null
  var mapClass: Class[_ <: mutable.Map[AnyRef, AnyRef]] = classOf[mutable.HashMap[AnyRef, AnyRef]]
  val srng = new LoopHelpers.SimpleRandom()
  val rng = new java.util.Random(3152688)
  @volatile var checkSum: Int = _
  
  def reallyAssert(b: Boolean): Unit = if (!b) throw new Error("Failed Assertion")

  def main(args:Array[String]) {
    var numTests = 100
    var size = 36864 // about midway of HashMap resize interval
    if (args.length == 0) 
      System.out.println("Usage: MapCheck mapclass [int|float|string|object] [trials] [size] [serialtest]")

    if (args.length > 0) {
      try {
        mapClass = Class.forName(args(0)).asInstanceOf[Class[mutable.Map[AnyRef, AnyRef]]]
      } catch { 
        case e:ClassNotFoundException => 
          throw new RuntimeException("Class " + args(0) + " not found.")
      }
    }

    if (args.length > 1) {
      val et = args(1).toLowerCase()
      if (et.startsWith("i")) 
        eclass = classOf[java.lang.Integer]
      else if (et.startsWith("f")) 
        eclass = classOf[java.lang.Float]
      else if (et.startsWith("s")) 
        eclass = classOf[java.lang.String]
      else if (et.startsWith("d"))
        eclass = classOf[java.lang.Double]
    }
    if (eclass == null) eclass = classOf[Object]

    if (args.length > 2) numTests = Integer.parseInt(args(2))

    if (args.length > 3) size = Integer.parseInt(args(3))

    val doSerializeTest = args.length > 4

    while ((size & 3) != 0) size += 1
    
    print("Class: " + mapClass.getName())
    print(" elements: " + eclass.getName())
    print(" trials: " + numTests)
    print(" size: " + size)
    println()
    var key = new Array[Object](size)
    var absent = new Array[Object](size)
    initializeKeys(key, absent, size)
    precheck(size, key, absent)

    {
      var rep = 0
      while (rep < numTests) {
        mainTest(newMap, key, absent)
        if ((rep & 3) == 3 && rep < numTests - 1) {
          shuffle(key)
          // Thread.sleep(10)
        }
        rep += 1
      }
    }

    TestTimer.printStats
//    checkNullKey()
    
    if (doSerializeTest) serTest(newMap, size)
  }
  
  def newMap: mutable.Map[AnyRef, AnyRef] = {
    try {
      return mapClass.newInstance
    } catch { 
      case e:Exception => 
        throw new RuntimeException("Can't instantiate " + mapClass + ": " + e)
    }
  }

  def precheck(n: Int, key: Array[Object], abs: Array[Object]) {
    var ck = 0
    var s = newMap

    {
      var i = 0
      while (i < n) {
        var k = key(i)
        if (k eq null) throw new Error("Null key at" + i)
        ck += System.identityHashCode(k)
        val v = s.put(k, k)
        if (v.isDefined) throw new Error("Duplicate " + k + " / " + v.get)
        i += 1
      }
    }


    {
      var i:Int = 0
      while (i < n) {
        var k:Object = abs(i)
        if (k == null) 
          throw new Error("Null key at" + i)


        ck += System.identityHashCode(k)
        val v = s.put(k, k)
        if (v.isDefined) throw new Error("Duplicate " + k + " / " + v.get)
        i += 1
      }
    }

    checkSum += ck
  }

  def checkNullKey() {
    val m = newMap
    val x = new Object
    try {
      m.put(null, x)
      val v = m.get(null)
      if (v.get != x) throw new Error()
      if (m.remove(null.asInstanceOf[AnyRef]).get != v) throw new Error()
      if (m.get(null).isDefined) throw new Error()
    } catch { 
      case npe: NullPointerException => println("Map does not allow null keys")
    }
  }

  def getTest(nm: String, n: Int, s: Map[AnyRef, AnyRef], key: Array[Object], expect: Int) {
    var sum = 0
    timer.start(nm, n)

    var i = 0
    while (i < n) {
      val v = s.get(key(i))
      if (v.isDefined && v.get.getClass == eclass) 
        sum += 1

      i += 1
    }

    timer.finish
    reallyAssert(sum == expect)
    checkSum += sum
  }
  
  def remTest(nm: String, n: Int, s: mutable.Map[AnyRef, AnyRef], key: Array[Object], expect:Int) {
    var sum:Int = 0
    timer.start(nm, n)

    {
      var i:Int = 0
      while (i < n) {
        if (s.remove(key(i)).isDefined) sum = sum + 1
        i += 1
      }
    }

    timer.finish
    reallyAssert(sum == expect)
    checkSum += sum
  }

  def clrTest(n:Int, s: mutable.Map[AnyRef, AnyRef]) {
    var nm:String = "Remove Present         "
    timer.start(nm, n)
    s.clear
    timer.finish
    reallyAssert(s.isEmpty)
  }

  def putTest(nm: String, n: Int, s: mutable.Map[AnyRef, AnyRef], key: Array[Object], expect: Int):Unit = {
    var sum:Int = 0
    timer.start(nm, n)

    {
      var i:Int = 0
      while (i < n) {
        var k:Object = key(i)
        if (s.put(k, k).isEmpty) sum = sum + 1
        i += 1
      }
    }

    timer.finish
    reallyAssert(sum == expect)
    checkSum += sum
  }

  def keyTest(nm: String, n: Int, s: Map[AnyRef, AnyRef], key: Array[Object], expect: Int) {
    var sum = 0
    timer.start(nm, n)

    {
      var i = 0
      while (i < n) {
        if (s.contains(key(i))) sum += 1
        i += 1
      }
    }

    timer.finish
    reallyAssert(sum == expect)
    checkSum += sum
  }
  
  // version without timing for uncategorized tests
  def untimedKeyTest(nm: String, n: Int, s: Map[AnyRef, AnyRef], key: Array[Object], expect: Int) {
    var sum:Int = 0

    {
      var i = 0
      while (i < n) {
        if (s.contains(key(i))) sum += 1
        i += 1
      }
    }

    reallyAssert(sum == expect)
    checkSum += sum
  }

  def remHalfTest(nm: String, n: Int, s: mutable.Map[AnyRef, AnyRef], key: Array[Object], expect: Int) {
    var sum = 0
    timer.start(nm, n / 2)

    {
      var i:Int = n - 2
      while (i >= 0) {
        if (s.remove(key(i)).isDefined) sum += 1
        i -= 2
      }
    }

    timer.finish
    reallyAssert(sum == expect)
    checkSum += sum
  }

  def valTest(s: Map[AnyRef, AnyRef], key: Array[Object]) {
    val size = s.size
    var sum = 0
    timer.start("Traverse key or value  ", size)
    if (s.values.exists(_ == Missing)) sum += 1

    timer.finish
    reallyAssert(sum == 0)
    checkSum += sum
  }

  def kitTest(s: Map[AnyRef, AnyRef], size:Int): AnyRef = {
    var last:Object = null
    var sum:Int = 0
    timer.start("Traverse key or value  ", size)

    s.keySet.foreach { x =>
      if (x != last && (x ne null) && x.getClass == eclass) sum += 1
      last = x
    }
    
    timer.finish
    reallyAssert(sum == size)
    checkSum += sum
    last
  }

  def vitTest(s: Map[AnyRef, AnyRef], size: Int): AnyRef = {
    var last: Object = null
    var sum = 0
    timer.start("Traverse key or value  ", size)

    s.values.foreach { x =>
      if (x != last && (x ne null) && x.getClass == eclass) sum = sum + 1
      last = x
    }

    timer.finish
    reallyAssert(sum == size)
    checkSum += sum
    last
  }

  def eitTest(s: Map[AnyRef, AnyRef], size: Int) {
    var sum = 0
    timer.start("Traverse entry         ", size)

    s.foreach { case (k, v) =>
      if (k.ne(null) && k.getClass == eclass && v.ne(null) && v.getClass == eclass) 
        sum += 1
    }

    timer.finish
    reallyAssert(sum == size)
    checkSum += sum
  }
  
  def itRemTest(s: mutable.Map[AnyRef, AnyRef], size: Int) {
    reallyAssert (s.size == size);
    /* Iterator in scala doesn't have a remove method, so we just perform the side-effects */
    val sum = s.keySet.size
    s.clear
    checkSum += sum;
  }

  def itHalfRemTest(s: mutable.Map[AnyRef, AnyRef], size: Int) {
    /* Iterator in scala doesn't have a remove method, so we just perform the side-effects */
    val sz = s.size
    reallyAssert (sz == size)
    var sum = 0
    val it = (new mutable.ArrayBuffer[AnyRef] ++= s.keySet).iterator
    while (it.hasNext) {
        s -= it.next
        if (it.hasNext)
            it.next
        sum += 1
    }
    reallyAssert (sum == sz / 2);
    checkSum += sum
  }

  def putAllTest(nm: String, n: Int, src: Map[AnyRef, AnyRef], dst: mutable.Map[AnyRef, AnyRef]) {
    timer.start(nm, n)
    dst ++= src
    timer.finish
    reallyAssert(src.size == dst.size)
  }

  def serTest(s: mutable.Map[AnyRef, AnyRef], size: Int) {
    if (!(s.isInstanceOf[java.io.Serializable])) return

    System.out.print("Serialize              : ")

    {
      var i:Int = 0
      while (i < size) {
        s.put(new java.lang.Integer(i), java.lang.Boolean.TRUE)
        i = i + 1
      }
    }

    import java.io._
    val startTime = System.currentTimeMillis()
    val out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream("MapCheck.dat")))
    out.writeObject(s)
    out.close()
    val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream("MapCheck.dat")))
    val m = in.readObject.asInstanceOf[Map[AnyRef, AnyRef]] 
    val endTime = System.currentTimeMillis()
    val time = endTime - startTime
    System.out.print(time + "ms")
    if (s.isInstanceOf[java.util.IdentityHashMap[_, _]]) return
    reallyAssert(s.equals(m))
  }

  def mainTest(s: mutable.Map[AnyRef, AnyRef], key: Array[Object], absent: Array[Object]) {
    val size = key.length
    putTest("Add    Absent          ", size, s, key, size)
    reallyAssert(s.size == size)
    getTest("Access Present         ", size, s, key, size)
    getTest("Search Absent          ", size, s, absent, 0)
    kitTest(s, size)
    vitTest(s, size)
    eitTest(s, size)
    putTest("Modify Present         ", size, s, key, 0)
    reallyAssert(s.size == size)
    untimedKeyTest("Access Present         ", size, s, key, size)
    keyTest("Search Absent          ", size, s, absent, 0)
    valTest(s, key)
    remTest("Search Absent          ", size, s, absent, 0)
    reallyAssert(s.size == size)
    remHalfTest("Remove Present         ", size, s, key, size / 2)
    reallyAssert(s.size == size / 2)
    getTest("Access Present         ", size, s, key, size / 2)
    putTest("Add    Absent          ", size, s, key, size / 2)
    reallyAssert(s.size == size)
    getTest("Access Present         ", size, s, key, size)
    getTest("Search Absent          ", size, s, absent, 0)
    itRemTest(s, size)
    putTest("Add    Absent          ", size, s, key, size)
    reallyAssert(s.size == size)
    getTest("Access Present         ", size, s, key, size)
    untimedKeyTest("Access Present         ", size, s, key, size)
    kitTest(s, size)
    vitTest(s, size)
    eitTest(s, size)
    twoMapTest1(s, key, absent)
    twoMapTest2(s, key, absent)
  }

  def twoMapTest1(s: Map[AnyRef, AnyRef], key: Array[Object], absent: Array[Object]) {
    var size = s.size
    var s2 = newMap
    putAllTest("Add    Absent          ", size, s, s2)
    getTest("Access Present         ", size, s2, key, size)
    itHalfRemTest(s2, size)
    reallyAssert(s2.size == size / 2)
    itHalfRemTest(s2, size / 2)
    reallyAssert(s2.size == size / 4)
    putTest("Add    Absent          ", size, s2, absent, size)
    putTest("Add    Absent          ", size, s2, key, size * 3 / 4)
    reallyAssert(s2.size == size * 2)
    clrTest(size, s2)
  }

  def twoMapTest2(s: mutable.Map[AnyRef, AnyRef], key: Array[Object], absent: Array[Object]) {
    var size = key.length
    var s2 = newMap
    putAllTest("Add    Absent          ", size, s, s2)
    putAllTest("Modify Present         ", size, s, s2)
    val lastkey = kitTest(s2, size)
    val hold = s2.get(lastkey)
    var sum = 0
    timer.start("Traverse entry         ", size * 12)
    val sh1 = s.hashCode() - s2.hashCode()
    reallyAssert(sh1 == 0)
    var eq1 = s2.equals(s)
    var eq2 = s.equals(s2)
    reallyAssert(eq1 && eq2)

    s.foreach { case (k, v) =>
      if (s2.contains(k)) sum += 1
    }
    reallyAssert(sum == size)
    
    s2.put(lastkey, Missing)
    
    val sh2 = s.hashCode - s2.hashCode
    reallyAssert(sh2 != 0)

    eq1 = s2.equals(s)
    eq2 = s.equals(s2)
    reallyAssert(!eq1 && !eq2)
    
    sum = 0
    s.foreach { case (k, v) =>
      s.put(k, absent(sum))
      sum += 1
    }
    reallyAssert(sum == size)

    s2.foreach { case (k, v) =>
      s2.put(k, s(k))
    }

    timer.finish
    var rmiss = 0
    timer.start("Remove Present         ", size * 2)
    s2.foreach { case (k, v) =>
      if (s.remove(k).isEmpty) rmiss += 1
    }
    
    timer.finish
    reallyAssert(rmiss == 0)
    clrTest(size, s2)
    reallyAssert(s2.isEmpty && s.isEmpty)
  }
  
  def initializeKeys(key: Array[Object], absent: Array[Object], size: Int) {
    if (eclass == classOf[Object]) {
      var i = 0
      while (i < size) {
        key(i) = new Object
        i += 1
      }

      i = 0
      while (i < size) {
        absent(i) = new Object
        i += 1
      }
    }
    else if (eclass == classOf[java.lang.Integer]) initInts(key, absent, size)
    else if (eclass == classOf[java.lang.Float]) initFloats(key, absent, size)
    else if (eclass == classOf[java.lang.Double]) initDoubles(key, absent, size)
    else if (eclass == classOf[String]) initWords(size, key, absent)
    else throw new Error("unknown type")
  }
  
  def initInts(key: Array[Object], absent: Array[Object], size: Int) {
    var i = 0
    while (i < size) {
      key(i) = java.lang.Integer.valueOf(i)
      i += 1
    }

    var m = newMap
    
    var k = 0
    while (k < size) {
      val r = srng.next
      if (r < 0 || r >= size) {
        val ir = java.lang.Integer.valueOf(r)
        if (m.put(ir, ir).isEmpty) {
          absent(k) = ir
          k += 1
        }
      }
    }
  }

  def initFloats(key: Array[Object], absent: Array[Object], size: Int) {
    var m = newMap

    var i = 0
    while (i < size) {
      val r = java.lang.Float.valueOf(i)
      key(i) = r
      m.put(r, r)
      i += 1
    }

    var k = 0
    while (k < size) {
      val r = rng.nextFloat()
      val ir = java.lang.Float.valueOf(r)
      if (m.put(ir, ir).isEmpty) {
        absent(k) = ir
        k += 1
      }
    }
  }

  def initDoubles(key: Array[Object], absent: Array[Object], size:Int) {
    val m = newMap

    var i = 0
    while (i < size) {
      val r = java.lang.Double.valueOf(i)
      key(i) = r
      m.put(r, r)
      i += 1
    }

    var k = 0
    while (k < size) {
      val r = rng.nextDouble()
      val ir = java.lang.Double.valueOf(r)
      if (m.put(ir, ir).isEmpty) { 
        absent(k) = ir
        k += 1
      }
    }
  }
  
  // Use as many real words as possible, then use fake random words
  def initWords(size:Int, key: Array[Object], abs: Array[Object]):Unit = {
    val fileName = "testwords.txt"
    var ki = 0
    var ai = 0
    try {
      val in = new java.io.BufferedReader(new java.io.InputStreamReader(
          currentThread.getContextClassLoader.getResourceAsStream(fileName)))

      var line: String = null
      while ((ki < size || ai < size)  && {line = in.readLine; line != null}) {
        if (ki < size) { 
          key(ki) = line
          ki += 1
        }
        else {
          abs(ai) = line
          ai += 1
        }
      }
      if (ki < size) 
        randomWords(key, ki, size)
      if (ai < size) 
        randomWords(abs, ai, size)
        
      in.close()
    } catch { 
      case ex: java.io.IOException => 
        System.out.println("Can't read words file:" + ex)
        throw new Error(ex)

    }
  }

  def randomWords(ws: Array[Object], origin: Int, size: Int) {
    var i = origin
    while (i < size) {
      var k = 0
      val len = 2 + (srng.next & 0xf)
      val c = new Array[Char](len * 4 + 1)

      var j = 1
      while (j < len) {
        var r = srng.next
        c({k += 1; k - 1}) = (' ' + (r & 0x7f)).asInstanceOf[Char] 
        r >>>= 8
        c({k += 1; k - 1}) = (' ' + (r & 0x7f)).asInstanceOf[Char] 
        r >>>= 8
        c({k += 1; k - 1}) = (' ' + (r & 0x7f)).asInstanceOf[Char] 
        r >>>= 8
        c({k += 1; k - 1}) = (' ' + (r & 0x7f)).asInstanceOf[Char] 
        j += 1
      }

      c({k += 1; k - 1}) = ((i & 31) | 1).asInstanceOf[Char]  // never == to any testword
      ws(i) = new String(c)
      i += 1
    }
  }

  def shuffle(keys: Array[Object]) {
    val size = keys.length
    var i = size
    while (i > 1) {
      val r = rng.nextInt(i)
      val t = keys(i - 1)
      keys(i - 1) = keys(r)
      keys(r) = t
      i -= 1
    }
  }

  def shuffle[A](keys: mutable.Buffer[A]) {
    val size = keys.size

    var i = size
    while (i > 1) {
      val r = rng.nextInt(i)
      val t = keys(i - 1)
      keys(i - 1) = keys(r)
      keys(r) = t
      i -= 1
    }
  }
  
  object TestTimer {
    
    import JavaConversions._
    val accum = new java.util.TreeMap[String, Stats]()
    def printStats {
      accum.foreach { case (name, stats) =>
        println(name + ": ")
        val (s, n) =
          if (stats.number == 0) (stats.first, stats.firstn)
          else (stats.sum, stats.number)
        val t = s.toDouble / n
        val nano = Math.round(t)
        printf("%6d", + nano)
        println()
      }
    }
  }
  
  final class TestTimer {
    import TestTimer._
    
    private var name: String = _
    private var numOps: Long = _
    private var startTime: Long = _
    
    def start(name: String, numOps: Long) {
      this.name = name
      this.numOps = numOps
      startTime = System.nanoTime
    }
    
    def finish {
      val elapsed = System.nanoTime - startTime
      accum.get(name) match {
        case null => accum.put(name, new Stats(elapsed, numOps))
        case stats => stats.addTime(elapsed, numOps)
      }
    }
  }
  
  final class Stats(val first: Long, val firstn: Long) {
    var sum: Long = _
    var number: Long = _
    
    def addTime(t: Long, n: Long) {
      sum += t
      number += n
    }
  }
}
