package util

import scala.collection._
import generic.CanBuildFrom
import mutable.{MapBuilder, Builder}

object ChainedHashMap {
  def empty[K, V] = apply[K, V]
  def apply[K, V] = new ChainedHashMap[K, V]()

  def apply[K, V](elems : (K, V)*) = new ChainedHashMap[K, V](elems.size) ++= (elems);

  private[util] class Entry[Key, Value](val key : Key, 
                                           val hash : Int,
                                           var value : Value,
                                           var next: Entry[Key, Value])
                                           
  /* See http://bits.stephan-brumme.com/roundUpToNextPowerOfTwo.html */
  private[util] def powerOfTwo(value: Int): Int = {
    var c = value - 1;
    c |= c >>>  1;
    c |= c >>>  2;
    c |= c >>>  4;
    c |= c >>>  8;
    c |= c >>> 16;
    c + 1;
  }

  implicit def canBuildFrom[K, V]: CanBuildFrom[ChainedHashMap[_, _], (K, V), ChainedHashMap[K, V]] =
    new CanBuildFrom[ChainedHashMap[_, _], (K, V), ChainedHashMap[K, V]] {
      def apply(from: ChainedHashMap[_, _]) = apply
      def apply = empty
    }

}

import ChainedHashMap.Entry;

class ChainedHashMap[Key, Value](expectedSize : Int) extends scala.collection.mutable.Map[Key, Value]
    with scala.collection.mutable.MapLike[Key, Value, ChainedHashMap[Key, Value]] {

  def this() = this(8)

  private[this] def resizeMultiplier = 2
  private[this] def loadFactor = 0.75

  private def computeCapacity(expectedSize: Int): Int =
    ChainedHashMap.powerOfTwo(Math.ceil(expectedSize / loadFactor).toInt)
 
  //TODO Consider re-using an empty array to start with and resize on first put
  private var table : Array[Entry[Key, Value]] = new Array[Entry[Key, Value]](computeCapacity(expectedSize));
  private var mask: Int = table.length - 1
  
  private var _size = 0;
  
 
  // Used for tracking inserts so that iterators can determine in concurrent modification has occurred.
  private[this] var modCount = 0;

  override def size = _size;
  private[this] def size_=(s : Int) = _size = s;

  private def hashOf(key : Key) = {
    var h = key.hashCode;
    h ^= ((h >>> 20) ^ (h >>> 12));
    h ^ (h >>> 7) ^ (h >>> 4);
  }
  
  private def requireNonNull(key: Key) {
    require(key.asInstanceOf[AnyRef] ne null, "non-null key")
  }

  private[this] def growTable() {
    val oldTable = table;
    val oldLength = oldTable.length;
    val newTable = new Array[Entry[Key, Value]](resizeMultiplier * oldLength);
    val newMask = newTable.length - 1
    var i = 0
    while (i < oldLength) {
      var entry = oldTable(i)
      while (entry ne null) {
        val next = entry.next
        val newIndex = findIndex(entry.hash, newMask)
        entry.next = newTable(newIndex)
        newTable(newIndex) = entry
        entry = next
      }
      i += 1
    }
    mask = newMask
    table = newTable
  }
  
  private def shouldGrowTable: Boolean = _size > (mask + 1) * loadFactor;
  
  private[this] def findIndex(hash: Int): Int = findIndex(hash, mask)

  private def findIndex(hash : Int, mask: Int) : Int = hash & mask

  def +=(kv: (Key, Value)): this.type = {
    update(kv._1, kv._2)
    this
  }
  
  override def update(key : Key, value : Value) {
    requireNonNull(key)
    update(key, hashOf(key), value);
  }
  
  private def update(key : Key, hash : Int, value : Value) {
    val index = findIndex(hash);
    val firstEntry = table(index);
    if (firstEntry eq null) addEntry(index, new Entry(key, hash, value, null))
    else {
      var e = firstEntry;
      while(e ne null){
        if (e.hash == hash && e.key == key){
          e.value = value;
          return;
        }
        e = e.next
      }
      addEntry(index, new Entry(key, hash, value, firstEntry))
    }
  }
  
  private[this] def addEntry(index: Int, entry: Entry[Key, Value]) {
    table(index) = entry
    modCount += 1;
    size += 1;
    if (shouldGrowTable) growTable()
  }
  
  def -=(key : Key): this.type = {
    requireNonNull(key)
    val hash = hashOf(key)
    val index = findIndex(hash);
    var previous = table(index)
    var entry = previous
    while(entry ne null){
      val next = entry.next
      if (entry.hash == hash && entry.key == key) {
        size -= 1
        if (previous == entry) table(index) = next
        else previous.next = next
        return this
      }
      else {
        previous = entry
        entry = next
      }
    }
    this
  }
  
  override def apply(key: Key): Value = {
    requireNonNull(key)
    val hash = hashOf(key)
    var entry = table(findIndex(hash))
    while(entry ne null) {
      if (entry.hash == hash && entry.key == key) return entry.value

      entry = entry.next
    }

    default(key)
  }

  def get(key : Key) : Option[Value] = {
    requireNonNull(key)
    val hash = hashOf(key);
    var entry = table(findIndex(hash));
    while(entry ne null){
      if (entry.hash == hash && entry.key == key) return Some(entry.value);
      
      entry = entry.next
    }
    None;
  }
 
  /**
   * An iterator over the elements of this map. Use of this iterator follows the same
   * contract for concurrent modification as the foreach method.
   */ 
  def iterator: Iterator[(Key, Value)] = new Iterator[(Key, Value)] {
    val it = new EntryIterator
    def hasNext = it.hasNext
    def next = {
      val entry = it.next
      (entry.key, entry.value);
    }
  }

  private class EntryIterator extends Iterator[Entry[Key, Value]] {
    var index = 0;
    var nextEntry: Entry[Key, Value] = _
    val initialModCount = modCount;

    if (ChainedHashMap.this.size > 0) advanceInTable();
    
    private[this] def advanceInTable() {
      while (index < table.length) {
        nextEntry = table(index)
        index += 1
        if (nextEntry ne null) return
      }
    }
    
    private[this] def advance() {
      if (initialModCount != modCount) sys.error("Concurrent modification");
      if (nextEntry eq null) throw new NoSuchElementException()
      
      nextEntry = nextEntry.next
      if (nextEntry eq null) advanceInTable()
    }

    def hasNext = (nextEntry ne null)

    def next() = {
      val result = nextEntry;
      advance;
      result
    }
  }

  override def clone : ChainedHashMap[Key, Value] = {
    val it = new ChainedHashMap[Key, Value]
    foreachEntry(entry => it.update(entry.key, entry.hash, entry.value));
    it
  }
  
  def foreach[U](f: (Key, Value) => U) {
    val startModCount = modCount;
    foreachEntry(entry => {
      if (modCount != startModCount) sys.error("Concurrent Modification")
      f(entry.key, entry.value)
    });
  }

  override def foreach[U](f : ((Key, Value)) => U) {
    val startModCount = modCount;
    foreachEntry(entry => {
      if (modCount != startModCount) sys.error("Concurrent Modification")
      f((entry.key, entry.value))}
    );  
  }
  
  /* Override to avoid tuple allocation in foreach */
  override def keySet: scala.collection.Set[Key] = new DefaultKeySet {
    override def foreach[C](f: Key => C) = foreachEntry(e => f(e.key))
    override def toSeq = toArraySeq(this)
  }
  
  /* Override to avoid tuple allocation in foreach */
  override def values: collection.Iterable[Value] = new DefaultValuesIterable {
    override def foreach[C](f: Value => C) = foreachEntry(e => f(e.value))
    override def toSeq = toArraySeq(this)
  }
  
  /* Override to avoid tuple allocation */
  override def keys = keySet
  
  /* Override to avoid tuple allocation */
  override def keysIterator: Iterator[Key] = new Iterator[Key] {
    val iter = new EntryIterator
    def hasNext = iter.hasNext
    def next = iter.next.key
  }
  
  /* Override to avoid tuple allocation */
  override def valuesIterator: Iterator[Value] = new Iterator[Value] {
    val iter = new EntryIterator
    def hasNext = iter.hasNext
    def next = iter.next.value
  }
  
  private[this] def foreachEntry[U](f : Entry[Key, Value] => U){
    var i = 0
    while (i < table.length) {
      var entry = table(i)
      while(entry ne null){
        f(entry)
        entry = entry.next
      }
      i += 1
    }
  }
  
  override def clear() {
    modCount += 1
    size = 0
    java.util.Arrays.fill(table.asInstanceOf[Array[Object]], null)
  }
  
  override def transform(f : (Key, Value) => Value) = {
    foreachEntry(entry => entry.value = f(entry.key, entry.value));
    this
  }
  

  private def toArraySeq[T](traversable: Traversable[T]): mutable.ArraySeq[T] = {
    val result = new mutable.ArraySeq[T](traversable.size)
    var i = 0
    for (x <- traversable) {
      result(i) = x
      i += 1
    }
    result
  }
  
  override def toSeq: Seq[(Key, Value)] = toArraySeq(this)

  override def empty: ChainedHashMap[Key, Value] = ChainedHashMap.empty

/*
  override def retain(f : (Key, Value) => Boolean) = {
    var i = 0
    while (i < mask) {
      val entry = table(i)
      if (entry != null && if (!f(entry.key, entry.value.get)
    }
  }
*/  
  override def stringPrefix = "ChainedHashMap"
}