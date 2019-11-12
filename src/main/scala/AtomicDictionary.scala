package mutable

/** A thread-safe key-value store with atomic update in-place. */
final class AtomicDictionary[K, V] {
  private val chm = new java.util.concurrent.ConcurrentHashMap[K,V]

  def put(k: K, v: V): Option[V] = Option(chm.put(k, v))
  def get(k: K): Option[V] = if (chm.containsKey(k)) Some(chm.get(k)) else None
  def remove(k: K): Option[V] = Option(chm.remove(k)) 

  /** Update k -> v atomically to k -> f(Option(v)). 
    *
    * The old value Some(v) is given as argument to f 
    * If value is absent then the argument to f is None.
    * The new value computed by f is returned wrapped in an Option. 
    * If f returns None then the key is removed. 
    */ 
  def update(k: K)(f: Option[V] => Option[V]): Option[V] = {
    import scala.jdk.FunctionConverters._
    val g: (K, V) => V = (k,v) => f(Option(v)).getOrElse(null.asInstanceOf[V])
    Option(chm.compute(k, g.asJava))
  }

  /** Update all k -> v atomically to k -> f(k, v). */
  def updateAll(f: (K, V) => V): Unit = {
    import scala.jdk.FunctionConverters._
    chm.replaceAll(f.asJava)
  }

  def toMap: Map[K, V] = {
    import scala.jdk.CollectionConverters._
    chm.asScala.toMap
  }

  def size: Int = chm.size

  def clear(): Unit = chm.clear()

  override def toString: String = toMap.toString
}
