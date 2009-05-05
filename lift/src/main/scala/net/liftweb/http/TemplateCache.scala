package net.liftweb.http

import _root_.net.liftweb.util.{Box, Full, Empty, LRU, ConcurrentLock}
import _root_.scala.xml.{NodeSeq}
import java.util.{Locale}

trait TemplateCache[K] {

  /**
   * Returns a cached template by a key. If the template is not cached yet,
   * it will be provided by templateProvider.
   */
  def findTemplate(key: K): Box[NodeSeq];

  /**
   * Adds the node in the cache
   */
  def cacheTemplate(key: K, node: NodeSeq): NodeSeq
  
  /**
   * Removes a template from the cache
   */
  def removeTemplate(key: K);
}

/**
 * A cache that caches nothing
 */
object NoCache extends TemplateCache[(Locale, List[String])] {

  def findTemplate(key: (Locale, List[String])): Box[NodeSeq] = Empty
  
  def cacheTemplate(key: (Locale, List[String]), node: NodeSeq): NodeSeq = node

  override def removeTemplate(key: (Locale, List[String])) {
  }
}

/**
 * Companion module for InMemoryCache
 */
object InMemoryCache {
  def apply(templatesCount: Int) = new InMemoryCache(templatesCount)
}

/**
 * Caches templates in a LRU map
 */
private[http] class InMemoryCache(templatesCount: Int) extends TemplateCache[(Locale, List[String])] {

  private val cache : LRU[(Locale, List[String]), NodeSeq] = new LRU(templatesCount)
  private val cacheLock = new ConcurrentLock
  
  def findTemplate(key: (Locale, List[String])): Box[NodeSeq] = {
    cacheLock.read {
     cache.get(key)
    }
  }

  def cacheTemplate(key: (Locale, List[String]), node: NodeSeq): NodeSeq = cacheLock.write {
    cache(key) = node
    node
  }

  override def removeTemplate(key: (Locale, List[String])) {
    cache.remove(key)
  }

}
