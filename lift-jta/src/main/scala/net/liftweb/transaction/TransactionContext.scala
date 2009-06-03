/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.transaction

import javax.persistence.{EntityManager, EntityManagerFactory}
import javax.transaction.TransactionManager

import net.liftweb.util.Log

/**
 * Base monad for the transaction monad implementations.
 */
trait TransactionMonad {
  def map[T](f: TransactionMonad => T): T
    
  def flatMap[T](f: TransactionMonad => T): T

  def foreach(f: TransactionMonad => Unit): Unit

  def setRollbackOnly = TransactionContext.setRollbackOnly

  def getTransactionManager: TransactionManager = TransactionContext.getTransactionManager

  def getEntityManager: EntityManager = TransactionContext.getEntityManager

  def hasEntityManager: Boolean = TransactionContext.hasEntityManager

  def closeEntityManager = TransactionContext.closeEntityManager
}

/**
 * Manages a thread-local stack of TransactionContexts.
 * <p/>
 * Choose TransactionService implementation by implicit definition of the implementation of choice, 
 * e.g. <code>implicit val txService = TransactionServices.AtomikosTransactionService</code>.
 * <p/>
 * Example usage:
 * <pre>
 * for {
 *   ctx <- TransactionContext.Required
 *   entity <- updatedEntities
 *   if (!entity.isValid) ctx.setRollbackOnly
 * } {
 *   // transactional stuff
 *   val em = ctx.getEntityManager
 *   em.merge(entity)
 * }
 * </pre>
 */
object TransactionContext extends TransactionProtocol {
  // FIXME: make configurable
  private implicit val defaultTransactionService = new AtomikosTransactionService
  
  private[TransactionContext] val stack = new scala.util.DynamicVariable(new TransactionContext)
  
  //override def foreach(f: A => Any): Unit = f(value)
  //override def map[T](f: A => B): Box[T] = Full(f(value))
  //override def flatMap[T](f: A => Box[T]): Box[T] = f(value)
  
  object Required extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxRequired { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxRequired { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxRequired { f(this) }
  }
  object RequiresNew extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxRequiresNew { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxRequiresNew { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxRequiresNew { f(this) }
  }
  object Supports extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxSupports { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxSupports { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxSupports { f(this) }
  }
  object Mandatory extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxMandatory { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxMandatory { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxMandatory { f(this) }
  }
  object Never extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxNever { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxNever { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxNever { f(this) }
  }
  
  /**
   * Marks the current transaction as doomed.
   */
  def setRollbackOnly = current.setRollbackOnly
  
  /**
   * Returns the current TransactionManager.
   */
  def getTransactionManager: TransactionManager = current.getTransactionManager

  /**
   * Returns the current EntityManager.
   */
  def getEntityManager: EntityManager = current.getEntityManager

  /**
   * Checks if an EntityManager exists in current context.
   */
  def hasEntityManager: Boolean = current.em.isDefined

  /**
   * Closes and removes the current EntityManager.
   * <p/>
   * NOTE: This method must always be used to close the EntityManager, never use em.close directly.
   */
  def closeEntityManager = current.closeEntityManager

  /**
   * Returns the current context.
   */
  private def current = stack.value

  /**
   * Continues with the invocation defined in 'body' with the brand new context define in 'newCtx', the old
   * one is put on the stack and will automatically come back in scope when the method exits.
   */
  private[liftweb] def withContext[T](body: => T): T = stack.withValue(new TransactionContext) { body }
}

/**
 * Transaction context, holds the EntityManager and the TransactionManager.
 */
class TransactionContext(private implicit val transactionService: TransactionService) { 
  private var em: Option[EntityManager] = None
  private val tm: TransactionManager = transactionService.getTransactionManager

  /**
   * Marks the current transaction as doomed.
   */
  private def setRollbackOnly = tm.setRollbackOnly

  /**
   * Returns the current TransactionManager.
   */
  private def getTransactionManager: TransactionManager = tm

  /**
   * Returns the current EntityManager.
   */
   private def getEntityManager: EntityManager = em match {
     case Some(entityManager) => entityManager
     case None => 
       val entityManager = transactionService.getEntityManagerFactory.createEntityManager
       em = Some(entityManager)
       entityManager
   }

  /**
   * Closes and removes the current EntityManager.
   * <p/>
   * PLEASE NOTE: This method must always be used to close the EntityManager, never use em.close directly.
   */
  private def closeEntityManager = if (em.isDefined) {
    em.get.close
    em = None
  }
}


