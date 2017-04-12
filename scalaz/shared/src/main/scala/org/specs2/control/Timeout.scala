package org.specs2
package control

import scala.annotation.tailrec
import java.util.concurrent.locks.ReentrantReadWriteLock
import scalaz.concurrent._
import scalaz._, Scalaz._

/**
 * This code is taken from Scalaz to provide time-out functionality for Futures
 *
 * It is extended with start/stop methods to better control the usage of thread resources
 */
private[specs2]
case class Timeout(timeoutTickMs: Int = 100, workerName: String = "TimeoutContextWorker") {
  val safeTickMs = if (timeoutTickMs > 0) timeoutTickMs else 1
  private[this] val nondeterminism = Nondeterminism[Future]
  @volatile private[this] var continueRunning: Boolean = true
  @volatile private[this] var lastNow: Long = System.currentTimeMillis
  private[this] val lock = new ReentrantReadWriteLock()
  private[this] var futures: Vector[(Long, () => Unit)] = Vector.empty

  private[this] lazy val workerRunnable: Runnable = new Runnable() {
    def run() {
      @tailrec
      def innerRun() {
        // Deal with stuff to expire.
        lastNow = System.currentTimeMillis
        futures.headOption match {
          case Some((time, _)) if time < lastNow =>
            val expiredFutures: Vector[(Long, () => Unit)] = withWrite{
              val (past, future) = futures.span(pair => pair._1 < lastNow)
              futures = future
              past
            }
            expiredFutures.foreach(call => call._2())
          case _ => ()
        }
        // Should we keep running?
        if (continueRunning) {
          Thread.sleep(safeTickMs.toLong)
          innerRun()
        } else workerThread.interrupt
      }
      innerRun()
    }
  }

  private[this] lazy val workerThread = new Thread(workerRunnable, workerName)

  def start = {
    workerThread.start()
    this
  }

  def stop() {
    continueRunning = false
  }

  private[this] def withWrite[T](expression: => T): T = {
    lock.writeLock().lock()
    try     expression
    finally lock.writeLock().unlock()
  }

  def valueWait[T](value: T, waitMs: Long): Future[T] = {
    val listen: (T => Unit) => Unit = callback => withWrite {
      val waitTime = lastNow + (if (waitMs < 0) 0 else waitMs)
      // Lazy implementation for now.
      val timedCallback = () => callback(value)
      futures = (futures :+ ((waitTime, timedCallback))).sortBy(_._1)
    }
    Future.async[T](listen)
  }

  def withTimeout[T](future: Future[T], timeout: Long): Future[SomeTimeout \/ T] = {
    val timeoutFuture = valueWait(SomeTimeout, timeout)
    nondeterminism.choose(timeoutFuture, future).map(_.fold(_._1.left, _._2.right))
  }
}

trait SomeTimeout
object SomeTimeout extends SomeTimeout
