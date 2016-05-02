package scalaz.stream

import scalaz.concurrent.{Strategy, Task}
import scalaz.stream.Process.halt
import scalaz.stream.async.mutable._

import scalaz.\/._

package object async {

  /**
   * Creates a bounded queue that is bound by supplied max size bound.
   * Please see [[scalaz.stream.async.mutable.Queue]] for more details.
   * @param max The maximum size of the queue (must be > 0)
   * @param recover Flag controlling automatic dequeue error recovery semantics.  When
   * false (the default), data may be lost in the event of an error during dequeue.
   * When true, data will never be lost on dequeue, but concurrent dequeue processes
   * may see data out of order under certain error conditions.
   */
  def boundedQueue[A](max: Int, recover: Boolean = false)(implicit S: Strategy): Queue[A] = {
    if (max <= 0)
      throw new IllegalArgumentException(s"queue bound must be greater than zero (got $max)")
    else
      Queue[A](max, recover)
  }

  /**
   * Creates an unbounded queue. see [[scalaz.stream.async.mutable.Queue]] for more
   */
  def unboundedQueue[A](implicit S: Strategy): Queue[A] = Queue[A](0)

  def unboundedQueue[A](recover: Boolean)(implicit S: Strategy): Queue[A] = Queue[A](0, recover)

  /**
   * Builds a queue that functions as a circular buffer. Up to `size` elements of
   * type `A` will accumulate on the queue and then it will begin overwriting
   * the oldest elements. Thus an enqueue process will never wait.
   * @param size The size of the circular buffer (must be > 0)
   */
  def circularBuffer[A](size: Int)(implicit S: Strategy): Queue[A] = CircularBuffer[A](size)

  /**
   * Create a new continuous signal which may be controlled asynchronously.
   * Note that this would block any resulting processes (discrete, continuous) until any signal value is set.
   */
  @deprecated("Use signalOf or signalUnset instead","0.7.0")
  def signal[A](implicit S: Strategy): Signal[A] = signalUnset

  /**
   * Create a new continuous signal which may be controlled asynchronously.
   * Note that this would block any resulting processes (discrete, continuous) until any signal value is set.
   */
  def signalUnset[A](implicit S: Strategy): Signal[A] =
    Signal(left(None))

  /**
   * Creates a new continuous signal which may be controlled asynchronously,
   * and immediately sets the value to `initialValue`.
   */
  def signalOf[A](initialValue: A)(implicit S: Strategy): Signal[A] =
    Signal(left(Some(initialValue)))


  /**
   * Converts discrete process to signal.
   * Note that resulting signal will terminate as soon as source terminates,
   * propagating reason for the termination to all `downstream` processes
   * failure
   * @param source          discrete process publishing values to this signal
   */
  def toSignal[A](source: Process[Task, A])(implicit S: Strategy): immutable.Signal[A] =
    Signal(right(source.map(Signal.Set(_))))

}
