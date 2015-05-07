package org.specs2
package foldm
package effect

import java.io._

import org.specs2.foldm.FoldId.Bytes

import scalaz.effect.IO
import FoldM._

object FoldIO {

  // unfortunately this type alias is necessary to have a proper type inference for
  // applicative operators like <*
  type FoldIO[A, B] = FoldM[A, IO, B]

  type Sink[A] = FoldIO[A, Unit]

  /** @return an output stream sink */
  def outputStreamSink[T](out: OutputStream)(write: (OutputStream, T) => Unit): Sink[T] =
    sinkIO[T, OutputStream](IO(out))(write)(closeStream)

  /** @return an output file stream sink */
  def fileSink[T](file: File)(write: (OutputStream, T) => Unit): Sink[T] =
    fileSink(file.getPath)(write)

  /** @return an output file stream sink */
  def fileSink[T](file: String)(write: (OutputStream, T) => Unit): Sink[T] =
    sinkIO[T, OutputStream](createOutputStream(file))(write)(closeStream)

  /** @return an output stream sink for string lines */
  def fileUTF8LineSink(file: File): Sink[String] =
    fileUTF8LineSink(file.getPath)

  /** @return an output stream sink for string lines */
  def fileUTF8LineSink(file: String): Sink[String] =
    sinkIO[String, PrintStream](createPrintStream(file))((s, t) => s.println(t))(closeStream)

  /** @return an output stream sink for string lines */
  def bytesSink[T](file: File): Sink[Bytes] =
    bytesSink(file.getPath)

  /** @return an output stream sink for string lines */
  def bytesSink[T](file: String): Sink[Bytes] =
    sinkIO[Bytes, OutputStream](createOutputStream(file))((s, t) => s.write(t._1, 0, t._2))(closeStream)

  def sinkIO[T, R](init: IO[R])(fd: (R, T) => Unit)(close: R => IO[Unit]): Sink[T] =
    sink[T, R, IO](init)(fd)(close)

  def sink[T, R, M[_]](init: M[R])(fd: (R, T) => Unit)(close: R => M[Unit]): SinkM[T, M] = new FoldM[T, M, Unit] {
    type S = R
    def start = init
    def fold = (s: S, t: T) => { fd(s, t); s }
    def end(s: S) = close(s)
  }

  def createOutputStream(file: String): IO[OutputStream] = IO {
    val f = new File(file)
    if (f.getParentFile != null) f.getParentFile.mkdirs
    new BufferedOutputStream(new FileOutputStream(f))
  }

  def createPrintStream(file: String): IO[PrintStream] = IO {
    val f = new File(file)
    if (f.getParentFile != null) f.getParentFile.mkdirs
    new PrintStream(f, "UTF-8")
  }

  def closeStream[S <: OutputStream]: S => IO[Unit] =
    (s: S) => IO(s.close)

}
