package org.specs2.reflect

trait ClassLoading {
  def setContextClassLoader(classLoader: ClassLoader): Unit =
    Thread.currentThread.setContextClassLoader(classLoader)
}
