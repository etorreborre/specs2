package org.specs2.reflect

case class ClassLoading():
  def setContextClassLoader(classLoader: ClassLoader): Unit =
    Thread.currentThread.setContextClassLoader(classLoader)
