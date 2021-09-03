package org.specs2.runner

import collection.convert.ImplicitConversions.*

import org.junit.platform.engine.*
import org.junit.platform.engine.discovery.*
import org.junit.platform.engine.support.descriptor.*
import org.junit.platform.engine.support.hierarchical.*
import java.util.concurrent.atomic.AtomicInteger
import java.lang.reflect.Modifier
import java.util.Optional
import java.util.stream.Collectors
import scala.collection.JavaConverters.*
import org.specs2.concurrent.*
import org.specs2.control.*
import org.specs2.execute.*
import org.junit.platform.engine.TestDescriptor.Type
import org.specs2.data.Trees.*
import org.specs2.fp.*
import org.specs2.fp.Tree.*
import org.specs2.main.*
import org.specs2.specification.core.*
import org.specs2.specification.create.*
import org.specs2.specification.process.*
import org.specs2.text.Trim.*

class Specs2TestEngine extends TestEngine:
  /** command line arguments, extracted from the system properties */
  lazy val arguments: Arguments =
    Arguments("junit")

  /** specification environment */
  lazy val env: Env =
    EnvDefault.create(arguments)

  /** Discover tests according to the supplied EngineDiscoveryRequest. */
  def discover(discoveryRequest: EngineDiscoveryRequest, uniqueId: UniqueId): TestDescriptor =
    val engineDescriptor = new Specs2EngineContainerDescriptor(uniqueId, getId)
    val specifications =
      discoveryRequest
        .getSelectorsByType(classOf[ClassSelector])
        .asScala
        .map(_.getJavaClass)
        .filter(klass =>
          !(klass.getEnclosingMethod != null //only local or anonymous classes have an enclosing method
            || klass.isSynthetic
            || Modifier.isAbstract(klass.getModifiers))
            && classOf[SpecificationStructure].isAssignableFrom(klass)
        )
        .map(klass =>
          SpecificationStructure.create(klass.getName, Thread.currentThread.getContextClassLoader, Some(env)).unsafeRun
        )

    Specs2EngineDescriptor.create(uniqueId, "specs2")(specifications.toList)(env.specs2ExecutionEnv)

  def getId(): String =
    "org.specs2.runner.Specs2TestEngine"

  def execute(request: ExecutionRequest): Unit =
    val engineDescriptor = request.getRootTestDescriptor
    val listener = request.getEngineExecutionListener
    executeTest(engineDescriptor, listener)

  def executeTest(descriptor: TestDescriptor, listener: EngineExecutionListener): Unit =
    listener.executionStarted(descriptor)
    descriptor match
      case Specs2EngineContainerDescriptor(_, _) =>
        descriptor.getChildren.foreach(d => executeTest(d, listener))
        listener.executionFinished(descriptor, TestExecutionResult.successful)
      case Specs2EngineTestDescriptor(_, _, execution) =>
        execution.startExecution(env).executionResult.runAction(env.executionEnv) match
          case Right(e: Error) =>
            listener.executionFinished(descriptor, TestExecutionResult.failed(ErrorException(e)))
          case Right(f: Failure) =>
            listener.executionFinished(descriptor, TestExecutionResult.failed(FailureException(f)))
          case Right(_) =>
            listener.executionFinished(descriptor, TestExecutionResult.successful)
          case Left(e) =>
            listener.executionFinished(descriptor, TestExecutionResult.failed(e))

case class Specs2EngineContainerDescriptor(uniqueId: UniqueId, name: String) extends EngineDescriptor(uniqueId, name) {
  override def getType = Type.CONTAINER
}

case class Specs2EngineTestDescriptor(uniqueId: UniqueId, name: String, execution: Execution)
    extends EngineDescriptor(uniqueId, name) {
  override def getType = Type.TEST
}

object Specs2EngineDescriptor:

  def create(engineId: UniqueId, name: String)(specificationStructures: List[SpecificationStructure])(
      ee: ExecutionEnv
  ): EngineDescriptor =
    val descriptor = Specs2EngineContainerDescriptor(engineId, name)
    specificationStructures.map(s => createSpecDescriptor(engineId, s.structure)(ee)).foreach(descriptor.addChild)
    descriptor

  def createSpecDescriptor(engineId: UniqueId, spec: SpecStructure)(ee: ExecutionEnv): TestDescriptor =
    createTestDescriptor(createTreeLoc(engineId, spec)(ee))

  def createTestDescriptor(treeLoc: TreeLoc[TestDescriptor]): TestDescriptor =
    treeLoc.toTree.bottomUp { (descriptor: TestDescriptor, children: LazyList[TestDescriptor]) =>
      children.foreach(descriptor.addChild)
      descriptor
    }.rootLabel

  def createTreeLoc(uniqueId: UniqueId, spec: SpecStructure)(ee: ExecutionEnv): TreeLoc[TestDescriptor] =
    createTestDescriptorTree(uniqueId, spec)(ee).map(_._2)

  def createTestDescriptorTree(uniqueId: UniqueId, spec: SpecStructure)(
      ee: ExecutionEnv
  ): TreeLoc[(Fragment, TestDescriptor)] =
    val className = spec.specClassName
    val rootFragment = DefaultFragmentFactory.text(spec.header.simpleName)

    Levels
      .treeLocMap(spec.fragments)(keep)(ee)
      .getOrElse(Leaf(rootFragment).loc)
      .root
      .setLabel(rootFragment)
      .cojoin
      .map { (current: TreeLoc[Fragment]) =>
        val descriptor =
          current.getLabel match
            case f @ Fragment(d, e, _) if !e.isExecutable =>
              createContainerDescriptor(uniqueId, d.show)
            case f @ Fragment(NoText, e, _) if e.mustJoin =>
              createTestDescriptor(uniqueId, "step", e)
            case f @ Fragment(NoText, e, _) =>
              createTestDescriptor(uniqueId, "action", e)
            case f @ Fragment(d, e, _) =>
              createTestDescriptor(uniqueId, d.show, e)
        (current.getLabel, descriptor)
      }

  def createTestDescriptor(uniqueId: UniqueId, description: String, execution: Execution): TestDescriptor =
    Specs2EngineTestDescriptor(uniqueId.append("test", UniqueIds.get.toString), description, execution)

  def createContainerDescriptor(uniqueId: UniqueId, description: String): TestDescriptor =
    Specs2EngineContainerDescriptor(uniqueId.append("suite", UniqueIds.get.toString), description)

  /** filter out the fragments which don't need to be represented in the JUnit descriptions */
  def keep: Levels.Mapper =
    case f @ Fragment(Text(t), e, _) if t.trim.isEmpty => None
    case f if Fragment.isFormatting(f)                 => None
    case f                                             => Some(f)

// This object provides unique ids to number tests
object UniqueIds:
  private val current: AtomicInteger = new AtomicInteger(0)

  def get: Int =
    current.incrementAndGet()
