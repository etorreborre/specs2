import sbt._
import reaktor.scct.ScctProject

class Project(info: ProjectInfo) extends DefaultProject(info) with ScctProject with posterous.Publish with ProguardProject {
  
  /** Paths */
  override def outputDirectoryName = "target"
  override def managedDependencyPath = "project" / "lib_managed"

  /** Dependencies */
  val mavenLocal = "Local Maven Repository" at "file://D:/mvn-repository"
  val snapshotsRepo = "snapshots-repo" at "http://scala-tools.org/repo-snapshots"

  val scalacheck    = "org.scala-tools.testing" %% "scalacheck" % "1.8" 
  val testinterface = "org.scala-tools.testing" % "test-interface" % "0.5" 
  val scalazcore    = "com.googlecode.scalaz" %% "scalaz-core" % "5.1-SNAPSHOT"
  val hamcrest      = "org.hamcrest" % "hamcrest-all" % "1.1"
  val mockito 	    = "org.mockito" % "mockito-all" % "1.8.5" 
  val junit         = "junit" % "junit" % "4.7"
  val pegdown       = "org.pegdown" % "pegdown" % "0.9.1"
  
  /** Compiling */
  override def compileOptions = Unchecked :: super.compileOptions.toList
  override def javaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m -Xss4m") :: Nil

  /** Testing */
  override def testFrameworks = super.testFrameworks ++ Seq(new TestFramework("org.specs2.runner.SpecsFramework"))

  override def testJavaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m -Xss4m") :: Nil
  override def includeTest(s: String) = Seq("Spec", "Suite", "Unit", "all").exists(s.endsWith(_)) && !s.endsWith("FeaturesSpec") || 
	                                            s.contains("UserGuide") || 
	                                            s.matches("org.specs2.guide.*") 

  /** Documenting  - hack for creating the doc with 2.9.0 because 2.8.1 throws an Exception */
  override protected def docAction = scaladocTask(mainLabel, mainSources, mainDocPath, docClasspath, documentOptions)
  

  /** Packaging */
	/** Proguard */
  override def proguardOptions = List("-dontshrink -dontobfuscate -dontpreverify")
	override def proguardInJars = { 
		println((super.proguardInJars +++ scalaLibraryPath) filter (_.name.contains("scalaz")))
		(super.proguardInJars +++ scalaLibraryPath)  filter (_.name.contains("scalaz"))
  }
	
	/** Sources */
  override def packageSrcJar = defaultJarPath("-sources.jar")
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc, proguard)
  override def artifacts = super.artifacts ++ Seq(Artifact("specs2_"+buildScalaVersion, "jar", ".min.jar"))
  
  /** Publishing */
	override def ivyXML =
    <dependencies>
      <exclude module={"scalaz-core_"+buildScalaVersion}/>
    </dependencies>
	
	override def pomPostProcess(pom: Node): Node = super.pomPostProcess(pom)
			
  override def managedStyle = ManagedStyle.Maven
  override def defaultPublishRepository = {
    val nexusDirect = "http://nexus-direct.scala-tools.org/content/repositories/"
    if (version.toString.endsWith("SNAPSHOT")) 
	  Some("scala-tools snapshots" at nexusDirect + "snapshots/")
	else
	  Some("scala-tools releases" at nexusDirect + "releases/")
  }
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

}