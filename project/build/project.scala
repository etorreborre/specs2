import sbt._
import reaktor.scct.ScctProject

class Project(info: ProjectInfo) extends DefaultProject(info) with ScctProject with posterous.Publish with ProguardProject {
  
  /** Paths */
  override def outputDirectoryName = "target"
  override def managedDependencyPath = "project" / "lib_managed"

  /** Dependencies */
  val mavenLocal = "Local Maven Repository" at "file://D:/mvn-repository"
  val snapshotsRepo = "snapshots-repo" at "http://scala-tools.org/repo-snapshots"

  val scalacheck    = "org.scala-tools.testing" % "scalacheck_2.9.0-SNAPSHOT" % "1.9-SNAPSHOT" 
  val testinterface = "org.scala-tools.testing" % "test-interface" % "0.5" 
  val scalazcore    = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT" % "optional"
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
	// the published jar will contain scalaz classes so the "standard" one needs to be renamed
  override def jarPath = defaultJarPath("-noscalaz.jar")
    /** Proguard */
	// the proguard jar name will have the "standard" jar name
	override def minJarName = super.artifactBaseName + ".jar"
  override def proguardOptions = List("-dontshrink -dontobfuscate -dontpreverify")

	// add only the dependencies having scalaz in their name, to retain only the scalaz jar
  override def proguardInJars = (super.proguardInJars +++ scalaLibraryPath) filter (_.name.contains("scalaz"))
	
    /** Sources */
  val sourceArtifact = Artifact.sources(artifactID)
  override def packageSrcJar = defaultJarPath("-sources.jar")
	// before publishing, package the sources and create the specs2 jar including the scalaz classes
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc, proguard)
  
  /** Publishing */
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