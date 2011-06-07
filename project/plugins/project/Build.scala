import sbt._
object PluginDef extends Build {
  override lazy val projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn( posterous )
  lazy val posterous = uri("https://github.com/n8han/posterous-sbt")
}