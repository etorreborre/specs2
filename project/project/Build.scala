import sbt._
object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn(ghpages)
  lazy val ghpages = uri("git://github.com/jsuereth/xsbt-ghpages-plugin.git")
}