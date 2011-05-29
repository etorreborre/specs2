import sbt._

class Plugins(info : ProjectInfo) extends PluginDefinition(info) {
   val scctRepo       = "scct-repo" at   "http://mtkopone.github.com/scct/maven-repo/"
   val snuggletexRepo = "snuggletex_repo" at "http://www2.ph.ed.ac.uk/maven2"
   val tristanRepo    = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"
   val codaRepo       = "Coda Hale's Repository" at "http://repo.codahale.com/"

   val proguard   = "org.scala-tools.sbt" % "sbt-proguard-plugin" % "0.0.5"
   val assembly   = "com.codahale" % "assembly-sbt" % "0.1.1"
   val scctPlugin = "reaktor" % "sbt-scct-for-2.8" % "0.1-SNAPSHOT"
   val posterous  = "net.databinder" % "posterous-sbt" % "0.1.7"
}