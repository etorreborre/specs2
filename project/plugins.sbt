

resolvers ++= Seq(
  "less is" at "http://repo.lessis.me",
  "coda" at "http://repo.codahale.com")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.1")

resolvers += "gseitz@github.com" at "http://gseitz.github.com/maven/"

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.4")
