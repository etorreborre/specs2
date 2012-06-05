resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")

addSbtPlugin("com.jsuereth" % "sbt-site-plugin" % "0.4.0")

resolvers ++= Seq("less is" at "http://repo.lessis.me", "coda" at "http://repo.codahale.com")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.1")



resolvers ++= Seq(
  "less is" at "http://repo.lessis.me",
  "coda" at "http://repo.codahale.com")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.1")
