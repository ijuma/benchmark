resolvers ++= Seq(
  "sbt-idea-repo" at "http://mpeltonen.github.com/maven/",
  Classpaths.typesafeResolver
)

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse" % "1.5.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

