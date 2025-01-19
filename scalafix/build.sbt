lazy val V = _root_.scalafix.sbt.BuildInfo

inThisBuild(
  List(
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    organization := "com.spotify",
    scalaVersion := V.scala212,
    scalacOptions ++= List("-Yrangepos"),
    publish / skip := true,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbIncludeInJar := true,
    scalafmtOnCompile := false,
    scalafmtConfig := baseDirectory.value / ".." / ".scalafmt.conf"
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(
    tests.projectRefs ++ Seq[sbt.ProjectReference](
      // 0.7
      `input-0_7`,
      `output-0_7`,
      // scalafix
      rules
    ): _*
  )

lazy val rules = project
  .settings(
    moduleName := "scalafix",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion
    )
  )

def magnolify(version: String): List[ModuleID] = {
  val modules = List(
    "magnolify-avro",
    "magnolify-bigquery",
    "magnolify-bigtable",
    "magnolify-cats",
    "magnolify-datastore",
    "magnolify-guava",
    "magnolify-neo4j",
    "magnolify-parquet",
    "magnolify-protobuf",
    "magnolify-refined",
    "magnolify-shared",
    "magnolify-scalacheck",
    "magnolify-tensorflow"
  )

  modules.map(name => "com.spotify" %% name % version % "compile->compile,provided")
}

// coursied does not respect compile->compile,provided
ThisBuild / useCoursier := false

lazy val `input-0_7` = project
  .settings(
    libraryDependencies ++= magnolify("0.6.0")
  )

lazy val `output-0_7` = project
  .settings(
    libraryDependencies ++= magnolify("0.7.0")
  )

lazy val magnolify0_7 = ConfigAxis("-magnolify-0_7", "-0_7-")

lazy val tests = projectMatrix
  .in(file("tests"))
  .enablePlugins(ScalafixTestkitPlugin)
  .customRow(
    scalaVersions = Seq(V.scala212),
    axisValues = Seq(magnolify0_7, VirtualAxis.jvm),
    _.settings(
      moduleName := name.value + magnolify0_7.idSuffix,
      scalafixTestkitOutputSourceDirectories := (`output-0_7` / Compile / unmanagedSourceDirectories).value,
      scalafixTestkitInputSourceDirectories := (`input-0_7` / Compile / unmanagedSourceDirectories).value,
      scalafixTestkitInputClasspath := (`input-0_7` / Compile / fullClasspath).value
    ).dependsOn(rules)
  )
