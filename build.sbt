/*
 * Copyright 2019 Spotify AB
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import sbt._
import sbt.util.CacheImplicits._
import sbtprotoc.ProtocPlugin.ProtobufConfig
import com.github.sbt.git.SbtGit.GitKeys.gitRemoteRepo
import com.typesafe.tools.mima.core._

val magnoliaScala2Version = "1.1.10"
val magnoliaScala3Version = "1.3.8"

val algebirdVersion = "0.13.10"
val avroVersion = Option(sys.props("avro.version")).getOrElse("1.11.3")
val beamVersion = "2.61.0"
val bigqueryVersion = "v2-rev20241013-2.0.0"
val bigtableVersion = "2.51.1"
val catsVersion = "2.12.0"
val datastoreVersion = "2.25.2"
val guavaVersion = "33.4.0-jre"
val hadoopVersion = "3.4.1"
val jacksonVersion = "2.18.2"
val jodaTimeVersion = "2.13.0"
val munitVersion = "1.0.4"
val munitScalacheckVersion = "1.0.0"
val neo4jDriverVersion = "4.4.19"
val paigesVersion = "0.4.4"
val parquetVersion = "1.15.0"
val protobufVersion = "3.25.5"
val refinedVersion = "0.11.3"
val scalaCollectionCompatVersion = "2.12.0"
val scalacheckVersion = "1.18.1"
val shapelessVersion = "2.3.12"
val slf4jVersion = "2.0.16"
val tensorflowMetadataVersion = "1.16.1"
val tensorflowVersion = "0.5.0"

// project
ThisBuild / tlBaseVersion := "0.7"
ThisBuild / organization := "com.spotify"
ThisBuild / organizationName := "Spotify AB"
ThisBuild / startYear := Some(2016)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / sonatypeCredentialHost := Sonatype.sonatypeLegacy
ThisBuild / developers := List(
  Developer(
    id = "sinisa_lyh",
    name = "Neville Li",
    email = "neville.lyh@gmail.com",
    url = url("https://twitter.com/sinisa_lyh")
  ),
  Developer(
    id = "andrewsmartin",
    name = "Andrew Martin",
    email = "andrewsmartin.mg@gmail.com",
    url = url("https://twitter.com/andrew_martin92")
  ),
  Developer(
    id = "daikeshi",
    name = "Keshi Dai",
    email = "keshi.dai@gmail.com",
    url = url("https://twitter.com/daikeshi")
  ),
  Developer(
    id = "clairemcginty",
    name = "Claire McGinty",
    email = "clairem@spotify.com",
    url = url("http://github.com/clairemcginty")
  ),
  Developer(
    id = "anne-decusatis",
    name = "Anne DeCusatis",
    email = "anned@spotify.com",
    url = url("http://twitter.com/precisememory")
  ),
  Developer(
    id = "stormy-ua",
    name = "Kirill Panarin",
    email = "kirill.panarin@gmail.com",
    url = url("https://twitter.com/panarin_kirill")
  ),
  Developer(
    id = "syodage",
    name = "Shameera Rathnayaka Yodage",
    email = "shameerayodage@gmail.com",
    url = url("https://twitter.com/syodage")
  ),
  Developer(
    id = "shnapz",
    name = "Andrew Kabas",
    email = "akabas@spotify.com",
    url = url("https://github.com/shnapz")
  )
)

// scala versions
val scala3 = "3.3.4"
val scala213 = "2.13.16"
val scala212 = "2.12.20"
val scalaDefault = scala213
val scala3Projects = List(
  "shared",
  "test"
)

// github actions
val java17 = JavaSpec.corretto("17")
val java11 = JavaSpec.corretto("11")
val javaDefault = java17

val condIsScala3 = "matrix.scala == '3'"
val condNotScala3 = s"!($condIsScala3)"
val condIsMain = "github.ref == 'refs/heads/main'"
val condIsTag = "startsWith(github.ref, 'refs/tags/v')"

ThisBuild / scalaVersion := scalaDefault
ThisBuild / crossScalaVersions := Seq(scala3, scala213, scala212)
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowJavaVersions := Seq(java17, java11)
ThisBuild / tlJdkRelease := Some(8)
ThisBuild / tlFatalWarnings := true
ThisBuild / tlCiHeaderCheck := true
ThisBuild / tlCiScalafmtCheck := true
ThisBuild / tlCiDocCheck := true
ThisBuild / tlCiMimaBinaryIssueCheck := true
ThisBuild / githubWorkflowBuild ~= { steps: Seq[WorkflowStep] =>
  steps.flatMap {
    case s if s.name.contains("Test") =>
      Seq(
        s.withCond(Some(condNotScala3)),
        WorkflowStep.Sbt(
          scala3Projects.map(p => s"$p/test"),
          name = Some("Test"),
          cond = Some(condIsScala3)
        )
      )
    case s =>
      if (
        s.name.contains("Check binary compatibility") ||
        s.name.contains("Generate API documentation")
      ) {
        Seq(s.withCond(Some(condNotScala3)))
      } else {
        Seq(s)
      }
  }
}
ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    "coverage",
    "Test coverage",
    WorkflowStep.CheckoutFull ::
      WorkflowStep.SetupJava(List(javaDefault)) :::
      List(
        WorkflowStep.Sbt(
          List("coverage", "test", "coverageAggregate"),
          name = Some("Test coverage")
        ),
        WorkflowStep.Use(
          UseRef.Public("codecov", "codecov-action", "v5"),
          Map("token" -> "${{ secrets.CODECOV_TOKEN }}"),
          name = Some("Upload coverage report")
        )
      ),
    scalas = List(CrossVersion.binaryScalaVersion(scalaDefault)),
    javas = List(javaDefault)
  ),
  WorkflowJob(
    "avro-legacy",
    "Test with legacy avro",
    WorkflowStep.CheckoutFull ::
      WorkflowStep.SetupJava(List(javaDefault)) :::
      List(
        WorkflowStep.Sbt(
          List("avro/test"),
          env = Map("JAVA_OPTS" -> "-Davro.version=1.8.2"),
          name = Some("Test")
        )
      ),
    scalas = List(CrossVersion.binaryScalaVersion(scalaDefault)),
    javas = List(javaDefault)
  ),
  WorkflowJob(
    "site",
    "Generate Site",
    WorkflowStep.CheckoutFull ::
      WorkflowStep.SetupJava(List(javaDefault)) :::
      List(
        WorkflowStep.Sbt(
          List("site/makeSite"),
          name = Some("Generate site")
        ),
        WorkflowStep.Use(
          UseRef.Public("peaceiris", "actions-gh-pages", "v3.9.3"),
          params = Map(
            "github_token" -> "${{ secrets.GITHUB_TOKEN }}",
            "publish_dir" -> {
              val path = (ThisBuild / baseDirectory).value.toPath.toAbsolutePath
                .relativize((site / makeSite / target).value.toPath)
              // os-independent path rendering ...
              (0 until path.getNameCount).map(path.getName).mkString("/")
            },
            "keep_files" -> "true"
          ),
          name = Some("Publish site"),
          cond = Some(condIsTag)
        )
      ),
    scalas = List(CrossVersion.binaryScalaVersion(scalaDefault)),
    javas = List(javaDefault)
  )
)

// mima
ThisBuild / mimaBinaryIssueFilters ++= Seq(
  // genFunnelMacro should not be available to users
  ProblemFilters.exclude[DirectMissingMethodProblem]("magnolify.guava.auto.package.genFunnelMacro"),
  // incorrectly named implicit
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "magnolify.parquet.logical.package#micros.pfTimestampMillis"
  ),
  // incorrectly named implicit
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "magnolify.parquet.logical.package#micros.pfLocalDateTimeMillis"
  ),
  // incorrectly named implicit
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "magnolify.parquet.logical.package#nanos.pfTimestampMillis"
  ),
  // incorrectly named implicit
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "magnolify.parquet.logical.package#nanos.pfLocalDateTimeMillis"
  )
)
ThisBuild / tlVersionIntroduced := Map("3" -> "0.8.0")

// protobuf
val protocJavaSourceManaged =
  settingKey[File]("Default directory for java sources generated by protoc.")
ThisBuild / PB.protocVersion := protobufVersion
lazy val scopedProtobufSettings = Def.settings(
  PB.targets := Seq(
    PB.gens.java(protobufVersion) -> Defaults.configSrcSub(protocJavaSourceManaged).value
  ),
  managedSourceDirectories ++= PB.targets.value.map(_.outputPath)
)
lazy val protobufSettings = Seq(
  protocJavaSourceManaged := sourceManaged.value / "compiled_proto",
  PB.additionalDependencies := Seq(
    "com.google.protobuf" % "protobuf-java" % protobufVersion % Provided,
    "com.google.protobuf" % "protobuf-java" % protobufVersion % Test
  )
) ++ Seq(Compile, Test).flatMap(c => inConfig(c)(scopedProtobufSettings))

lazy val currentYear = java.time.LocalDate.now().getYear
lazy val keepExistingHeader =
  HeaderCommentStyle.cStyleBlockComment.copy(commentCreator =
    (text: String, existingText: Option[String]) =>
      existingText
        .getOrElse(HeaderCommentStyle.cStyleBlockComment.commentCreator(text))
        .trim()
  )

val commonSettings = Seq(
  // So far most projects do no support scala 3
  crossScalaVersions := Seq(scala213, scala212),
  // skip scala 3 publishing until ready
  publish / skip := (publish / skip).value || (scalaVersion.value == scala3),
  scalaVersion := scalaDefault,
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq(
        // required by magnolia for accessing default values
        "-Yretain-trees",
        // tolerate some nested macro expansion
        "-Xmax-inlines",
        "64"
      )
    case Some((2, 13)) =>
      Seq(
        "-Wmacros:after",
        // silence warnings
        "-Wconf:cat=unused-imports&origin=scala\\.collection\\.compat\\..*:s" +
          ",cat=unused-imports&origin=magnolify\\.shims\\..*:s" +
          ",cat=unused-imports&origin=magnolify\\.scalacheck\\.MoreCollectionsBuildable\\..*:s"
      )
    case Some((2, 12)) =>
      Seq(
        "-Ywarn-macros:after",
        // silence warnings
        "-Wconf:cat=unused-params:s"
      )
    case _ =>
      Seq.empty
  }),
  headerLicense := Some(HeaderLicense.ALv2(currentYear.toString, organizationName.value)),
  headerMappings ++= Map(
    HeaderFileType.scala -> keepExistingHeader,
    HeaderFileType.java -> keepExistingHeader
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollectionCompatVersion,
    "joda-time" % "joda-time" % jodaTimeVersion % Provided,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq(
        "com.softwaremill.magnolia1_3" %% "magnolia" % magnoliaScala3Version
      )
    case Some((2, _)) =>
      Seq(
        "com.softwaremill.magnolia1_2" %% "magnolia" % magnoliaScala2Version,
        "com.chuusai" %% "shapeless" % shapelessVersion,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
      )
    case _ =>
      throw new Exception("Unsupported scala version")
  }),
  Test / fork := true,
  Test / javaOptions ++= Seq(
    "-Dorg.slf4j.simpleLogger.defaultLogLevel=info",
    "-Dorg.slf4j.simpleLogger.logFile=target/magnolify.log"
  )
)

lazy val root = tlCrossRootProject
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "magnolify",
    description := "A collection of Magnolia add-on modules"
  )
  .aggregate(
    avro,
    beam,
    bigquery,
    bigtable,
    bom,
    cats,
    datastore,
    guava,
    parquet,
    protobuf,
    refined,
    scalacheck,
    shared,
    tensorflow,
    neo4j,
    test,
    tools
  )

lazy val bom = project
  .in(file("bom"))
  .enablePlugins(BillOfMaterialsPlugin)
  .disablePlugins(TypelevelSettingsPlugin)
  .settings(
    // Just one BOM including all cross Scala versions
    crossVersion := CrossVersion.disabled,
    // Create BOM in the first run
    crossScalaVersions := Seq(scalaDefault),
    moduleName := "magnolify-bom",
    bomIncludeProjects := Seq(
      avro,
      bigquery,
      bigtable,
      cats,
      datastore,
      guava,
      parquet,
      protobuf,
      refined,
      scalacheck,
      shared,
      tensorflow,
      neo4j,
      tools
    ),
    // pom project. No ABI
    tlMimaPreviousVersions := Set.empty
  )

lazy val shared = project
  .in(file("shared"))
  .settings(
    commonSettings,
    crossScalaVersions := Seq(scala3, scala213, scala212),
    moduleName := "magnolify-shared",
    description := "Shared code for Magnolify",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test
  )

// shared code for unit tests
lazy val test = project
  .in(file("test"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(shared)
  .settings(commonSettings)
  .settings(
    crossScalaVersions := Seq(scala3, scala213, scala212),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitScalacheckVersion % Test,
      "org.typelevel" %% "cats-core" % catsVersion % Test
    ),
    Test / scalacOptions := {
      val opts = (Test / scalacOptions).value
      // silence warning.
      // cat & origin are not valid categories and filter yet
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => opts.filter(_ != "-Wunused:imports")
        case _            => opts
      }
    }
  )

lazy val scalacheck = project
  .in(file("scalacheck"))
  .dependsOn(
    shared % "compile,test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-scalacheck",
    description := "Magnolia add-on for ScalaCheck",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % scalacheckVersion % Provided
  )

lazy val cats = project
  .in(file("cats"))
  .dependsOn(
    shared,
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-cats",
    description := "Magnolia add-on for Cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion % Provided,
      "com.twitter" %% "algebird-core" % algebirdVersion % Test,
      "org.typelevel" %% "cats-laws" % catsVersion % Test
    )
  )

lazy val guava = project
  .in(file("guava"))
  .dependsOn(
    shared,
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-guava",
    description := "Magnolia add-on for Guava",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion % Provided
    )
  )

lazy val refined = project
  .in(file("refined"))
  .dependsOn(
    avro % Provided,
    bigquery % Provided,
    bigtable % Provided,
    datastore % Provided,
    guava % "provided,test->test",
    protobuf % "provided,test->test",
    tensorflow % Provided,
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-refined",
    description := "Magnolia add-on for Refined",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % guavaVersion % Provided,
      "eu.timepit" %% "refined" % refinedVersion % Provided,
      "com.google.api.grpc" % "proto-google-cloud-bigtable-v2" % bigtableVersion % Test,
      "com.google.apis" % "google-api-services-bigquery" % bigqueryVersion % Test,
      "com.google.cloud.datastore" % "datastore-v1-proto-client" % datastoreVersion % Test,
      "org.apache.avro" % "avro" % avroVersion % Test,
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.tensorflow" % "tensorflow-core-api" % tensorflowVersion % Test
    )
  )

lazy val avro = project
  .in(file("avro"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-avro",
    description := "Magnolia add-on for Apache Avro",
    libraryDependencies ++= Seq(
      "org.apache.avro" % "avro" % avroVersion % Provided,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion % Test
    )
  )

lazy val beam = project
  .in(file("beam"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    protobufSettings,
    moduleName := "magnolify-beam",
    description := "Magnolia add-on for Apache Beam",
    libraryDependencies ++= Seq(
      "org.apache.beam" % "beam-sdks-java-core" % beamVersion % Provided,
      "com.google.protobuf" % "protobuf-java" % protobufVersion % Provided
    ),
    // TODO remove this line after release
    tlMimaPreviousVersions := Set.empty
  )

lazy val bigquery = project
  .in(file("bigquery"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-bigquery",
    description := "Magnolia add-on for Google Cloud BigQuery",
    libraryDependencies ++= Seq(
      "com.google.apis" % "google-api-services-bigquery" % bigqueryVersion % Provided,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion % Test
    )
  )

lazy val bigtable: Project = project
  .in(file("bigtable"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-bigtable",
    description := "Magnolia add-on for Google Cloud Bigtable",
    libraryDependencies ++= Seq(
      "com.google.api.grpc" % "proto-google-cloud-bigtable-v2" % bigtableVersion % Provided
    )
  )

lazy val datastore = project
  .in(file("datastore"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-datastore",
    description := "Magnolia add-on for Google Cloud Datastore",
    libraryDependencies ++= Seq(
      "com.google.cloud.datastore" % "datastore-v1-proto-client" % datastoreVersion % Provided
    )
  )

lazy val parquet = project
  .in(file("parquet"))
  .dependsOn(
    shared,
    avro % Test,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-parquet",
    description := "Magnolia add-on for Apache Parquet",
    libraryDependencies ++= Seq(
      "org.apache.hadoop" % "hadoop-client" % hadoopVersion % Provided,
      "org.apache.parquet" % "parquet-avro" % parquetVersion % Provided,
      "org.apache.parquet" % "parquet-hadoop" % parquetVersion % Provided
    ),
    dependencyOverrides ++= Seq(
      "org.apache.avro" % "avro" % avroVersion % Provided,
      "org.apache.avro" % "avro" % avroVersion % Test
    ),
    apiMappings := {
      def findJar(organization: String, name: String): File =
        update.value.select { module: ModuleID =>
          module.organization == organization && module.name == name
        }.head

      Map(
        findJar("org.apache.parquet", "parquet-column") -> url(
          s"https://www.javadoc.io/doc/org.apache.parquet/parquet-column/$parquetVersion/"
        )
      )
    }
  )

lazy val protobuf = project
  .in(file("protobuf"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    protobufSettings,
    moduleName := "magnolify-protobuf",
    description := "Magnolia add-on for Google Protocol Buffer"
  )

val tensorflowMetadataSourcesDir =
  settingKey[File]("Directory containing TensorFlow metadata proto files")
val tensorflowMetadata =
  taskKey[Seq[File]]("Retrieve TensorFlow metadata proto files")

lazy val tensorflow = project
  .in(file("tensorflow"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    protobufSettings,
    moduleName := "magnolify-tensorflow",
    description := "Magnolia add-on for TensorFlow",
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % protobufVersion % ProtobufConfig,
      "org.tensorflow" % "tensorflow-core-api" % tensorflowVersion % Provided
    ),
    // remove compilation warnings for generated java files
    javacOptions ~= { _.filterNot(_ == "-Xlint:all") },
    // tensorflow metadata protos are not packaged into a jar. Manually extract them as external
    Compile / tensorflowMetadataSourcesDir := target.value / s"metadata-$tensorflowMetadataVersion",
    Compile / PB.protoSources += target.value / s"metadata-$tensorflowMetadataVersion",
    Compile / tensorflowMetadata := {
      def work(tensorFlowMetadataVersion: String) = {
        val tfMetadata = url(
          s"https://github.com/tensorflow/metadata/archive/refs/tags/v$tensorFlowMetadataVersion.zip"
        )
        IO.unzipURL(tfMetadata, target.value, "*.proto").toSeq
      }

      val cacheStoreFactory = streams.value.cacheStoreFactory
      val root = (Compile / tensorflowMetadataSourcesDir).value
      val tracker =
        Tracked.inputChanged(cacheStoreFactory.make("input")) { (versionChanged, version: String) =>
          val cached = Tracked.outputChanged(cacheStoreFactory.make("output")) {
            (outputChanged: Boolean, files: Seq[HashFileInfo]) =>
              if (versionChanged || outputChanged) work(version)
              else files.map(_.file)
          }
          cached(() => (root ** "*.proto").get().map(FileInfo.hash(_)))
        }

      tracker(tensorflowMetadataVersion)
    },
    Compile / PB.unpackDependencies := {
      val protoFiles = (Compile / tensorflowMetadata).value
      val root = (Compile / tensorflowMetadataSourcesDir).value
      val metadataDep = ProtocPlugin.UnpackedDependency(protoFiles, Seq.empty)
      val deps = (Compile / PB.unpackDependencies).value
      new ProtocPlugin.UnpackedDependencies(deps.mappedFiles ++ Map(root -> metadataDep))
    },
    Compile / packageBin / mappings ~= {
      _.filterNot { case (_, n) => n.startsWith("org/tensorflow") }
    },
    // Something messes with Compile/packageSrc/mappings and adds protobuf managed sources
    // Force back to original value from sbt
    inConfig(Compile)(Defaults.packageTaskSettings(packageSrc, Defaults.packageSrcMappings)),
    inConfig(Compile)(Defaults.packageTaskSettings(packageDoc, Defaults.packageDocMappings)),
    Compile / doc / scalacOptions ++= Seq("-skip-packages", "org.tensorflow")
  )

lazy val neo4j = project
  .in(file("neo4j"))
  .dependsOn(
    shared,
    cats % "test->test",
    scalacheck % "test->test",
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-neo4j",
    description := "Magnolia add-on for Neo4j",
    libraryDependencies ++= Seq(
      "org.neo4j.driver" % "neo4j-java-driver" % neo4jDriverVersion % Provided
    )
  )

lazy val tools = project
  .in(file("tools"))
  .dependsOn(
    shared,
    avro % Test,
    bigquery % Test,
    parquet % Test,
    test % "test->test"
  )
  .settings(
    commonSettings,
    moduleName := "magnolify-tools",
    description := "Magnolia add-on for code generation",
    libraryDependencies ++= Seq(
      "com.google.apis" % "google-api-services-bigquery" % bigqueryVersion,
      "org.apache.avro" % "avro" % avroVersion % Provided,
      "org.apache.parquet" % "parquet-hadoop" % parquetVersion,
      "org.typelevel" %% "paiges-core" % paigesVersion
    )
  )

lazy val jmh: Project = project
  .in(file("jmh"))
  .enablePlugins(JmhPlugin)
  .dependsOn(
    avro % Test,
    bigquery % Test,
    bigtable % Test,
    cats % Test,
    datastore % Test,
    guava % Test,
    parquet % Test,
    protobuf % "test->test",
    scalacheck % Test,
    tensorflow % Test,
    test % "test->test"
  )
  .settings(
    commonSettings,
    crossScalaVersions := Seq(scalaDefault),
    Jmh / classDirectory := (Test / classDirectory).value,
    Jmh / dependencyClasspath := (Test / dependencyClasspath).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile'
    // (otherwise a clean 'jmh:run' would fail)
    Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
    Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated,
    libraryDependencies ++= Seq(
      "com.google.api.grpc" % "proto-google-cloud-bigtable-v2" % bigtableVersion % Test,
      "com.google.apis" % "google-api-services-bigquery" % bigqueryVersion % Test,
      "com.google.cloud.datastore" % "datastore-v1-proto-client" % datastoreVersion % Test,
      "org.apache.avro" % "avro" % avroVersion % Test,
      "org.tensorflow" % "tensorflow-core-api" % tensorflowVersion % Test,
      "joda-time" % "joda-time" % jodaTimeVersion % Test,
      "org.apache.parquet" % "parquet-avro" % parquetVersion % Test,
      "org.apache.parquet" % "parquet-column" % parquetVersion % Test,
      "org.apache.parquet" % "parquet-hadoop" % parquetVersion % Test,
      "org.apache.hadoop" % "hadoop-common" % hadoopVersion % Test,
      "org.apache.hadoop" % "hadoop-mapreduce-client-core" % hadoopVersion % Test
    )
  )

// =======================================================================
// Site settings
// =======================================================================
lazy val site = project
  .in(file("site"))
  .enablePlugins(
    NoPublishPlugin,
    ParadoxSitePlugin,
    ParadoxMaterialThemePlugin,
    GhpagesPlugin,
    SiteScaladocPlugin,
    MdocPlugin
  )
  .dependsOn(
    avro % "compile->compile,provided",
    beam % "compile->compile,provided",
    bigquery % "compile->compile,provided",
    bigtable % "compile->compile,provided",
    cats % "compile->compile,provided",
    datastore % "compile->compile,provided",
    guava % "compile->compile,provided",
    neo4j % "compile->compile,provided",
    parquet % "compile->compile,provided",
    protobuf % "compile->compile,provided",
    refined % "compile->compile,provided",
    shared,
    scalacheck % "compile->compile,provided",
    tensorflow % "compile->compile,provided",
    unidocs
  )
  .settings(commonSettings)
  .settings(
    description := "Magnolify - Documentation",
    fork := false,
    autoAPIMappings := true,
    gitRemoteRepo := "git@github.com:spotify/magnolify.git",
    // mdoc
    // pre-compile md using mdoc
    mdocExtraArguments ++= Seq("--no-link-hygiene"),
    // paradox
    Compile / paradoxOverlayDirectories += mdocOut.value,
    Compile / paradoxProperties ++= Map(
      "github.base_url" -> "https://github.com/spotify/magnolify"
    ),
    Compile / paradoxMaterialTheme := ParadoxMaterialTheme()
      .withFavicon("images/favicon.ico")
      .withColor("white", "indigo")
      .withLogo("images/logo.png")
      .withCopyright(s"Copyright (C) $currentYear Spotify AB")
      .withRepository(uri("https://github.com/spotify/magnolify"))
      .withSocial(uri("https://github.com/spotify"), uri("https://twitter.com/spotifyeng")),
    // sbt-site
    addMappingsToSiteDir(
      unidocs / ScalaUnidoc / packageDoc / mappings,
      unidocs / ScalaUnidoc / siteSubdirName
    ),
    makeSite := makeSite.dependsOn(mdoc.toTask("")).value
  )

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(
    NoPublishPlugin,
    TypelevelUnidocPlugin
  )
  .settings(commonSettings)
  .settings(
    moduleName := "magnolify-docs",
    crossScalaVersions := Seq(scalaDefault),
    scalaVersion := scalaDefault,
    // unidoc
    ScalaUnidoc / siteSubdirName := "api",
    ScalaUnidoc / scalacOptions := Seq.empty,
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(test, jmh),
    ScalaUnidoc / unidoc / unidocAllClasspaths ~= { cp =>
      // somehow protobuf 2 is in classpath and fails doc
      cp.map(_.filterNot(_.data.getName.endsWith("protobuf-java-2.5.0.jar")))
    },
    ScalaUnidoc / unidoc / unidocAllSources ~= { sources =>
      // filter out doc from generated proto TFMD sources
      sources.map(_.filterNot(_.getPath.contains("compiled_proto")))
    }
  )
