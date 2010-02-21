import sbt._

class LiftFrameworkProject(info: ProjectInfo) extends ParentProject(info) with LiftBasicProjectPlugin {

  override def parallelExecution = true

  lazy val base = project("lift-base", "lift-base", new LiftBaseProject(_))
  // lazy val persistence = project("lift-persistence", "lift-persistence", new LiftPersistenceProject(_), base)
  // lazy val modules = project("lift-modules", "lift-modules", new LiftModulesProject(_), base, persistence)

  class LiftBaseProject(info: ProjectInfo) extends ParentProject(info) with LiftBasicProjectPlugin {
    lazy val common = project("lift-common", "lift-common", new LiftCommonProject(_))
    lazy val actor = project("lift-actor", "lift-actor", new LiftActorProject(_), common)
    lazy val json = project("lift-json", "lift-json", new LiftJsonProject(_), common)
    lazy val util = project("lift-util", "lift-util", new LiftUtilProject(_), actor)
    lazy val webkit = project("lift-webkit", "lift-webkit", new LiftWebkitProject(_), json, util)

    class LiftCommonProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftActorProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftJsonProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftUtilProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftWebkitProject(info: ProjectInfo) extends LiftDefaultProject(info)
  }

  class LiftPersistenceProject(info: ProjectInfo) extends ParentProject(info) with LiftBasicProjectPlugin {
    lazy val mapper = project("lift-mapper", "lift-mapper", new LiftMapperProject(_))
    lazy val record = project("lift-record", "lift-record", new LiftRecordProject(_), mapper)
    lazy val jpa = project("lift-jpa", "lift-jpa", new LiftJpaProject(_), mapper)
    lazy val couchdb = project("lift-couchdb", "lift-jpa", new LiftJpaProject(_), mapper, record)

    class LiftMapperProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftRecordProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftJpaProject(info: ProjectInfo) extends LiftDefaultProject(info)
    class LiftCouchdbProject(info: ProjectInfo) extends LiftDefaultProject(info)
  }

}

// TODO: This would go in a plugin
trait LiftBasicProjectPlugin extends BasicManagedProject {

  import java.util.Calendar

  val projectUrl = propertyOptional[String]("", true)
  val projectInceptionyear = propertyOptional[Int](Calendar.getInstance().get(Calendar.YEAR), true)
  val projectOrganizationName = propertyOptional[String](organization, true)
  val projectOrganizationUrl = propertyOptional[String](projectUrl.value, true)
  val projectLicenseName = propertyOptional[String]("", true)
  val projectLicenseUrl = propertyOptional[String](if (isApacheLic) "http://www.apache.org/licenses/LICENSE-2.0.txt" else "", true)
  val projectLicenseComment = propertyOptional[String](name + " is licensed under " + projectLicenseName.value, true) // TODO: format smarter
  val projectLicenseDistribution = propertyOptional[String]("repo", true) // TODO: object DistRepo extends Enumeration {...}

  private def isApacheLic = projectLicenseName.value.startsWith("Apache") // TODO: Enrich

    override def disableCrossPaths = true
  //  override def crossScalaVersions: Seq[String] = Seq.empty

  // Add ScalaToolsSnapshots if this project is on snapshot
  //  val snapshots = ScalaToolsSnapshots
  override def repositories = version.toString match {
    case s if s.endsWith("-SNAPSHOT") => super.repositories + ScalaToolsSnapshots
    case _ => super.repositories
  }

  // Add Maven Local repository for SBT to search for
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  // But take care to remove it from generated pom, how?
  lazy val removeMavenLocal = task {
    log.info("TODO: Remove mavenLocal from the repository List " + projectOrganizationName.value)
    None
  }

  override def makePomAction = super.makePomAction dependsOn removeMavenLocal

  override def pomExtra =
    <url>{projectUrl.value}</url>
    <inceptionYear>{projectInceptionyear.value}</inceptionYear>
    <organization>
      <name>{projectOrganizationName.value}</name>
      <url>{projectOrganizationUrl.value}</url>
    </organization>
    <licenses>
      <license>
        <name>{projectLicenseName.value}</name>
        <url>{projectLicenseUrl.value}</url>
        <distribution>{projectLicenseDistribution.value}</distribution>
        <!-- <comments>Lift open source software is licensed under an Apache 2.0 license.</comments> -->
        <comments>{projectLicenseComment.value}</comments>
      </license>
    </licenses>;
}

abstract class LiftDefaultProject(info: ProjectInfo) extends DefaultProject(info) with LiftBasicProjectPlugin {

  // Compile options

  override def compileOptions = super.compileOptions ++ Seq(Deprecation, ExplainTypes) ++ Seq("-encoding", "utf8").map(x => CompileOption(x))

  // Test options
  // override def testOptions = super.testOptions ++ TODO

  // Package options

  import java.util.jar.Attributes

  lazy val extraSpecificationEntries = ManifestAttributes(
    (Attributes.Name.SPECIFICATION_TITLE.toString, name),
    (Attributes.Name.SPECIFICATION_VERSION.toString, version.toString),
    (Attributes.Name.SPECIFICATION_VENDOR.toString, projectOrganizationName.value))

  lazy val extraImplementationEntries = ManifestAttributes(
    (Attributes.Name.IMPLEMENTATION_TITLE.toString, name),
    (Attributes.Name.IMPLEMENTATION_VERSION.toString, version.toString),
    (Attributes.Name.IMPLEMENTATION_VENDOR_ID.toString, organization),
    (Attributes.Name.IMPLEMENTATION_VENDOR.toString, projectOrganizationName.value),
    (Attributes.Name.IMPLEMENTATION_URL.toString, projectUrl.value))

  override def packageOptions = super.packageOptions ++ Seq(extraSpecificationEntries, extraImplementationEntries)

  // Document options
  
  import java.util.Calendar
  
  private val _footer = String.format(
    "Copyright (c) %s-%s %s. All Rights Reserved.", 
    projectInceptionyear.value.toString, Calendar.getInstance().get(Calendar.YEAR).toString, 
    projectOrganizationName.value)

//  override def documentOptions = super.documentOptions ++ Seq(LinkSource, documentFooter("Copyright &#169; {inceptionYear}-{currentYear} {organizationName}. All Rights Reserved."))
  override def documentOptions = super.documentOptions ++ Seq(LinkSource, documentFooter(_footer))

}
