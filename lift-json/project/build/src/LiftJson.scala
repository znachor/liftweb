import sbt._

class LiftJson(info: ProjectInfo) extends DefaultProject(info) {
  val specs      = "org.scala-tools.testing" % "specs" % "1.5.0"
  val scalacheck = "org.scala-tools.testing" % "scalacheck" % "1.5"
  
  override def ivyXML =
    <publications>
      <artifact name="lift-json" type="jar" ext="jar"/>
    </publications>
}
