import sbt._

class LiftProject(info: ProjectInfo) extends DefaultWebProject(info) {

  // Disable cross scala path
  override def disableCrossPaths = true

  // Add Maven Local repository for SBT to search for
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

#if ($liftVersion.endsWith("SNAPSHOT"))
  // Add snapshot repo, since Lift SNAPSHOT in use
  val snapshots = ScalaToolsSnapshots
#end
  
}
