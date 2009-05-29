package au.com.immu.snippet

/*
 Normally, a thin wrapper around the Log4j Logger
*/
object Log {
  
  def debug(msg: String) = println("DEBUG: "+msg)
  def info(msg: String) = println("INFO: "+msg)
  def error(msg: String) = println("ERROR: "+msg)

}
