import sbt._

trait Libs extends ManagedProject {
    def C = "compile->default"
    def P = "provided->default"
    def T = "test->default"

    def paranamer          = "com.thoughtworks.paranamer" % "paranamer"                % "2.0"
    def commonsCollections = "commons-collections"        % "commons-collections"      % "3.2.1"
    def commonsCodec       = "commons-codec"              % "commons-codec"            % "1.3"
    def commonsFileUpload  = "commons-fileupload"         % "commons-fileupload"       % "1.2.1"
    def commonsHttpClient  = "commons-httpclient"         % "commons-httpclient"       % "3.1"
    def javaxMail          = "javax.mail"                 % "mail"                     % "1.4.1"

    // provided
    def javaxPersistence   = "javax.persistence"          % "persistence-api"          % "1.0"
    def javaxServlet       = "javax.servlet"              % "servlet-api"              % "2.5"

    def log4j              = "log4j"                      % "log4j"                    % "1.2.14"
    def jetty              = "org.mortbay.jetty"          % "jetty"                    % "6.1.21"
    def specs              = "org.scala-tools.testing"    % "specs"                    % "1.6.1"
    def scalacheck         = "org.scala-tools.testing"    %% "scalacheck"              % "1.6"
    def junit              = "junit"                      % "junit"                    % "4.7"
    def slf4jApi           = "org.slf4j"                  % "slf4j-api"                % "1.5.9.RC1"
    def slf4jSimple        = "org.slf4j"                  % "slf4j-simple"             % "1.5.9.RC1"
    def scalaJpa           = "org.scala-libs"             % "scalajpa"                 % "1.1"

    // todo: Exclude javax.servlet/servlet-api
    def jwebUnit           = "net.sourceforge.jwebunit"   % "jwebunit-htmlunit-plugin" % "2.2"

    def rabbitmq                    = "com.rabbitmq"    % "rabbitmq-client"        % "1.3.0"

    def atomikosTransactions        = "com.atomikos"    % "transactions"          % "3.2.3"
    def atomikosTransactionsJta     = "com.atomikos"    % "transactions-jta"      % "3.2.3"
    def atomikosTransactionsApi     = "com.atomikos"    % "transactions-api"      % "3.2.3"
    def atomikosUtil                = "com.atomikos"    % "atomikos-util"         % "3.2.3"
    def geronimoSpecsJta  = "org.apache.geronimo.specs" % "geronimo-jta_1.1_spec" % "1.1.1"

    def openid4java                 = "org.openid4java" % "openid4java"            % "0.9.5"

    // TODO exclude javax.transaction/jta
    def hibernateEntityManager = "org.hibernate" % "hibernate-entitymanager" % "3.2.1.ga"

    // TODO: Exclude servlet API in smack and smackx
    def smack  = "jivesoftware" % "smack"  % "3.1.0"
    def smackx = "jivesoftware" % "smackx" % "3.1.0"

    // OSGi - provided
    def scalamodulesCore = "org.scalamodules"       % "scalamodules-core"      % "1.1-SNAPSHOT"
    def osgiCompendium   = "org.apache.felix"       % "org.osgi.compendium"    % "1.2.0"
    def paxWeb           = "org.ops4j.pax.web"      % "pax-web-bundle"         % "0.6.0"
    def paxSwissbox      = "org.ops4j.pax.swissbox" % "pax-swissbox-extender"  % "0.2.0"

    def loggingSimple = Set(slf4jApi    % C,
                            slf4jSimple % T)

    def testing       = Set(specs       % T,// % withSources(),
                            scalacheck  % T,
                            junit       % T)
}

trait LiftDefaultProject extends BasicManagedProject with Libs {
    val mavenLocalRepo = Resolver.file("maven-local-repo", (Path.userHome / ".m2" / "repository").asFile)
    override def libraryDependencies = depends ++ defaultLibs ++ super.libraryDependencies
    def defaultLibs = testing
    def depends: Set[ModuleID]
}

trait LiftParentProject extends ParentProject with LiftDefaultProject {
    // Resetting the autodect manager option from ParentProject, so SBT does
    // not try to parse pom.xml files.
    override def managerOption: Seq[ManagedOption] = LibraryManager(manager) :: Nil
    def depends = Set.empty[ModuleID]
}

// todo: Optional dependencies
trait Database extends ManagedProject {
    val mysql      = "mysql"            % "mysql-connector-java" % "5.1.6"
    val derby      = "org.apache.derby" % "derby"                % "10.4.2.0"
    val h2         = "com.h2database"   % "h2"                   % "1.2.123"
    val postgresql = "postgresql"       % "postgresql"           % "8.3-603.jdbc3"
}

class LiftProject(info: ProjectInfo) extends ParentProject(info)
    with LiftParentProject {

    lazy val base        = project("lift-base"       , "Lift Base Components", new LiftBase(_))
    lazy val persistence = project("lift-persistence", "Lift Persistence Components", new LiftPersistence(_))
    lazy val modules     = project("lift-modules"    , "Lift Modules", new LiftModules(_))

    class LProj(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
        def depends = Set.empty[ModuleID]
    }

    class LiftBase(info: ProjectInfo) extends ParentProject(info) with LiftParentProject {
        lazy val actor  = project("lift-actor" , "Lift Actor" , new LProj(_), common)
        lazy val common = project("lift-common", "Lift Common", new LProj(_))
        lazy val json   = project("lift-json"  , "Lift Json"  , new LiftJson(_))
        lazy val util   = project("lift-util"  , "Lift Util"  , new LiftUtil(_), actor)
        lazy val webkit = project("lift-webkit", "Lift WebKit", new LiftWebKit(_), util)

        class LiftJson(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(paranamer % C)
        }

        class LiftUtil(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(commonsCollections % C,
                              commonsCodec % C,
                              javaxMail % C,
                              log4j % C) ++ loggingSimple
        }

        class LiftWebKit(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(commonsFileUpload % C,
                              javaxServlet % P,
                              jetty % T,
                              jwebUnit % T) ++ loggingSimple
        }
    }

    class LiftPersistence(info: ProjectInfo) extends ParentProject(info) with LiftParentProject {
        lazy val jpa    = project("lift-jpa"   , "Lift JPA"   , new LiftJPA(_), base.webkit)
        lazy val mapper = project("lift-mapper", "Lift Mapper", new LProj(_), base.webkit, modules.widgets)
        lazy val record = project("lift-record", "Lift Record", new LProj(_), mapper)

        class LiftJPA(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(scalaJpa % C,
                              javaxPersistence % C)
        }
    }

    class LiftModules(info: ProjectInfo) extends ParentProject(info) with LiftParentProject {
        lazy val amqp     = project("lift-amqp"    , "Lift AMQP"    , new LiftAMQP(_), base.actor)
        lazy val facebook = project("lift-facebook", "Lift Facebook", new LProj(_), base.webkit)
        lazy val jta      = project("lift-jta"     , "Lift JTA"     , new LiftJTA(_), base.util)
        lazy val machine  = project("lift-machine" , "Lift Machine" , new LProj(_), base.webkit, persistence.mapper)
        lazy val oauth    = project("lift-oauth"   , "Lift OAuth"   , new LProj(_))
        lazy val openid   = project("lift-openid"  , "Lift OpenID"  , new LiftOpenID(_), base.webkit, persistence.mapper)
        lazy val osgi     = project("lift-osgi"    , "Lift OSGi"    , new LiftOSGi(_), base.webkit)
        lazy val paypal   = project("lift-paypal"  , "Lift PayPal"  , new LiftPayPal(_), base.webkit)
        lazy val testkit  = project("lift-testkit" , "Lift TestKit" , new LiftTestKit(_), base.webkit)
        lazy val textile  = project("lift-textile" , "Lift Textile" , new LiftTextile(_))
        lazy val widgets  = project("lift-widgets" , "Lift Widgets" , new LProj(_), base.webkit)
        lazy val wizard   = project("lift-wizard"  , "Lift Wizard"  , new LProj(_), base.webkit)
        lazy val xmpp     = project("lift-xmpp"    , "Lift XMPP"    , new LiftXMPP(_), base.actor)

        class LiftAMQP(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(rabbitmq % C)
        }

        class LiftJTA(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(scalaJpa % C,
                              javaxPersistence % P,
                              atomikosTransactions % C,
                              atomikosTransactionsJta % C,
                              atomikosTransactionsApi % C,
                              atomikosUtil % C,
                              geronimoSpecsJta % C,
                              hibernateEntityManager % C)
        }

        class LiftOpenID(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(openid4java % C)
        }

        // todo: bnd plugin
        class LiftOSGi(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            val opsj4_releases = "opsj4-releases" at "http://repository.ops4j.org/maven2"

            def depends = Set(scalamodulesCore % P,
                              osgiCompendium % P,
                              paxWeb % P,
                              paxSwissbox % P,
                              javaxServlet % P)
        }

        class LiftPayPal(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(commonsHttpClient % C)
        }

        class LiftTestKit(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(commonsHttpClient % C,
                              javaxServlet % P)
        }

        class LiftTextile(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            // Depencency on lift-util in test scope
            override def deliverProjectDependencies =
                super.deliverProjectDependencies.toList - base.util.projectID ++ Seq(base.util.projectID % T)
            def depends = Set.empty[ModuleID]
        }

        class LiftXMPP(info: ProjectInfo) extends DefaultProject(info) with LiftDefaultProject {
            def depends = Set(smack % C, smackx % C)
        }
    }
}
