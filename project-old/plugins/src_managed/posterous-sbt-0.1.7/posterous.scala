package posterous
import sbt._

import dispatch._
import java.net.URI
import com.tristanhunt.knockoff.DefaultDiscounter._
import scala.xml.Node

trait Publish extends BasicDependencyProject {
  import Publish._
  def posterousCredentialsPath = Path.userHome / ".posterous"
  private def getPosterousProperty(name: String) = {
    val props = new java.util.Properties
    FileUtilities.readStream(posterousCredentialsPath.asFile, log){ input => props.load(input); None }
    props.getProperty(name, "")
  }
  def posterousEmail = getPosterousProperty("email")
  def posterousPassword = getPosterousProperty("password")
  
  /** Posterous site id, defaults to implicit.ly */
  def postSiteId = 1031779
  /** Hostname of target posterous, used to check that a post is not a duplicate */
  def posterousSite = "implicit.ly"
  /** Subjective tags for the release notes post, e.g. a Scala library this project uses. */
  def extraTags: List[String] = Nil
  /** Strings to tag the post with, defaults to the project name, organization, extraTags, and Scala build versions */
  def postTags = name :: organization :: extraTags ::: crossScalaVersions.map { "Scala " + _ }.toList
  /** Title defaults to name and version */
  def postTitle(vers: String) = "%s %s".format(name, vers)
  /** Path to release notes and text about project. */
  def notesPath = path("notes")
  def notesFiles = notesPath * ("*" + extension)
  override def watchPaths = super.watchPaths +++ notesFiles
  def extension = ".markdown"
  /** Current version with any -SNAPSHOT suffix removed */
  def currentNotesVersion = "-SNAPSHOT$".r.replaceFirstIn(version.toString, "")
  /** Release notes named with the version and a .markdown suffix. */
  def versionNotesPath(version: String) = notesPath / (version + extension)
  /** Project info named about.markdown. */
  def aboutNotesPath = notesPath / ("about" + extension)
  /** The content to be posted, transformed into xml. Default implementation is the version notes
      followed by the "about" boilerplate in a div of class "about" */
  def postBody(vers: String) = 
    mdToXml(versionNotesPath(vers)) ++
    <div class="about"> { mdToXml(aboutNotesPath) } </div>
  /** Agent that is posting to Posterous (this plugin) */
  def postSource = <a href="http://github.com/n8han/posterous-sbt">posterous-sbt plugin</a>

  private def versionTask(inner: String => Option[String]) = task { _ match {
    case Array(vers:String) => task { inner(vers) }
    case _ => task { inner(currentNotesVersion) }
  } }
  
  lazy val publishNotes = publishNotesAction
  def publishNotesAction = versionTask(publishNotes_!) describedAs ("Publish project release notes to Posterous.")
  /** Parameterless action provided as a convenience for adding as a dependency to other actions */
  def publishCurrentNotes = task { publishNotes_!(currentNotesVersion) }

  lazy val checkPublishNotes = checkPublishNotesAction
  def checkPublishNotesAction = versionTask(publishNotesReqs) describedAs ("Check that all requirements for notes publishing are met.")

  lazy val changelog = changelogAction
  def changelogPath = outputPath / "CHANGELOG.md"
  def changelogAction = task { generateChangelog(changelogPath) } describedAs ("Produce combined release notes" )

  /** @returns Some(error) if a note publishing requirement is not met */
  def publishNotesReqs(vers: String) = localNotesReqs(vers) orElse 
    credentialReqs orElse uniquePostReq(vers)
  def credentialReqs = ( missing(posterousCredentialsPath, "credentials file")
    ) orElse { missing(posterousEmail, posterousCredentialsPath, "email")
    } orElse { missing(posterousPassword, posterousCredentialsPath, "password") }
  def localNotesReqs(version: String) = missing(versionNotesPath(version), "release notes file")
  
  def posterousApi = :/("posterous.com").secure / "api" as_! (posterousEmail, posterousPassword)

  def generateChangelog(output: Path) = {
    def cmpName(f: Path) = f.base.replace("""\.markdown$""", "").replaceAll("""\.""", "")
    val outputFile = output.asFile
    val inOrder = (notesFiles --- aboutNotesPath).getFiles.toList.map(Path.fromFile(_)).
                  sort((p1, p2) => cmpName(p1).compareTo(cmpName(p2)) < 0).reverse.foreach { p =>
      val fileVersion = p.base.replace("""\.markdown$""", "")
      FileUtilities.readString(p.asFile, log) match {
        case Right(text) => FileUtilities.append(outputFile, "\nVersion " + fileVersion + ":\n\n" + text, log)
        case Left(error) => throw new RuntimeException(error)
      }
    }
    log.info("Generated " + output)
    None
  }

  def publishNotes_!(vers: String) =
    publishNotesReqs(vers) orElse {
      val newpost = posterousApi / "newpost" << Map(
        "site_id" -> postSiteId,
        "title" -> postTitle(vers),
        "tags" -> postTags.map { _.replace(",","_") }.removeDuplicates.mkString(","),
        "body" -> postBody(vers).mkString,
        "source" -> postSource
      )
      http { _(newpost <> { rsp =>
        (rsp \ "post" \ "url").firstOption match {
          case None => Some("No post URL found in response:\n" + rsp.mkString)
          case Some(url) =>
            log.success("Posted release notes: " + url.text)
            tryBrowse(new URI(url.text), true) // ignore any error if not 1.6
        }
      }) }
    }

  /** Check that the current version's notes aren't already posted to posterous */
  def uniquePostReq(vers: String) = {
    val posting = :/(posterousSite) / postTitle(vers).replace(" ", "-").replace(".", "")
    http { _.x(posting.HEAD) { 
      case (200 | 302, _, _) =>  Some("Someone has already posted notes on version %s at %s" format(
        vers, posting.to_uri
      )) 
      case _ => None
    } }
  }

  lazy val checkPosterous = checkPosterousAction
  def checkPosterousAction = task { credentialReqs orElse {
    http { _(posterousApi / "getsites" <> { rsp =>
      log.info("%s contributes to the following sites:" format posterousEmail)
      for (site <- rsp \ "site"; id <- site \ "id"; name <- site \ "name")
        log.info("  %-10s%s" format (id.text, name.text))
      
      rsp \ "site" \ "id" filter { _.text == postSiteId.toString } match {
        case ids if ids.isEmpty => Some("You are not authorized to contribute to %d, this project's postSiteId." format postSiteId)
        case _ => 
          log.success("You may contribute to %d, this project's postSiteId." format postSiteId)
          None
      }
    }) }
  } } describedAs ("Check Posterous credentials and permissions for the current postSiteId.")

  /** Where notes are saved for previewing */
  def notesOutputPath = outputPath / ("%s-notes.html" format artifactBaseName)
  lazy val previewNotes = previewNotesAction
  def previewNotesAction = versionTask { vers =>
    localNotesReqs(vers) orElse {
      FileUtilities.write(notesOutputPath.asFile, 
          <html>
          <head>
            <title> { postTitle(vers) } </title>
            <style> {"""
              div.about * { font-style: italic }
              div.about em { font-style: normal }
            """} </style>
          </head>
          <body>
            <h2><a href="#">{ postTitle(vers) }</a></h2>
            { postBody(vers) }
            <div id="tags">
              { mdToXml(postTags.map("[%s](#)" format _) mkString("""Filed under // """, " ", "")) }
            </div>
          </body>
          </html> mkString, log
      ) orElse {
        log.success("Saved release notes: " + notesOutputPath)
        tryBrowse(notesOutputPath.asFile.toURI, false)
      }
    }
  } describedAs ("Preview project release notes as HTML and check for publishing credentials.")
  
}

/** Utility functions */
object Publish {
  def http(block: Http => Option[String]) = try {
    block(new Http)
  } catch {
    case e: StatusCode => Some(e.getMessage)
  }
  
   /** @return node sequence from str or Nil if str is null or empty. */
  def mdToXml(str: String) = str match {
    case null | "" => Nil
    case _ => toXML(knockoff(str))
  }   

  /** @return node sequence from file or Nil if file is not found. */
  def mdToXml(md: Path) =
    if (md.exists)
      toXML(knockoff(scala.io.Source.fromFile(md.asFile).mkString))
    else
      Nil

 def missing(path: Path, title: String) =
    Some(path) filter (!_.exists) map { ne =>
      "Missing %s, expected in %s" format (title, path)
    }

  def missing(str: String, path: Path, title: String) = 
    Some(str) filter { _ == "" } map { str =>
      "Missing value %s in %s" format (title, path)
    }
    
  /** Opens uri in a browser if on a JVM 1.6+ */
  def tryBrowse(uri: URI, quiet: Boolean) = {
    try {
      val dsk = Class.forName("java.awt.Desktop")
      dsk.getMethod("browse", classOf[java.net.URI]).invoke(
        dsk.getMethod("getDesktop").invoke(null), uri
      )
      None
    } catch { case e => if(quiet) None else Some("Error trying to preview notes:\n\t" + rootCause(e).toString) }
  }
  private def rootCause(e: Throwable): Throwable = if(e.getCause eq null) e else rootCause(e.getCause)
}
