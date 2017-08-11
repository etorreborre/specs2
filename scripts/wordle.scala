import scala.io._
import java.io._

def scalaFiles(file: File): List[File] = {
	println("checking "+file.getName)
	if (file.isDirectory)
		file.listFiles.toList.flatMap(scalaFiles(_))
	else if (file.getName.endsWith(".scala"))
		List(file)
	else
  	Nil
}

println("starting wordle")
val all = scalaFiles(new File("src/")).flatMap(Source.fromFile(_).getLines.toList)
val out = new File("wordle.txt")
val writer = new FileWriter(out)
writer.write(all.mkString("\n"))
writer.close
