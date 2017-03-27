package org.github.il2boscontrol.txtlog

import java.io.{File, InputStreamReader, LineNumberReader}
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.function.Consumer

import scala.util.{Failure, Success, Try}

object ProcessAll extends App {

  val files = Files.newDirectoryStream(new File("C:\\Users\\ivano.DESKTOP-7AGOO0J\\Downloads\\logs\\txt\\mission_report_backup\\2016\\10\\6").toPath)

  files.forEach(new Consumer[Path] {
    override def accept(t: Path) = {
      try {

        val reader = new LineNumberReader(Files.newBufferedReader(t))

        println(s"Reading file $t")

        val source = scala.io.Source.fromFile(t.toFile)
        val events = source.getLines.map { line =>
          line -> EventlogEventParsers.parse(EventlogEventParsers.event, line)
        }
        events.foreach {
          case (line, r) if r.successful => //println(r.get)
          case (line, r) => println(s"err: $line, $r")
        }
      } catch {
        case ex => println(ex)
      }
    }
  })


}
