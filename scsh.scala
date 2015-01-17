#!/usr/bin/env scala
import java.io.File
import scala.io.StdIn
import scala.io.Source
import scala.io.AnsiColor
import scala.util.Properties
import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}


val logger = ProcessLogger (
  out => System.out.println(out),
  err => System.err.println(err)
)
var pos = new File(".")

// command execution
def system(cmd: Seq[String]) = Process(cmd, pos) ! logger
def parse(cmd: String) =
  """\$\{([a-zA-Z_]+)\}""".r
  .replaceAllIn(cmd, _.group(1) match { case e => env.get(e) })
  .replace("\\ ", "$space")
  .split(' ')
  .map ( _.trim.replace("$space", " ") )
  .toList

def getFile(file: File)(name: String) = new File(file.toURI.resolve(name))
// read a line with prompt
def readLine(prompt: String) = {
  print(prompt)
  StdIn.readLine()
}

class Env {
  import scala.io.AnsiColor._
  private val p = RED + "$ " + RESET
  val values = mutable.Map[String, String]("prompt" -> p)
  def get(x: String) = Properties.envOrElse(x, values.getOrElse(x, ""))
}


// config
class Conf {
  val file = new File(".scshrc")
  def load(): Unit =
    for (l <- Source.fromFile(file).getLines) run(l)
}


class Alias {
  // aliases
  val table = mutable.Map[String, String]()
  // returns aliased list
  def aliased(cmd: List[String]): List[String] = cmd match {
    case Nil     => Nil
    case x :: xs => parse(aliased(x)) ::: xs
  }
  // returns aliased string
  def aliased(cmd: String): String = table.getOrElse(cmd, cmd)

  // split string by the first '='
  def splitByEq(str: String) =
    str.splitAt(str.indexOf('=')) match { case (l, r) => (l, r.tail) }
}


val alias = new Alias()
val conf = new Conf()
val env = new Env()
// command execution
def run(input: String): Boolean =
  parse(input) match {
    case Nil => true // do nothing

    // shell built-in commands
    case "exit" :: xs =>
      println("Program will exit...")
      sys.exit(0)
      true

    case "cd" :: Nil => true // do nothing
    case "cd" :: x :: _ =>
      val dest = getFile(pos)(x)
      if (!dest.isDirectory) {
        println("cd: no such directory")
        false
      } else {
        pos = dest
        true
      }

    case "alias" :: Nil =>
      println(alias.table.mkString("\n"))
      true

    case "alias" :: x :: _ =>
      if (!x.contains('=')) {
        if (alias.table.contains(x)) {
          println(s"$x=${alias.table(x)}")
          true
        } else false
      } else {
        val p = alias.splitByEq(x)
        println(s"alias: ${p._1} -> ${p._2}")
        alias.table += p
        true
      }

    case in =>
      // result (0 or else)
      val result = try system(alias.aliased(in))
      catch {
        case e: java.io.IOException =>
          println(s"permission denied: $input")
          1
      }
      result == 0
  }

conf.load()

while (true) {
  val input = readLine(env.values("prompt"))
  run(input)
}

