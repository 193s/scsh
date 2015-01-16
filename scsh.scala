#!/usr/bin/env scala
import java.io.File
import scala.io.StdIn
import scala.io.Source
import scala.io.AnsiColor
import scala.collection.mutable
import scala.sys.process._


val logger = ProcessLogger (
  out => System.out.println(out),
  err => System.err.println(err)
)
var pos = new java.io.File(".")

// command execution
def system(cmd: Seq[String]) = Process(cmd, pos) ! logger
def parse(cmd: String): List[String] = cmd.replace("\\ ", "__space__").split(' ').map(_.trim.replace("__space__", " ")).toList
def getFile(file: File)(name: String) = new File(file.toURI.resolve(name))
// read a line with prompt
def readLine(prompt: String) = {
  print(prompt)
  val ret = StdIn.readLine()
  ret
}
val prompt = AnsiColor.RED + "$ " + AnsiColor.RESET
// config
val conf_file = new File(".scshrc")
def loadConf() {
  for (l <- Source.fromFile(conf_file).getLines) {
    run(l)
  }
}

// aliases
val alias_table = mutable.Map[String, String]()
// returns aliased list
def aliased(cmd: List[String]): List[String] = cmd match {
  case Nil     => Nil
  case x :: xs => parse(aliased(x)) ::: xs
}
// returns aliased string
def aliased(cmd: String): String =
  if (alias_table.contains(cmd)) alias_table(cmd)
  else cmd

// split string by the first '='
def splitByEq(str: String) = {
  val (l, r) = str.splitAt(str.indexOf('='))
  (l, r.tail)
}

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
      }
      else {
        pos = dest
        true
      }

    case "alias" :: Nil =>
      println(alias_table.mkString("\n"))
      true

    case "alias" :: x :: _ =>
      if (!x.contains('=')) {
        if (alias_table.contains(x)) {
          println(s"$x=${alias_table(x)}")
          true
        } else false
      } else {
        val (name, value) = splitByEq(x)
        println(s"alias: $name -> $value")
        alias_table += name -> value
        true
      }

    case in =>
      // result (0 or else)
      val result = try system(aliased(in))
      catch {
        case e: java.io.IOException =>
          println(s"permission denied: $input")
          1
      }
      result == 0
  }

loadConf()

while (true) {
  val input = readLine(prompt)
  run(input)
}
