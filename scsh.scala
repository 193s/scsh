#!/usr/bin/env scala
import java.io.File
import scala.io.StdIn
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
def parse(cmd: String): List[String] = cmd.split(' ').map(_.trim).toList
def getFile(file: File)(name: String) = new File(file.toURI.resolve(name))
// read a line with prompt
def readLine(prompt: String) = {
  print(prompt)
  val ret = StdIn.readLine()
  ret
}
val prompt = AnsiColor.RED + "$ " + AnsiColor.RESET
val alias_table = mutable.Map("ls" -> "gls --color")

def aliased(cmd: List[String]): List[String] = cmd match {
  case Nil     => Nil
  case x :: xs => parse(aliased(x)) ::: xs
}
def aliased(cmd: String): String =
  if (alias_table.contains(cmd)) alias_table(cmd)
  else cmd

// split string by the first '='
def splitByEq(str: String) = {
  val (l, r) = str.splitAt(str.indexOf('='))
  (l, r.tail)
}

while (true) {
  val input = readLine(prompt)
  parse(input) match {
    case Nil => // do nothing

    // shell built-in commands
    case "exit" :: xs =>
      println("Program will exit...")
      sys.exit(0)

    case "cd" :: Nil => // do nothing
    case "cd" :: x :: _ =>
      val dest = getFile(pos)(x)
      if (!dest.isDirectory) println("cd: no such directory")
      else pos = dest

    case "alias" :: Nil =>
      println(alias_table.mkString("\n"))

    case "alias" :: x :: _ =>
      if (!x.contains('=')) {
        if (alias_table.contains(x)) println(s"$x=${alias_table(x)}")
      }
      else {
        val (name, value) = splitByEq(x)
        println(s"alias: $name -> $value")
        alias_table += name -> value
      }

    case in =>
      // result (0 or else)
      val result = try system(aliased(in))
      catch {
        case e: java.io.IOException =>
          println(s"permission denied: $input")
          1
      }
  }
}
