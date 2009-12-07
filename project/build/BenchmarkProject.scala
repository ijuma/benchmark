package me.juma.benchmark

import sbt._

class BenchmarkProject (info: ProjectInfo) extends DefaultProject(info) {
  import java.io.File
  //TODO Should use / instead of messy nested File constructors
  def scalaHome = new File(new File(System.getProperty("user.home"), "src"), "scala")
  override def localScala = defineScala("2.8.0-quick", new File(new File(scalaHome, "build"), "pack")) :: Nil
}
