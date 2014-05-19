/*
 * Copyright 2012 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.editor.filesystem

import java.io.File
import util.matching.Regex
import swing.{FileChooser, Dialog}
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 6/29/12
 * Time: 3:37 PM
 */

object SimXProperties {

  checkHome("simx.home")((simXHome) => {
    new File(simXHome, "core").exists() && new File(simXHome, "components").exists()
  })

  val simXHome = new File(System.getProperty("simx.home", ""))

  val pathSeparator = if(isWindows) ";" else ":"

  val scalaVersion = try {
    Source.fromFile(new File(simXHome, "project/Build.scala")).getLines().
      filter(_.contains("scalaVersion :=")).next().split('"').apply(1)
  } catch {
    case _: Throwable =>
      throw new Exception("[error][SimXProperties] Could not determine scala version from file 'Build.scala'.")
  }

  private val _scalaLibs = "scala-library-" :: "scala-compiler-" :: Nil

  val scalaLibs : List[File] = System.getProperty("java.class.path", "").split(pathSeparator).filter( p =>
      //Check if one of the _scalaLibs entries matches the current path
      _scalaLibs.exists( libPrefix => p.matches(""".*""" + libPrefix + scalaVersion + """\.jar""") )
    ).toList.map(new File(_))

  private val _simXLibsExcludes = """.*idea_rt.jar""" :: """.*target/scala/classes""" :: _scalaLibs.map(".*" + _ + ".*")

  /**
   * Contains core, component and application libs as well as JDK and scala libs.
   */
  val simXLibs: List[File] = System.getProperty("java.class.path", "").split(pathSeparator).filterNot(
    p => _simXLibsExcludes.exists( exclude => p.matches(exclude)) ).toList.map(new File(_))

  val simXClassPath =
    ("core" :: "applications" :: "components" :: Nil)
      //Find all 'target' directories
      .map(dirName => findDirs(new File(simXHome, dirName), """^target$""".r)).flatten
      //Check if there is a 'scala' directory present and generate a respective java.io.File
      .map(checkAndMapSbtTargetDir).flatten

  lazy val scalaEditor: String =
    if(isMac) "open" else askForEditor().getAbsolutePath


  lazy val editorSettingsRoot: File = {
    val wd = new File(new File("").getAbsolutePath)
    val localSettings = new File(wd, "editor-settings")
    val globalSettings = new File(simXHome, "components/editor/editor-settings")
    //Try local working directory
    if(localSettings.exists()) localSettings
    //Assume simx-application structure (like in simx-production.git)
    else if(globalSettings.exists()) globalSettings
    //Ask user
    else {
      Dialog.showMessage(
        null,
        "Could not find the editor-settings root directory. Tried \n" +
          localSettings.getAbsolutePath + " and\n" +
          globalSettings.getAbsolutePath + ".\n\n" +
          "Manual specification required.",
        "simx.components.editor.filesystem.SimXProperties",
        Dialog.Message.Warning)
      val fc = new FileChooser(wd)
      fc.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      fc.showDialog(null, "Choose editor-settings directory") match {
        case FileChooser.Result.Approve => fc.selectedFile
        case _ => throw new java.lang.Exception("No editor-settings root directory selected.")
      }
    }
  }

  private def askForEditor(): File = {
    Dialog.showMessage(
      null,
      "Please select a scala editor.",
      "Scala editor required",
      Dialog.Message.Info)
    val fc = new FileChooser()
    fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
    fc.multiSelectionEnabled = false
    fc.showDialog(null, "Choose a scala editor") match {
      case FileChooser.Result.Approve => fc.selectedFile
      case _ => throw new java.lang.Exception("No scala editor selected.")
    }
  }

  def findFiles(baseDir: File, r: Regex): List[File] = {
    val thisDir = baseDir.listFiles.toList
    thisDir.filter(
      (f: File) => {
        (!f.isDirectory) && r.findFirstIn(f.getName).isDefined
      }) ::: thisDir.filter(_.isDirectory).flatMap(findFiles(_,r))
  }

  def findDirs(baseDir: File, r: Regex): List[File] = {
    val thisDir = baseDir.listFiles.toList
    thisDir.filter(
      (f: File) => {
        f.isDirectory && r.findFirstIn(f.getName).isDefined
      }) ::: thisDir.filter(_.isDirectory).flatMap(findDirs(_,r))
  }

  def isWindows = System.getProperty("os.name").toLowerCase.indexOf("win") >= 0

  def isMac = System.getProperty("os.name").toLowerCase.indexOf("mac") >= 0

  def isUnix =
    (System.getProperty("os.name").toLowerCase.indexOf("nix") >= 0) ||
    (System.getProperty("os.name").toLowerCase.indexOf("nux") >= 0)

  private def checkHome(sysPropName: String)(additionalCheck: (File) => Boolean = (f) => true)
  {
    if(System.getProperty(sysPropName, "") == "") {
      val wd = new File(new File("").getAbsolutePath)
      //Search for SimX home in the parent directories
      var home = wd
      1 to 10 foreach{ _ =>
        home = home.getParentFile
        if(home.exists() && additionalCheck(home)) {
          System.setProperty(sysPropName, home.getAbsolutePath)
          return
        }
      }
      throw new java.lang.Exception(
        "Could not find " + sysPropName + ". Tried " +
        home.getAbsolutePath +
        ". Please set the system property \"" + sysPropName + "\" correctly."
      )
    }
  }

  private def checkAndMapSbtTargetDir(dir: File): List[File] =
    dir.listFiles.toList.filter(_.getName == "scala").map(scalaDir => new File(scalaDir, "classes"))
}
