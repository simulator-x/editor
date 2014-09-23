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

/*
 * User: Martin Fischbach
 * Date: 3/11 and 8/13
 */
package simx.components.editor.filesystem

//import tools.nsc.util.ScalaClassLoader.URLClassLoader
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import java.io.{FileWriter, File}
import simx.core.helper.Loggable
import scala.tools.nsc.{Settings, Global}
import java.lang.management.ManagementFactory
import java.awt.Desktop

case class ClassFile(file: File, className: String)
case class CompilerSettings(outputPath: File, additionalClassPaths: List[File], additionalSourceFiles: List[File] = Nil)

object DynamicReloader {
  private[filesystem] val emitToLog = true

  private val arguments =
    ManagementFactory.getRuntimeMXBean.getInputArguments.toArray.toList.map(_.asInstanceOf[String])
  private var checked = false

  private[filesystem] def checkPermSize() {
    if(!checked) {
      if(!arguments.exists(_.matches("""-XX:MaxPermSize=.*""")))
        System.err.println("[warn][DynamicReloader] Class loading may exceed the default memory. " +
          "Use the VM option'-XX:MaxPermSize=' to increase the memory to eg. '128m'.")
      checked = true
    }
  }

  //TODO: Access via an management actor (like the simx.components.renderer.jvr.ResourceManager)
  private var compileEnvironments = Map[CompilerSettings, Global]()
  private[filesystem] def compileEnvironmentFor(cs: CompilerSettings) = {
    //if(!compileEnvironments.contains(cs)) {
      val classpath = cs.additionalClassPaths.mkString(SimXProperties.pathSeparator)
      val settings = new Settings()
      settings.deprecation.value = true // enable detailed deprecation warnings
      settings.unchecked.value = true   // enable detailed unchecked warnings
      settings.outputDirs.setSingleOutput(cs.outputPath.getCanonicalPath)
      settings.usejavacp.value = true
      settings.classpath.value = classpath
      //compileEnvironments += cs -> new Global(settings)
    //}
    //compileEnvironments(cs)
    new Global(settings)
  }
}

class DynamicReloader[T] (
  val classFile: ClassFile,
  compilerSettings: CompilerSettings,
  classTemplate: Option[String] = None,
  var onLoad: (Option[T]) => Unit = (loaded: Option[T])=> {},
  pollingIntervalInMillis: Long = 1000L
) extends Loggable {

  protected var currentClass: Option[T] = None
  private lazy val fileMonitor = new FileMonitor(pollingIntervalInMillis)

  DynamicReloader.checkPermSize()
  init()

  def getCurrentClass: Option[T] = currentClass

  protected def init() {
    classTemplate.collect{
      case template =>
        val fw = new FileWriter(classFile.file)
        fw.write(template)
        fw.close()
    }

    if(!classFile.file.exists)
      throw new java.io.FileNotFoundException(
        this.toString + " could not find the file '" + classFile.file.getCanonicalPath + "'.")

    fileMonitor.addFile(classFile.file)
    fileMonitor.addListener(new FileListener{
      def fileChanged(p1: File) {
        emitln("File " + p1.getName + " changed. Recompiling and loading ..." )
        load().collect{
          case clazz =>
            currentClass = Some(clazz)
            onLoad(currentClass)
        }
      }
    })
    currentClass = load()
    onLoad(currentClass)
  }

  private def load(): Option[T] =  {
    compile()

    emit("Loading " + classFile.file.getName + "'... " )
    val byteCodeDirectory = new File(compilerSettings.outputPath.getCanonicalPath).toURI.toURL
    val classLoader = new URLClassLoader(Array(byteCodeDirectory), this.getClass.getClassLoader)

    try {
      val clazz = classLoader.loadClass( classFile.className )
      val loadedObject: T = clazz.asInstanceOf[Class[T]].newInstance
      emitln("successful.")
      Some(loadedObject)
    } catch {
      case e : Throwable =>
        emitln("Error: \n" + e)
        None
    }
  }

  //checks if the <classFile.file> file needs to be recompiled
  private def compile() {

    //get the path of classfiles
    (io.Source.fromFile(classFile.file).getLines.find{line => line.startsWith("package ")}    ) match {
      case Some(line) =>
        val sb = new StringBuilder
        sb.append(compilerSettings.outputPath.getCanonicalPath + File.separator)
        val packageName = line.split(' ').reverse.head.split('.')
        sb.append(packageName.head)
        packageName.tail.foreach{
          dir => sb.append(File.separator + dir)
        }

        //get the class file
        val compiledClassFile = new File(sb.toString, classFile.file.getName.split('.').init :+ "class" mkString ".")

        //recompile whether no class filer exists or it's not up-to-date.
        if(!compiledClassFile.exists() || compiledClassFile.lastModified() < classFile.file.lastModified()) {
          if(!compiledClassFile.exists()) emitln("Initially compiling: " + classFile.file.getName + "'... ")
          else emit("Recompiling: " + classFile.file.getName + "'... ")

          //Todo compiling deprecated use SBT?
          val env = DynamicReloader.compileEnvironmentFor(compilerSettings)
          new env.Run().compile(classFile.file.getCanonicalPath :: Nil)
          emitln("done")
        }
        else emitln("Skip compiling: no changes in " +classFile.file.getName)
      case None => println("[error][DynamicReloader] No package path found in .scala file: " + classFile.file.getName )
      }



  }

  def showInIDE() {
    if (Desktop.isDesktopSupported) {
      try{
        Desktop.getDesktop.open(classFile.file)
      }catch{
        case e: java.io.IOException =>
          val extension = classFile.file.getName.split('.').toList.tail.headOption.getOrElse("scala")
          println("[error][DynamicReloader] Could not open file for editing.")
          println("\tMake sure you have a default program set to open ." + extension+ " files with.")
      }
    }
    else {
      println("[error][DynamicReloader] Could not open the following file for editing:")
      println("\tclassFile.file.getAbsolutePath")
    }
    //Windows alternative
    //Runtime.getRuntime.exec("rundll32 SHELL32.DLL,ShellExec_RunDLL "+ classFile.file.getCanonicalPath)
    //Old Impl
    //val cmd: List[String] = SimXProperties.scalaEditor :: classFile.file.getCanonicalPath :: Nil
    //val child = Runtime.getRuntime.exec(cmd.toArray)
    //child.waitFor
  }

  def emitln(s: String) { if(DynamicReloader.emitToLog) info(s) else println(s) }
  def emit(s: String) { if(DynamicReloader.emitToLog) info(s) else print(s) }

  override def toString: String = {
    getCurrentClass match {
      case Some(clazz) => clazz.toString
      case None => "[DynamicReloader] Class loaded with errors"
    }
  }
}