scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

compileOrder := CompileOrder.JavaThenScala

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")
}

scalacOptions += "-P:continuations:enable"

ivyXML := scala.xml.XML.load( editor.base + "/ivy.xml" ) \ "dependencies"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")
