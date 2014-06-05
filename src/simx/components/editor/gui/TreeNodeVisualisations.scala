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
 * Date: 2/11 and 08/13
 */
package simx.components.editor.gui

import scala.collection.mutable
import simx.components.editor.filesystem.DynamicReloader
import simx.core.ontology.SVarDescription
import simx.components.editor.filesystem.ClassFile
import swing.{GridPanel, Label}
import java.io.{FileWriter, File}
import xml.{PrettyPrinter, Elem}
import simx.core.svaractor.SVarActor
import scala.xml

/**
 *  Manages the visualisations for sVars and sVals
 *
 * @param configFile: The file from/to which the settings are stored/loaded
 * @param editorActor: The corresponding editor component
 */
class TreeNodeVisualisations(configFile: File, editorActor: SVarActor.Ref) {

  private val configFileDir = configFile.getParentFile
  private val sourceDir = new File(configFileDir, "src")
  if(!sourceDir.exists()) sourceDir.mkdir()

  private val debug = false

  private def makeMap[A, B]: mutable.Map[A, B] = {
    new mutable.HashMap[A, B] with mutable.SynchronizedMap[A, B]
  }

  private def makeSet[A]: mutable.Set[A] = {
    new mutable.HashSet[A] with mutable.SynchronizedSet[A]
  }

  //TODO Update all windows on change
  private val sVarViews: mutable.Map[Symbol, DynamicReloader[SVarViewGeneratorBase]] =
    makeMap[Symbol, DynamicReloader[SVarViewGeneratorBase]]
  private val typeViews: mutable.Map[Symbol, mutable.Set[DynamicReloader[SVarViewGeneratorBase]]] =
    makeMap[Symbol, mutable.Set[DynamicReloader[SVarViewGeneratorBase]]]

  //TODO Update all windows on change
  private val sVarSetters: mutable.Map[Symbol, DynamicReloader[SVarSetterGeneratorBase]] =
    makeMap[Symbol, DynamicReloader[SVarSetterGeneratorBase]]
  private val typeSetters: mutable.Map[Symbol, mutable.Set[DynamicReloader[SVarSetterGeneratorBase]]] =
    makeMap[Symbol, mutable.Set[DynamicReloader[SVarSetterGeneratorBase]]]

  if(configFile.exists) loadConfiguration()

  def detailsViewFor(tn: TreeNode): DetailsView = tn match {
    case n: EnRoot => simpleLabelView(n, n.appName _)
    case n: EnEntity => simpleLabelView(n, "Entity")
    case n: EnSVarCollection => simpleLabelView(n, "SVars")
    case n: EnSVar =>
      val typeInfo = n.sVar.containedValueManifest
      //Check manifests
      if(debug)
        if(SVarDescription(Symbol(typeInfo.toString)).head.typeinfo != n.sVar.containedValueManifest) {
          println("Warning: Manifest difference between " +
            SVarDescription(Symbol(typeInfo.toString)).head.typeinfo + " and " + n.name.name + "!")
          println("Onto: " +
            SVarDescription(Symbol(typeInfo.toString)).head.classTag.runtimeClass.getCanonicalName)
          println("SVar: " +
            n.sVar.containedValueManifest)
        }

      if(!typeViews.contains(Symbol(typeInfo.toString)))
        typeViews += (Symbol(typeInfo.toString) -> makeSet[DynamicReloader[SVarViewGeneratorBase]])
      if(!typeSetters.contains(Symbol(typeInfo.toString)))
        typeSetters += (Symbol(typeInfo.toString) -> makeSet[DynamicReloader[SVarSetterGeneratorBase]])

      val view = new SVarViewPanel(
        node = n.enSVarValue,
        sVarIdentifier = n.name,
        man = n.sVar.containedValueManifest,
        sVarViews = sVarViews,
        availableViews = typeViews(Symbol(typeInfo.toString)),
        editorActor = editorActor,
        sourceDir = sourceDir
      )

      val setter = new SVarSetterPanel(
        node = n,
        sVarIdentifier = n.name,
        man = n.sVar.containedValueManifest,
        sVarSetters = sVarSetters,
        availableSetters = typeSetters(Symbol(typeInfo.toString)),
        editorActor = editorActor,
        sourceDir = sourceDir
      )

      new DetailsView{
        val component = new GridPanel(2, 1) {contents += setter.component; contents+= view.component}
        def update(): Unit = {view.update(); setter.update()}
        val node = n.enSVarValue
      }
    case n: EnSVarValue => simpleLabelView(n, n.value.toString)
    case n: EnCreateParamSet => simpleLabelView(n, "EntityAspect " + n.cps.semantics +  " of " + n.component.name)
    case n: EnCreateParam =>
      //Check manifests
      if(debug)
        if(SVarDescription(Symbol(n.cp.typedSemantics.typeinfo.toString)).head.typeinfo !=
          n.cp.containedValueManifest)
        {
          println("Warning: Manifest difference between " +
            SVarDescription(Symbol(n.cp.typedSemantics.typeinfo.toString)).head.sVarIdentifier + " and " +
            n.cp.typedSemantics.asConvertibleTrait.sVarIdentifier.name + "!")
          println("Onto: " + SVarDescription(Symbol(n.cp.typedSemantics.asConvertibleTrait.typeinfo.toString)).
            head.classTag.runtimeClass.getCanonicalName)
          println("SVar: " + n.cp.containedValueManifest.runtimeClass.getCanonicalName)
        }

      if(!typeViews.contains(Symbol(n.cp.typedSemantics.typeinfo.toString)))
        typeViews += (Symbol(n.cp.typedSemantics.typeinfo.toString)
          -> makeSet[DynamicReloader[SVarViewGeneratorBase]])

      new SVarViewPanel(
        node = n.enCreateParamValue,
        sVarIdentifier = n.cp.typedSemantics.asConvertibleTrait.sVarIdentifier,
        man = n.cp.typedSemantics.typeinfo,
        sVarViews = sVarViews,
        availableViews = typeViews(Symbol(n.cp.typedSemantics.typeinfo.toString)),
        editorActor = editorActor,
        sourceDir = sourceDir
      )
    case n: EnCreateParamValue => simpleLabelView(n, n.toString)
    case n => simpleLabelView(n, "Unknown")
  }

  private def simpleLabelView(tn: TreeNode, text: String): DetailsView =
    simpleLabelView(tn, () => text)

  private def simpleLabelView(tn: TreeNode, f: () => String): DetailsView =
    new DetailsView {
      val component = new Label
      def update() = {component.text = f()}
      val node = tn
    }

  private def loadConfiguration() = {
    println("[Editor] Loading configuration from: " + configFile.getCanonicalPath)
    val xml = scala.xml.XML.loadFile(configFile)

    //Views
    val typedSVarViews = xml \\ "TypedSvarView"
    for(typedSVarView <- typedSVarViews) {
      val typeInfo = Symbol((typedSVarView \ "typeInfo").text)
      val sVarViewSet = makeSet[DynamicReloader[SVarViewGeneratorBase]]
      val sVarViewElems = typedSVarView \\ "SVarView"
      for(sVarViewElem <- sVarViewElems) {
        val file = new File(configFileDir, (sVarViewElem \ "fileName").text)
        val className = (sVarViewElem \ "className").text
        val dr = new DynamicReloader[SVarViewGeneratorBase] (
          ClassFile(file, className),
          SVarViewPanel.compilerSettings(sourceDir)
        )
        sVarViewSet.add(dr)
        val usedForSVars = (sVarViewElem \ "usedForSVars") \ "sVarIdentifier"
        for(useElem <- usedForSVars) {
          val sVarId = Symbol(useElem.text)
          sVarViews += sVarId -> dr
        }
      }
      if(!sVarViewSet.isEmpty) typeViews += typeInfo -> sVarViewSet
    }

    //Setters
    val typedSVarSetters = xml \\ "TypedSvarSetter"
    for(typedSVarSetter <- typedSVarSetters) {
      val typeInfo = Symbol((typedSVarSetter \ "typeInfo").text)
      val sVarSetterSet = makeSet[DynamicReloader[SVarSetterGeneratorBase]]
      val sVarSetterElems = typedSVarSetter \\ "SVarSetter"
      for(sVarSetterElem <- sVarSetterElems) {
        val file = new File(configFileDir, (sVarSetterElem \ "fileName").text)
        val className = (sVarSetterElem \ "className").text

        val dr = new DynamicReloader[SVarSetterGeneratorBase] (
          ClassFile(file, className),
          SVarSetterPanel.compilerSettings(sourceDir)
        )
        sVarSetterSet.add(dr)
        val usedForSVars = (sVarSetterElem \ "usedForSVars") \ "sVarIdentifier"
        for(useElem <- usedForSVars) {
          val sVarId = Symbol(useElem.text)
          sVarSetters += sVarId -> dr
        }
      }
      if(!sVarSetterSet.isEmpty) typeSetters += typeInfo -> sVarSetterSet
    }
  }

  def saveConfiguration() = {
    println("Saving configuration to: " + configFile.getCanonicalPath)
    val configFileDir = configFile.getParentFile

    //Views
    val tvs = for(typeView <- typeViews.filter(_._2.nonEmpty).toSeq.sortWith(_._1.name < _._1.name)) yield {
      val typeInfo = <typeInfo>{typeView._1.name}</typeInfo>
      val views = for(view <- typeView._2.toSeq.sortWith(_.classFile.className < _.classFile.className)) yield {
        val fileName = <fileName>{configFileDir.toURI.relativize(view.classFile.file.toURI).getPath}</fileName>
        val className = <className>{view.classFile.className}</className>
        val usedToVisualizeSet = sVarViews.filter((symbolDrTuple) => {symbolDrTuple._2 == view})
        val usedToVisualize = for(symbolDrTuple <- usedToVisualizeSet.toSeq.sortWith(_._1.name < _._1.name)) yield {
          <sVarIdentifier>{symbolDrTuple._1.name}</sVarIdentifier>
        }
        val usedForSVars = new Elem(null, "usedForSVars", xml.Null, xml.TopScope, false, usedToVisualize.toSeq:_*)
        new Elem(null, "SVarView", xml.Null, xml.TopScope, false, fileName, className, usedForSVars)
      }
      val viewsElem = new Elem(null, "SVarViews", xml.Null, xml.TopScope, false, views.toSeq:_*)
      new Elem(null, "TypedSvarView", xml.Null, xml.TopScope, false, typeInfo, viewsElem)
    }
    val typedSvarViews = new Elem(null, "TypedSvarViews", xml.Null, xml.TopScope, false, tvs.toSeq:_*)

    //Setters
    val tss = for(typeSetter <- typeSetters.filter(_._2.nonEmpty).toSeq.sortWith(_._1.name < _._1.name)) yield {
      val typeInfo = <typeInfo>{typeSetter._1.name}</typeInfo>
      val setters = for(setter <- typeSetter._2.toSeq.sortWith(_.classFile.className < _.classFile.className)) yield {
        val fileName = <fileName>{configFileDir.toURI.relativize(setter.classFile.file.toURI).getPath}</fileName>
        val className = <className>{setter.classFile.className}</className>
        val usedToSetSet = sVarSetters.filter((symbolDrTuple) => {symbolDrTuple._2 == setter})
        val usedToSet = for(symbolDrTuple <- usedToSetSet.toSeq.sortWith(_._1.name < _._1.name)) yield {
          <sVarIdentifier>{symbolDrTuple._1.name}</sVarIdentifier>
        }
        val usedForSVars = new Elem(null, "usedForSVars", xml.Null, xml.TopScope, false, usedToSet.toSeq:_*)
        new Elem(null, "SVarSetter", xml.Null, xml.TopScope, false, fileName, className, usedForSVars)
      }
      val viewsElem = new Elem(null, "SVarSetters", xml.Null, xml.TopScope, false, setters.toSeq:_*)
      new Elem(null, "TypedSvarSetter", xml.Null, xml.TopScope, false, typeInfo, viewsElem)
    }
    val typedSvarSetters = new Elem(null, "TypedSvarSetters", xml.Null, xml.TopScope, false, tss.toSeq:_*)

    val xmlRoot = new Elem(null, "EditorConfig", xml.Null, xml.TopScope, false, typedSvarViews, typedSvarSetters)

    val fw = new FileWriter(configFile)
    //The conversion to String and back is a quick and dirty way to deal with the "null"s in the
    //above Elem constructors, that lead to a NullPointerException in the PrettyPrinter
    // but not in the toString method of Node
    fw.write(new PrettyPrinter(1000,2).format(scala.xml.XML.loadString(xmlRoot.toString())))
    fw.close()
  }

}
