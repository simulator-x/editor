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
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 3/13/11
 * Time: 1:42 PM
 */
package simx.components.editor.gui

import swing._
import scala.collection.mutable
import java.io.File
import simx.components.editor.filesystem._
import swing.event.{ButtonClicked, SelectionChanged}
import simx.components.editor.AvailableViewsChanged
import simx.core.svaractor.SVarActor
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag


class SVarSetterPanel(
  val node: EnSVar,
  sVarIdentifier: Symbol,
  man: ClassTag[_],
  sVarSetters: mutable.Map[Symbol,
  DynamicReloader[SVarSetterGeneratorBase]],
  availableSetters: mutable.Set[DynamicReloader[SVarSetterGeneratorBase]],
  editorActor: SVarActor.Ref,
  sourceDir: File
) extends DetailsView {

  private var updateFunc: () => Unit = () => {}

  /*Minor Components*/
  val editButton = new Button("e"){
    maximumSize = new Dimension(20, 20)
    minimumSize = new Dimension(20, 20)
    preferredSize = new Dimension(20, 20)

    peer.setMargin(new java.awt.Insets(1, 2, 1, 2))

    listenTo(this)
    reactions += {
      case ButtonClicked(source) if source == this =>
        val selectedItem: DynamicReloader[SVarSetterGeneratorBase]  = list.selection.item
        if(selectedItem != SVarViewPanel.default)
          selectedItem.showInIDE()
    }

  }
  val addButton = new Button("+"){
    maximumSize = new Dimension(20, 20)
    minimumSize = new Dimension(20, 20)
    preferredSize = new Dimension(20, 20)

    peer.setMargin(new java.awt.Insets(1, 2, 1, 2))

    listenTo(this)

    reactions += {
      case ButtonClicked(source) if source == this =>
        val typeName = man.toString
        val shortClassName =
          typeName.substring(typeName.lastIndexOf('.') + 1) +
          "SVarSetterGenerator" +
          java.util.UUID.randomUUID.toString.replace("-", "")
        val fullClassName = "simx.components.editor.gui." + shortClassName
        val file = new File(sourceDir, shortClassName + ".scala")
        val dr = new DynamicReloader[SVarSetterGeneratorBase] (
          ClassFile(file, fullClassName),
          SVarSetterPanel.compilerSettings(sourceDir),
          Some(SVarSetterPanel.generateClassTemplateFor(shortClassName, typeName)),
          (svarViewGenBaseOption: Option[SVarSetterGeneratorBase]) => {list.repaint(); updateSVarView()}
        ) {
          override def toString: String = {
            getCurrentClass match {
              case Some(clazz) => clazz.toString()
              case None => "Class loaded with errors"
            }
          }
        }
        availableSetters.add(dr)
        publish(AvailableViewsChanged())
    }
  }
  val removeButton = new Button("-"){
    maximumSize = new Dimension(20, 20)
    minimumSize = new Dimension(20, 20)
    preferredSize = new Dimension(20, 20)

    peer.setMargin(new java.awt.Insets(1, 2, 1, 2))

    listenTo(this)
    reactions += {
      case ButtonClicked(source) if source == this =>
        val selectedItem: DynamicReloader[SVarSetterGeneratorBase]  = list.selection.item

        availableSetters.remove(selectedItem)
        sVarSetters.get(sVarIdentifier).collect{
          case dr => if(dr == selectedItem) sVarSetters.remove(sVarIdentifier)
        }
        println("Removing file " + selectedItem.classFile.file.getName)
        selectedItem.classFile.file.deleteOnExit()
        publish(AvailableViewsChanged())

    }
  }

  private val initialListItems =
    if(availableSetters.isEmpty) Seq(SVarSetterPanel.defaultListItem) else availableSetters.toSeq

  val list = new AdvComboBox[DynamicReloader[SVarSetterGeneratorBase]](initialListItems){
    listenTo(this)

    sVarSetters.get(sVarIdentifier).collect{
      case dr =>
        selection.item = dr
    }

    reactions += {
      case SelectionChanged(source) if source == this =>
        val selectedItem: DynamicReloader[SVarSetterGeneratorBase]  = selection.item
        //println("Updating " + sVarIdentifier.name)
        sVarSetters.update(sVarIdentifier, selectedItem)
        updateSVarView()
    }

    if((getItems.size == 1) && (getItems(0) == SVarSetterPanel.defaultListItem)) enabled = false
  }

  private def updateSVarView(){
    if(list.enabled) {
      if(list.selection.item.getCurrentClass.isDefined) {
        val newSVarSetterBase: SVarSetterBase = list.selection.item.getCurrentClass.get.generate
        newSVarSetterBase.changeRegisteredSvarTo(node.sVar)
        pane.contents = newSVarSetterBase.component
        list.selection.item.onLoad =
          (svarViewGenBaseOption: Option[SVarSetterGeneratorBase]) => {list.repaint(); updateSVarView()}
        updateFunc = () => {newSVarSetterBase.internalUpdate(node.children.head.asInstanceOf[EnSVarValue].value)}
        updateFunc.apply()
      }
      else {
        pane.contents = new Label("Error on loading SVar setter")
      }
    }
    else
      pane.contents = new Label("No SVar setter selected.")
  }

  val pane = new ScrollPane
  /*Minor Components END*/

  //Set the sVarView
  updateSVarView()

  /*Main Component*/
  val component = new GridBagPanel() {

    listenTo(addButton)
    listenTo(removeButton)

    val gbc = new Constraints()
    gbc.fill = GridBagPanel.Fill.Both
    gbc.gridy = 0
    gbc.gridx = 0
    gbc.weighty = 1.0
    gbc.weightx = 1.0
    add(pane, gbc)
    gbc.gridy = 1
    gbc.gridx = 0
    gbc.weighty = 0.0
    gbc.weightx = 0.0
    add(new BoxPanel(Orientation.Horizontal) {
      contents += addButton
      contents += removeButton
      contents += editButton
      contents += list
    }, gbc)
    border = Swing.EmptyBorder(10, 10, 10, 10)

    reactions += {
      case event: AvailableViewsChanged =>
        val sel = list.selection.item
        val listItems = if(availableSetters.isEmpty) Seq(SVarSetterPanel.defaultListItem) else availableSetters.toSeq
        list.setItems(listItems)
        if(list.getItems.contains(sel)) list.selection.item = sel

        if((list.getItems.size == 1) && (list.getItems(0) == SVarSetterPanel.defaultListItem))
          list.enabled = false
        else
          list.enabled = true
    }
  }

  //TODO: refactor class structure. Update is not used for a setter
  def update() { updateFunc.apply() }
  /*Main component END*/

}

object SVarSetterPanel {

  val defaultListItem =
    new DynamicReloader[SVarSetterGeneratorBase](
      classFile = ClassFile(null, ""),
      compilerSettings = CompilerSettings(null, List[File](), List[File]()),
      classTemplate = None,
      onLoad = (o : Option[SVarSetterGeneratorBase]) => {})
    {
      private val viewGen = Some(new SVarSetterGeneratorBase {
        val name = "No setter available."

        def generate: SVarSetterBase = {
          throw new Exception {
            override def toString = "SVarSetterPanel.defaultListItem this should not be called"
          }
          null.asInstanceOf[SVarSetterBase]
        }
      })

      currentClass = viewGen

      override def showInIDE() {}
      override protected def init() {}

      override def toString: String = {
        currentClass match {
          case Some(clazz) => clazz.toString()
          case None => "Class loaded with errors"
        }
      }
    }

  def compilerSettings(sourceDir: File) = CompilerSettings(
    outputPath = sourceDir,
    additionalClassPaths = SimXProperties.simXLibs ::: SimXProperties.simXClassPath ::: SimXProperties.scalaLibs
  )

  def generateClassTemplateFor(className: String, typeName: String): String = {
    val shortTypeName = typeName.substring(typeName.lastIndexOf('.') + 1)

"""//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import """ + typeName + """

class """ + className + """ extends SVarSetterGenerator[""" + shortTypeName + """] {

  def generate: SVarSetter[""" + shortTypeName + """] = new SVarSetter[""" + shortTypeName + """] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the SVar setter.
     *  Call
     *  setSvar(newValue: """ + shortTypeName + """): Unit
     *  to set new svar value.
     */
    //Todo: Implement yourself!
    val component = new Button("Click me to set the svar") {
      listenTo(this)
      reactions += {
        case event: event.ButtonClicked =>
          Dialog.showMessage(null, "Use 'e' button to edit the code", "Setter not implemented yet")

          /*
          setSvar(...)
          */
      }
    }

    /**
     * Override update if you want to use the current value of the SVar.
     * This function is initially called once and then every time the value of the SVar changes.
     */
    // override def update(newValue: """ + shortTypeName + """) {}

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = """" + shortTypeName + """ Setter"

}"""
  }

}