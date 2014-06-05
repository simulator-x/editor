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
 * Date: 2/28/11
 * Time: 7:47 PM
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


class SVarViewPanel(
  val node: TreeNodeWithValue,
  sVarIdentifier: Symbol,
  man: ClassTag[_],
  sVarViews: mutable.Map[Symbol,
  DynamicReloader[SVarViewGeneratorBase]],
  availableViews: mutable.Set[DynamicReloader[SVarViewGeneratorBase]],
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
        val selectedItem: DynamicReloader[SVarViewGeneratorBase]  = list.selection.item
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
          "SVarViewGenerator" +
          java.util.UUID.randomUUID.toString.replace("-", "")
        val fullClassName = "simx.components.editor.gui." + shortClassName
        val file = new File(sourceDir, shortClassName + ".scala")
        val dr = new DynamicReloader[SVarViewGeneratorBase] (
          ClassFile(file, fullClassName),
          SVarViewPanel.compilerSettings(sourceDir),
          Some(SVarViewPanel.generateClassTemplateFor(shortClassName, typeName)),
          (svarViewGenBaseOption: Option[SVarViewGeneratorBase]) => {list.repaint(); updateSVarView()}
        ) {
          override def toString: String = {
            getCurrentClass match {
              case Some(clazz) => clazz.toString()
              case None => "Class loaded with errors"
            }
          }
        }
        availableViews.add(dr)
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
        val selectedItem: DynamicReloader[SVarViewGeneratorBase]  = list.selection.item

        if(selectedItem != SVarViewPanel.default){
          availableViews.remove(selectedItem)
          sVarViews.get(sVarIdentifier).collect{
            case dr => if(dr == selectedItem) sVarViews.remove(sVarIdentifier)
          }
          println("Removing file " + selectedItem.classFile.file.getName)
          selectedItem.classFile.file.deleteOnExit()
          publish(AvailableViewsChanged())
        }
    }
  }

  val list =
    new AdvComboBox[DynamicReloader[SVarViewGeneratorBase]](Seq(SVarViewPanel.default).union(availableViews.toSeq)){
      listenTo(this)

      sVarViews.get(sVarIdentifier).collect{
        case dr =>
          selection.item = dr
      }

      reactions += {
        case SelectionChanged(source) if source == this =>
          val selectedItem: DynamicReloader[SVarViewGeneratorBase]  = selection.item
          //println("Updating " + sVarIdentifier.name)
          sVarViews.update(sVarIdentifier, selectedItem)
          updateSVarView()
      }
    }

  private def updateSVarView(){
    val newSVarViewBase =
      list.selection.item.getCurrentClass.getOrElse(SVarViewPanel.default.getCurrentClass.get).generate
    pane.contents = newSVarViewBase.component
    updateFunc = () => {newSVarViewBase.internalUpdate(node.value)}
    updateFunc.apply()

    list.selection.item.onLoad =
      (svarViewGenBaseOption: Option[SVarViewGeneratorBase]) => {list.repaint(); updateSVarView()}
  }

  val pane = new ScrollPane
  /*Minor Components END*/

  //Set the SVarView
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
        list.setItems(Seq(SVarViewPanel.default).union(availableViews.toSeq))
        if(list.getItems.contains(sel)) list.selection.item = sel
    }
  }

  def update() { updateFunc.apply() }
  /*Main component END*/

}

class DefaultSvarView extends SVarView[Any] {
  def update(sVarValue: Any) { component.text = sVarValue.toString }
  val component = new Label
}

object SVarViewPanel {

  val default =
    new DynamicReloader[SVarViewGeneratorBase](
      classFile = ClassFile(null, ""),
      compilerSettings = CompilerSettings(null, List[File](), List[File]()),
      classTemplate = None,
      onLoad = (o : Option[SVarViewGeneratorBase]) => {})
    {

      private val viewGen = Some(new SVarViewGenerator[Any] {
        val name = "Default View"
        private val view = new DefaultSvarView
        def generate = view
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

class """ + className + """ extends SVarViewGenerator[""" + shortTypeName + """] {

  def generate: SVarView[""" + shortTypeName + """] = new SVarView[""" + shortTypeName + """] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */
    //Todo: Implement yourself!
    val component = new Label

    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */
    //Todo: Implement yourself!
    def update(sVarValue: """ + shortTypeName + """) {
      component.text = sVarValue.toString
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = """" + shortTypeName + """ View"

}"""
  }

}