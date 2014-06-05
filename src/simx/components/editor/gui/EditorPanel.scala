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

package simx.components.editor.gui

import scala.swing._
import scala.collection._
import simx.core.entity.Entity
import java.awt.{Color, Dimension}
import simx.components.editor._
import javax.swing.ImageIcon
import simx.components.editor.filesystem.SimXProperties
import scala.swing.ListView.Renderer
import scala.swing.event.MousePressed
import java.io.File
import simx.core.worldinterface.eventhandling.Event
import simx.core.ontology.types

/**
* User: Martin Fischbach
* Date: 9/11 and 8/13
*/
class EditorPanel(editorComponentActor: Editor) extends MainFrame with SynchronizedReactor {
  thisEditorPanel =>

  private val treeRoot = new EnRoot("SimX App")
  private var sVarToNode = mutable.Map[(Entity, Symbol), EnSVarValue]()
  private var displayedNodes = Map[TreeNode, (Frame, DetailsView)]()
  private var selectedPath: List[TreeNode] = treeRoot.getPath

  val visualisations =
    new TreeNodeVisualisations(new File(SimXProperties.editorSettingsRoot, "config.xml"), editorComponentActor.self)

  private val list = new ListView[TreeNode]() {

    private val entityIcon = new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/entity.jpg")
    private val aspectIcon = new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/aspect.jpg")
    private val sVarIcon = new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/svar.jpg")
    private val rootIcon = new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/root.jpg")
    private val sVarRootIcon =
      new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/svarroot.jpg")
    private val createParamIcon =
      new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/createparam.jpg")

    renderer = new Renderer[TreeNode] {
      def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: TreeNode, index: Int) = {
        val comp =
          a match {
            case node: EnRoot => new Label(node.appName) {icon = rootIcon; xAlignment = Alignment.Left}
            case node: EnEntity => new Label(node.name) {icon = entityIcon; xAlignment = Alignment.Left}
            case node: EnSVarCollection => new Label("SVars") {icon = sVarRootIcon; xAlignment = Alignment.Left}
            case node: EnSVar => new Label(node.name.name) {icon = sVarIcon; xAlignment = Alignment.Left}
            case node: EnSVarValue => new Label(node.value.toString) {xAlignment = Alignment.Left}
            case node: EnCreateParamSet =>
              new Label(node.component.name) {icon = aspectIcon; xAlignment = Alignment.Left}
            case node: EnCreateParam =>
              new Label(node.cp.typedSemantics.sVarIdentifier.name) {
                icon = createParamIcon; xAlignment = Alignment.Left}
            case node: EnCreateParamValue => new Label(node.toString) {xAlignment = Alignment.Left}
            case _ => new Label("Unknown") {xAlignment = Alignment.Left}
          }
        if(selectedPath.contains(a)) {
          comp.opaque = true
          comp.background = Color.lightGray
        }
        comp.font = comp.font.deriveFont(18.0f)
        comp
      }
    }
  }

  private val eventList = new ListView[Event]() {

    private val entityIcon = new ImageIcon(SimXProperties.editorSettingsRoot.getAbsolutePath + "/icons/event.jpg")

    renderer = new Renderer[Event] {
      def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: Event, index: Int) = {
        val comp =
          a match {
            case e: Event =>
              new Label(e.name.value.toSymbol.name + " (" + e.values.firstValueFor(types.Time) + ")") {
                icon = entityIcon; xAlignment = Alignment.Left
              }
            case _ => new Label("Unknown") {xAlignment = Alignment.Left}
          }
        if(selectedPath.contains(a)) {
          comp.opaque = true
          comp.background = Color.lightGray
        }
        comp.font = comp.font.deriveFont(18.0f)
        comp
      }
    }
  }

  private def show(tn: TreeNode = selectedPath.reverse.head) {
    list.listData = selectedPath.toSeq ++ tn.children
  }

  val details = new DetailsViewScrollPane(
    new DetailsView {
      val component = new Label("Click on a list item to see its details here.")
      def update() = {}
      val node = treeRoot
    }
  )

  private val eventListSB = new ScrollPane(eventList)

  title = "SimX Editor"
  contents = new GridPanel(1, 3) {
    contents += eventListSB
    contents += details
    contents += new ScrollPane(list)
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  listenTo(eventList.mouse.clicks)
  listenTo(list.mouse.clicks)
  listenTo(editorComponentActor)

  addSynchronizedReaction {
    case msg: AppNameChanged =>
      treeRoot.appName = msg.name
      list.repaint()
  }

  private var eventsDisabled = false

  addSynchronizedReaction {
    case msg: EventArrived =>
      if(!eventsDisabled) {
        eventList.listData +:= msg.e
        if(eventList.listData.size > 100)
          eventList.listData = eventList.listData.dropRight(50)
      }
  }

  addSynchronizedReaction {
    case msg: SetEventStatus =>
      eventsDisabled = msg.disabled
      if(eventsDisabled) eventListSB.contents = new Label("Events disabled")
      else eventListSB.contents = eventList
  }

  addSynchronizedReaction {
    case msg: NewSVarValueArrived =>
      sVarToNode.get((msg.e, msg.sVarName)).collect({ case node =>
        val repaint: Boolean = node.parent.exists(n => selectedPath.contains(n))
        node.value = msg.value
        if(repaint) list.repaint()
        if(node == details.detailsView.node) details.detailsView.update()
        displayedNodes.get(node).collect{case i => i._2.update()}
      })
  }

  addSynchronizedReaction {
    case msg: NewEntityNameArrived =>
      treeRoot.children.find {
        case node: EnEntity => node.e == msg.e
        case _ => false
      }.collect{
        case node: EnEntity =>
          node.name = msg.name
          list.repaint()
      }
  }

  addSynchronizedReaction {
    case msg: EntityConfigurationArrived =>
      //Add the new configuration to the tree
      val eNode = new EnEntity(msg.e, Option(treeRoot))
      msg.csets.foreach( (symbolCSetTuple) => {
        val cSetNode = new EnCreateParamSet(symbolCSetTuple._1, symbolCSetTuple._2, Option(eNode))
        symbolCSetTuple._2.toSValSeq.foreach( (cp) => {
          val cParamNode = new EnCreateParam(cp, Option(cSetNode))
          new EnCreateParamValue(cp.value, Option(cParamNode))
        })
      })
      val svarCollNode = new EnSVarCollection(Option(eNode))
      msg.e.sVars.flatMap(t => t._2.map(t._1 -> _.svar)).foreach{
        case x => sVarToNode += (msg.e, x._1) -> new EnSVarValue( Some( EnSVar( x._2, x._1, Some( svarCollNode ) ) ) )
      }

      //Update the view
      show()
  }

  addSynchronizedReaction {
    case msg: RemoveEntity =>
      treeRoot.children.find {
        case node: EnEntity => node.e == msg.e
        case _ => false
      }.collect {
        case node: EnEntity =>
          treeRoot.children = treeRoot.children.filterNot(_ == node)
          if(selectedPath.contains(node)) selectedPath = treeRoot.getPath
          show()
          val dn = displayedNodes.values
          dn.foreach(i => if(i._2.node.getPath.contains(node)) {i._1.closeOperation(); i._1.dispose()})
      }
  }

  size = new Dimension(1067, 600)
  minimumSize = new Dimension(640, 480)
  visible = true

  reactions += {
    case e: MousePressed if e.source == list =>
      val selection =
        if(selectedPath.contains(list.selection.items.head)) list.selection.items.head.parent.getOrElse(treeRoot)
        else list.selection.items.head
      if(!selection.isInstanceOf[EnSVar]){
        selectedPath = selection.getPath
        show(selection)
        details.detailsView = visualisations.detailsViewFor(selection)
        details.detailsView.update()
      } else {
        val dv = visualisations.detailsViewFor(selection)
        val f = new Frame() {
          override def closeOperation() {
            super.closeOperation()
            displayedNodes -= dv.node
          }
          title = selection.getPath.filterNot(n => n.isInstanceOf[EnSVarCollection] || n.isInstanceOf[EnRoot]).
            map(_.toString).mkString(" > ")
          contents = new DetailsViewScrollPane(dv)
          size = new Dimension(thisEditorPanel.size.width/2, thisEditorPanel.size.height)
          minimumSize = new Dimension(thisEditorPanel.minimumSize.width/2, thisEditorPanel.minimumSize.height)
          visible = true
        }
        displayedNodes = displayedNodes.updated(dv.node, (f, dv))
        dv.update()
      }
    case e: MousePressed if e.source == eventList =>
      val item = eventList.selection.items.head
      details.detailsView =
        new DetailsView {
          val component = new Label(item.toString)
          def update() = {}
          val node = null
        }
  }

  //TODO Shutdown properly (no Sys.exit(0))
  override def closeOperation() = {
    visualisations.saveConfiguration()
    super.closeOperation()
  }
}