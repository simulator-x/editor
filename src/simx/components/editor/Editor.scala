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

package simx.components.editor

import simx.core.helper.SVarUpdateFunctionMap
import simx.core.svaractor.SVarActor
import swing.Publisher
import simx.core.component.Component
import gui._
import simx.core.entity.description._
import simx.core.ontology.types.Name
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity

//Global Types
import simx.core.ontology.{Symbols, types => gt}

/**
 * User: martin
 * Date: Oct 29, 2010
 */

class Editor(override val componentName : Symbol)
        extends SVarActor with Component with SVarUpdateFunctionMap with Publisher {

  def componentType = Symbols.editor

  private val conf = InternalEditorConfiguration()

  protected def configure(params: SValSet) {
    params.getFirstValueFor(gt.Name).collect {
      case name =>
        conf.name = name
    }
    params.getFirstValueFor(gt.Name.addAnnotations(Symbols.application)).collect {
      case appName =>
        conf.appName = appName
        publish(AppNameChanged(appName))
    }
  }

  def removeFromLocalRep(e: Entity) {
    e.getAllSVars.foreach {
      //Stop observing
      x => ignore(x._3)
    }
    publish(RemoveEntity(e))
  }

  private var gui: Option[EditorPanel] = None

  GeneralEntityDescription.registerCreationObserver(this)

  override def startUp() {
    gui = Some(new EditorPanel(this))
  }

  override def shutdown(){
    super.shutdown()
    gui.collect({case v => v.visible = false})
    //If this is commented in, exit(0) will be invoked
    //gui.collect({case v => v.closeOperation})
  }

  //React to entity-creations
  addHandler[EntityConfiguration]{ msg: EntityConfiguration =>
    //Add the new configuration to the gui
    publish(EntityConfigurationArrived(msg.e, msg.csets))

    msg.e.getAllSVars.foreach{ triple =>
      observe(triple._3){ a => publish(NewSVarValueArrived(msg.e, triple._1, a)) }
      get(triple._3){ a => publish(NewSVarValueArrived(msg.e, triple._1, a)) }
      if (triple._1 == Name.sVarIdentifier){
        observe(triple._3){ a => publish(NewEntityNameArrived(msg.e, a.toString)) }
        get(triple._3){ a => publish(NewEntityNameArrived(msg.e, a.toString)) }
      }
    }
  }

  //Forward events to the editor panel
  addHandler[scala.swing.event.Event]{ msg: scala.swing.event.Event =>
    publish(msg)
  }

  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {}

  protected def requestInitialValues(toProvide: scala.collection.immutable.Set[ConvertibleTrait[_]],
                                     aspect: EntityAspect, e: Entity, given: SValSet) {
    provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1) }

  protected def performSimulationStep() {
    this.simulationCompleted()
  }

  private case class InternalEditorConfiguration() {
    var name = "Unnamed Editor (" + this.hashCode().toString + ")"
    var appName = "Amazing SimX App"
  }
}