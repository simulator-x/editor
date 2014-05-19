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
import swing.Publisher
import simx.core.component.Component
import gui._
import simx.core.entity.description._
import simx.core.ontology.types.Name
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity
import simx.core.entity.component.ComponentAspect
import simx.core.worldinterface.CreationMessage
import simx.core.worldinterface.eventhandling.{Event, EventHandler, EventDescription}
import simx.core.svaractor.SVar

//Global Types
import simx.core.ontology.{types => gt, Symbols}

/**
 * User: martin
 * Date: Oct 29, 2010
 */


case class EditorComponentAspect(name : Symbol, appName: String = "SimXApp", eventsDisabled: Boolean = false)
  extends ComponentAspect[Editor](Symbols.editor, name)
{
  def getCreateParams: NamedSValSet =
    NamedSValSet(aspectType,
      gt.Enabled.withAnnotations(Symbols.event)(!eventsDisabled),
      gt.Application(appName)
    )

  def getComponentFeatures: Set[ConvertibleTrait[_]] =
    Set(gt.Enabled.withAnnotations(Symbols.event), gt.Application)
}

class Editor(override val componentName : Symbol)
        extends Component(componentName, Symbols.editor) with SVarUpdateFunctionMap with Publisher with EventHandler {

  private var configAspect: Option[EntityAspect] = None

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) = {
    configAspect = Some(aspect)
    aspect.getCreateParams.combineWithValues(toProvide)._1
  }

  private var configEntity: Option[Entity] = None

  protected def finalizeConfiguration(e: Entity){
    configEntity = Some(e)
    configAspect.collect{case ca => publish(EntityConfigurationArrived(e, Map(toTuple(ca))))}

    e.getAllStateParticles.foreach{ triple =>
      if (triple._1 == Name.sVarIdentifier){
        triple._3.observe{ a => publish(NewEntityNameArrived(e, a.toString)) }
        triple._3.get{ a => publish(NewEntityNameArrived(e, a.toString)) }
      }

      if (triple._1 == gt.Enabled.withAnnotations(Symbols.event).sVarIdentifier){
        triple._3.asInstanceOf[SVar[Boolean]].observe{
          a => {publish(SetEventStatus(!a)); publish(NewSVarValueArrived(e, triple._1, a))} }
        triple._3.asInstanceOf[SVar[Boolean]].get{
          a => {publish(SetEventStatus(!a)); publish(NewSVarValueArrived(e, triple._1, a))} }
      }
      else if (triple._1 == gt.Application.sVarIdentifier){
        triple._3.asInstanceOf[SVar[String]].observe{
          a => {publish(AppNameChanged(a)); publish(NewSVarValueArrived(e, triple._1, a))}  }
        triple._3.asInstanceOf[SVar[String]].get{
           a => {publish(AppNameChanged(a)); publish(NewSVarValueArrived(e, triple._1, a))} }
      } else {
        triple._3.observe{ a => publish(NewSVarValueArrived(e, triple._1, a)) }
        triple._3.get{ a => publish(NewSVarValueArrived(e, triple._1, a)) }
      }
    }
  }

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
    e.getAllStateParticles.foreach { _._3.ignore() /*Stop observing*/ }
    publish(RemoveEntity(e))
  }

  private var gui: Option[EditorPanel] = None


  registerForCreationOf(Nil)
  addHandler[CreationMessage]{ msg => handleCreation(msg.e, msg.e.description.aspects.map( toTuple ).toMap) }
  handleRegisteredEntities(Nil){ _.map( e => handleCreation(e, e.description.aspects.map( toTuple ).toMap)) }

  private def toTuple(a : EntityAspect) : (Symbol, NamedSValSet) =
    a.componentType.value.toSymbol -> a.getCreateParams

  override def startUp() {
    gui = Some(new EditorPanel(this))
  }

  override def shutdown(){
    super.shutdown()
    gui.collect({case v => v.visible = false})
    //If this is commented in, exit(0) will be invoked
    //gui.collect({case v => v.closeOperation})
  }

  private var registeredEntities = Set[Entity]()
  private var requestedEvents = Set[EventDescription]()

  def handleEventDesc(desc: EventDescription) {
    if(!requestedEvents.contains(desc)) {
      //println("[Editor][info] requesting event " + desc)
      requestEvent(desc)
      requestedEvents += desc
    }
  }

  override protected def handleEvent(e: Event) {
    super.handleEvent(e)
    publish(EventArrived(e))
  }

  //React to entity-creations
  def handleCreation(e: Entity, csets: Map[Symbol, NamedSValSet]){
    e.addRemoveObserver(self)
    e.get(gt.EventDescription).foreach( handleEventDesc(_) )
    if(!e.getSVars(gt.EventDescription).isEmpty) return
    if(configEntity.exists(_ == e)) return

    if (registeredEntities.contains(e))
      return
    registeredEntities = registeredEntities + e
    //Add the new configuration to the gui
    publish(EntityConfigurationArrived(e, csets))

    e.getAllStateParticles.foreach{ triple =>
      if (triple._1 == Name.sVarIdentifier){
        triple._3.observe{ a => publish(NewEntityNameArrived(e, a.toString)) }
        triple._3.get{ a => publish(NewEntityNameArrived(e, a.toString)) }
      }
      triple._3.observe{ a => publish(NewSVarValueArrived(e, triple._1, a)) }
      triple._3.get{ a => publish(NewSVarValueArrived(e, triple._1, a)) }
    }
  }

  //Forward events to the editor panel
  addHandler[scala.swing.event.Event]{
    msg => publish(msg)
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