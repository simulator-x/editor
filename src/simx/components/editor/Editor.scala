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
import simx.core.svaractor.unifiedaccess.{StateParticleInfo, Relation}
import scala.collection.mutable
import swing.Publisher
import simx.core.component.Component
import gui._
import simx.core.entity.description._
import simx.core.ontology.types.Name
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.Entity
import simx.core.entity.component.ComponentAspect
import simx.core.worldinterface.eventhandling.{Event, EventHandler, EventDescription}
import simx.core.svaractor.{StateParticle, SVar}

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
  private val entityKnownSVarsMap : mutable.Map[Entity, Map[Symbol, StateParticleInfo[_]]] =
    mutable.Map[Entity, Map[Symbol, StateParticleInfo[_]]] ()

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) = {
    configAspect = Some(aspect)
    aspect.getCreateParams.combineWithValues(toProvide)._1
  }

  private var configEntity: Option[Entity] = None

  protected def finalizeConfiguration(e: Entity){
    configEntity = Some(e)
    configAspect.collect{case ca => publish(EntityConfigurationArrived(e, Map(toTuple(ca))))}

    e.getAllStateParticles.foreach{ triple =>
      if (triple.identifier == Name.sVarIdentifier){
        triple.svar.observe{ a => publish(NewEntityNameArrived(e, a.toString)) }
        triple.svar.get{ a => publish(NewEntityNameArrived(e, a.toString)) }
      }

      if (triple.identifier == gt.Enabled.withAnnotations(Symbols.event).sVarIdentifier){
        triple.svar.asInstanceOf[SVar[Boolean]].observe{
          a => {publish(SetEventStatus(!a)); publish(NewSVarValueArrived(e, triple.identifier, triple , a))} }
        triple.svar.asInstanceOf[SVar[Boolean]].get{
          a => {publish(SetEventStatus(!a)); publish(NewSVarValueArrived(e, triple.identifier, triple,  a))} }
      }
      else if (triple.identifier == gt.Application.sVarIdentifier){
        triple.svar.asInstanceOf[SVar[String]].observe{
          a => {publish(AppNameChanged(a)); publish(NewSVarValueArrived(e, triple.identifier, triple,  a))}  }
        triple.svar.asInstanceOf[SVar[String]].get{
           a => {publish(AppNameChanged(a)); publish(NewSVarValueArrived(e, triple.identifier, triple, a))} }
      } else {
        triple.svar.observe{ a => publish(NewSVarValueArrived(e, triple.identifier, triple,  a)) }
        triple.svar.get{ a => publish(NewSVarValueArrived(e, triple.identifier, triple, a)) }
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
    e.getAllStateParticles.foreach { _.svar.ignore() /*Stop observing*/ }

    // cleanup local refs
//    println("remove entity " + e.toString)
    registeredEntities = registeredEntities - e
    entityKnownSVarsMap - e

    //update view
    publish(RemoveEntity(e))
  }

  private var gui: Option[EditorPanel] = None


  //registerForCreationOf(Nil)
  //addHandler[CreationMessage]{ msg => handleCreation(msg.e, msg.e.description.aspects.map( toTuple ).toMap) }
  handleEntityRegistration(Nil){ e => handleCreation(e, e.description.aspects.map( toTuple ).toMap) }
  //requestRegisteredEntities(Nil){ _.map( e => handleCreation(e, e.description.aspects.map( toTuple ).toMap)) }

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
    e.addRemoveObserver(self)()
    e.get(gt.EventDescription).foreach( handleEventDesc _ )
    if(e.getSVars(gt.EventDescription).nonEmpty) return
    if(configEntity.contains(e)) return

    if (registeredEntities.contains(e))
      return
    registeredEntities = registeredEntities + e
    //Add the new configuration to the gui
    publish(EntityConfigurationArrived(e, csets))

    //register all new SVars
    e.getAllStateParticles.foreach{ triple =>
      entityKnownSVarsMap.update(e ,
        entityKnownSVarsMap.getOrElse(e, Map[Symbol, StateParticleInfo[_]]()) + (triple.identifier -> triple)  )
      if (triple.identifier == Name.sVarIdentifier){
        triple.svar.observe{ a => publish(NewEntityNameArrived(e, a.toString)) }
        triple.svar.get{ a => publish(NewEntityNameArrived(e, a.toString)) }
      }
      //relation SVar
      if(triple.svar.containedValueManifest <:< gt.Relation.classTag) {
        triple.svar.observe{ a =>
          val rel = a.asInstanceOf[Relation]
          publish(NewSVarValueArrived(e, triple.identifier, triple,  rel.getObject)) }
        triple.svar.get{ a =>
          val rel = a.asInstanceOf[Relation]
          publish(NewSVarValueArrived(e, triple.identifier, triple, rel.getObject)) }
      }
      //all other SVar
      else
      {
        triple.svar.observe{ a => publish(NewSVarValueArrived(e, triple.identifier, triple,  a)) }
        triple.svar.get{ a => publish(NewSVarValueArrived(e, triple.identifier, triple, a)) }
      }
    }

    //observe Entity for changes
    e.onUpdate{ newEntity : Entity =>
      val knownSVars = entityKnownSVarsMap.getOrElse(e,Map[Symbol, StateParticle[_]]())
      val currentSVars : Map[Symbol, StateParticleInfo[_]]= {
        (for ( v <- newEntity.getAllStateParticles ) yield v.identifier -> v).toMap
      }

      //sVars to ADD
      val addMap = currentSVars.filterNot( elem => knownSVars.contains(elem._1))
      //add to knownSVarsMap
      entityKnownSVarsMap.update(e, entityKnownSVarsMap.getOrElse(e,Map[Symbol, StateParticleInfo[_]]() ) ++ addMap)
      //add observer and update View
      addMap.foreach{ tupel =>
        if (tupel._1 == Name.sVarIdentifier){
          tupel._2.svar.observe{ a => publish(NewEntityNameArrived(e, a.toString)) }
          tupel._2.svar.get{ a => publish(NewEntityNameArrived(e, a.toString)) }
        }
        //relation SVar
        if(tupel._2.svar.containedValueManifest <:< gt.Relation.classTag) {
          tupel._2.svar.observe{ a =>
            val rel = a.asInstanceOf[Relation]
            publish(UpdateSVarOfEntity(e, tupel._1, tupel._2, rel.getObject)) }
          tupel._2.svar.get{ a =>
            val rel = a.asInstanceOf[Relation]
            publish(UpdateSVarOfEntity(e, tupel._1, tupel._2, rel.getObject)) }
        }
        //all other SVar
        else {
          tupel._2.svar.observe{ a => publish(UpdateSVarOfEntity(e, tupel._1 , tupel._2, a)) }
          tupel._2.svar.get{ a => publish(UpdateSVarOfEntity(e, tupel._1 , tupel._2, a)) }
        }
      }

      //sVars to REMOVE
      val removeMap = knownSVars.filterNot( elem => currentSVars.contains(elem._1))
      //remove from knownSVarsMap
      entityKnownSVarsMap.update(e, entityKnownSVarsMap.getOrElse(
        e,Map[Symbol, StateParticleInfo[_]]() ).filterNot( elem => removeMap.contains(elem._1)))
      //update View
      removeMap.foreach{ tupel =>
        publish(RemoveSVarFromEntity(e, tupel._1))
      }
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