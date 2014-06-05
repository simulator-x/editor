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

import simx.core.entity.description.NamedSValSet
import simx.core.entity.Entity
import simx.core.svaractor.SVarActor
import simx.core.worldinterface.eventhandling.Event

/**
 * User: Martin Fischbach
 * Date: 2/11 amd 8/13
 */

/*SCALA SWING EVENTS*/
private[editor] case class EntityConfigurationArrived(e : Entity, csets : Map[Symbol, NamedSValSet]) extends scala.swing.event.Event
private[editor] case class NewSVarValueArrived(e : Entity, sVarName : Symbol, value: Any) extends scala.swing.event.Event
private[editor] case class NewEntityNameArrived(e : Entity, name: String) extends scala.swing.event.Event
private[editor] case class AppNameChanged(name: String) extends scala.swing.event.Event
private[editor] case class AvailableViewsChanged() extends scala.swing.event.Event
private[editor] case class UpdateSVarDetailsView() extends scala.swing.event.Event
private[editor] case class RemoveEntity(e : Entity) extends scala.swing.event.Event
private[editor] case class EventArrived(e : Event) extends scala.swing.event.Event
private[editor] case class SetEventStatus(disabled: Boolean) extends scala.swing.event.Event
/*SCALA SWING EVENTS END*/