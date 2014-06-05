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

//Global Types
import simx.core.ontology.{Symbols, types => gt}
import simx.core.entity.description.SValSet
import simx.core.component.ConfigureComponentMessage
import simx.core.svaractor.SVarActor
import scala.annotation.meta.param

/*
* User: martin
* Date: 6/10/11
* Time: 10:23 AM
*/

/**
*  Used to configure the Editor
*
* @param name    The editor's name.
* @param appName The application's name.
*/
case class EditorConfiguration(appName: String, name: String = "Editor")
                              (implicit @(transient @param) actorContext : SVarActor.Ref)
  extends ConfigureComponentMessage(SValSet(gt.Name(name), gt.Name.addAnnotations(Symbols.application).apply(appName)))
{
  def targetComponentType = Symbols.editor
}