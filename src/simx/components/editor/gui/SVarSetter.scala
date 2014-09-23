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
 * Date: 3/12/11
 * Time: 3:31 PM
 */
package simx.components.editor.gui

import simx.core.entity.typeconversion.TypeInfo._

import swing.Component
import simx.core.svaractor.{StateParticle, SVarActor, SVar}

/**
 *  Holds information about SVarViewException exceptions
 */
case class SVarSetterException(errorMessage: String) extends java.lang.Throwable {
  override def toString: String = errorMessage
}

abstract class SVarSetterBase {
  val component: Component
  //Changes the setter to write to this svar.
  //If the underlying type of the svar is compatible to sVar is not checked.
  //If the types are not compatible, an exception is thrown.
  def changeRegisteredSvarTo(sVar: StateParticle[_]): Unit
  def internalUpdate(sVarValue: Any)
}

abstract class SVarSetter[T : DataTag] extends SVarSetterBase {
  case class Value[U](v : U)
  case class HandleValue[U](handler : U => Unit)
  private var setter: Option[StateParticle[T]] = None
  private val actor = SVarActor.createActor(new SVarActor(){
    addHandler[Value[T]]{
      msg => setter.collect{case svar : SVar[T] => svar.set(msg.v)}
    }
    addHandler[HandleValue[T]]{
      msg => setter.collect{case svar : SVar[T] => svar.get(msg.handler)}
    }
  })

  final def changeRegisteredSvarTo(sVar: StateParticle[_]){
    if(sVar != null)
      try {
        setter = Some(sVar.asInstanceOf[StateParticle[T]])
      } catch {
        case _ : Throwable => throw SVarSetterException("SVarSetter can not handle a sVar of type " +
          sVar.containedValueManifest)
      }
  }

  final def internalUpdate(sVarValue: Any) {
    try {
      update(sVarValue.asInstanceOf[T])
    }
    catch {
      case _ : Throwable => throw SVarViewException("SVarView can not handle a value of type " +
        sVarValue.asInstanceOf[AnyRef].getClass.getCanonicalName)
    }
  }

  def update(sVarValue: T) {}

  final def setSvar(newValue: T){
    actor ! Value(newValue)
  }

  final def handleSvarValue(handler: (T) => Unit) {
    actor ! HandleValue(handler)
  }
}

abstract class SVarSetterGeneratorBase {
  val name: String
  def generate: SVarSetterBase
  override def toString : String = name
}

abstract class SVarSetterGenerator[T] extends SVarSetterGeneratorBase{
  def generate: SVarSetter[T]
}