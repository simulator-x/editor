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
 * Date: 2/25/11
 * Time: 10:01 AM
 */
package simx.components.editor.gui

import swing.Component

/**
 *  Holds information about SVarViewException exceptions
 */
case class SVarViewException(errorMessage: String) extends java.lang.Throwable {
  override def toString: String = errorMessage
}

abstract class SVarViewBase {
  val component: Component
  def internalUpdate(sVarValue: Any)
}

abstract class SVarView[T] extends SVarViewBase {
  type sVarValueType  = T

  final def internalUpdate(sVarValue: Any) {
    try {
      update(sVarValue.asInstanceOf[sVarValueType])
    }
    catch {
      case _ : Throwable => throw SVarViewException("SVarView can not handle a value of type " +
        sVarValue.asInstanceOf[AnyRef].getClass.getCanonicalName)
    }
  }

  def update(sVarValue: sVarValueType)
}

abstract class SVarViewGeneratorBase {
  val name: String
  def generate: SVarViewBase
  override def toString: String = name
}

abstract class SVarViewGenerator[T] extends SVarViewGeneratorBase{
  def generate: SVarView[T]
}