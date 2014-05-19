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

import swing.ComboBox

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 05.12.12
 * Time: 14:21
 */
trait ComboBoxSwingReflection[A] extends ComboBox[A] {

  protected def _peer = invoke(this, "javax.swing.JComboBox", "peer")
  protected def _getModel = invoke(_peer.get, "javax.swing.ComboBoxModel", "getModel")
  protected def _getSize = invoke(_getModel.get, "int", "getSize").get.asInstanceOf[Int]
  protected def _getElementAt(i: Int) =
    invoke(_getModel.get, "java.lang.Object", "getElementAt", i.asInstanceOf[AnyRef]).get.asInstanceOf[A]
  protected def _setModel(m: AnyRef) = invoke(_peer.get, "void", "setModel", m)

  private val _defModCon =
    Class.forName("javax.swing.DefaultComboBoxModel").getConstructor(classOf[java.util.Vector[A]])

  protected def _createDefaultModel(newItems: Seq[A]) = {
    val tmpVec = new java.util.Vector[A]()
    newItems.foreach(tmpVec.add)
    _defModCon.newInstance(tmpVec).asInstanceOf[AnyRef]
  }

  protected def invoke(obj: AnyRef, returnTypeName: String,  methodName: String, args: AnyRef*): Option[AnyRef] = {
    val  ms = obj.getClass.getMethods.filter(m => m.getName == methodName && m.getReturnType.getName == returnTypeName)
    if(ms.size == 0)
      throw new Exception(
        "ComboBoxSwingReflection: No matching method found. Candidates are: \n" +
        obj.getClass.getMethods.map(m => m.getReturnType.getName + " " +  m.getName + "(???)").mkString("\n"))
    else if(ms.size > 1)
      throw new Exception(
        "ComboBoxSwingReflection: To many matching methods found. Candidates are: \n" +
        obj.getClass.getMethods.filter(m => m.getName == methodName && m.getReturnType.getName == returnTypeName).
          map(m => m.getReturnType.getName + " " +  m.getName + "(???)").mkString("\n"))

    ms.headOption.map(_.invoke(obj, args: _*))
  }
}
