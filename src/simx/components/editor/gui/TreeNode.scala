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
 * User: Martin Fischbach
 * Date: 2/11 and 8/13
 */
package simx.components.editor.gui

import simx.core.entity.Entity

import java.util.UUID
import simx.core.svaractor.StateParticle
import simx.core.entity.description.{NamedSValSet, SVal}

abstract class TreeNode() {
  def children: Seq[TreeNode]
  def children_=(value: Seq[TreeNode])
  def parent: Option[TreeNode]

  parent.collect({case p => p.children = p.children :+ this })

  def getPath: List[TreeNode] = {
    parent.map(_.getPath ::: (this :: Nil)).getOrElse(this :: Nil)
  }

  def getPathToParent: List[TreeNode] = {
    getPath.dropRight(1)
  }

  private val id = UUID.randomUUID

  /**
   *  Implemented to prevent an infinite loop when invoking java.lang.hashcode.
   *  Somehow the scala println manages to invoke java.lang.Object.hashcode.
   *  Even though hashcode is overwritten here. //TODO investigate
   *  @see hashcode
   */
  override def toString : String = {
    getClass.getName + "@" + Integer.toHexString(hashCode)
  }

  /**
   *  Implemented to prevent an infinite loop when invoking java.lang.hashcode.
   *  The normal hashcode method would use the hashcodes of all members to calculate
   *  the hashcode of this object. Since it is a double linked tree this would
   *  cause an infinite loop.
   *
   *  Redirects the hashCode method call to the UUIDs hashCode method.
   */
  override def hashCode = {
    id.hashCode
  }

  /**
   *  Redirects the equals method call to the UUIDs equals method
   */
  override def equals(obj: Any) = {
    obj match {
      case that: TreeNode => that.id == id
      case _ => false
    }
  }

}

abstract class TreeNodeWithValue extends TreeNode {
  def value: Any
}

case class EnRoot private(var appName: String, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNode {
    def this(appName: String, children: TreeNode*) = this(appName, None, children.toSeq)
    override def toString = appName
}
case class EnEntity(e: Entity, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  var name: String = e.id.toString
  override def toString = "Entity[" + name + "]"
}

case class EnSVarCollection(parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  override def toString = "SVars"
}

case class EnSVar(sVar: StateParticle[_], name: Symbol, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNode {
  def enSVarValue = children.head.asInstanceOf[EnSVarValue]

  override def toString = "SVar[" + name.name + "]"
}

case class EnSVarValue(var value: Any, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNodeWithValue {
    def this(parent: Option[TreeNode], children: TreeNode*) = this("Not initialized", parent, children.toSeq)
}

case class EnCreateParamSet(
  component: Symbol, cps: NamedSValSet, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode

case class EnCreateParam(cp: SVal[_], parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  def enCreateParamValue = children.head.asInstanceOf[EnCreateParamValue]
}

case class EnCreateParamValue(value: Any, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNodeWithValue {
    override def toString : String = value.toString
}
