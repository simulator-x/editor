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
import simx.core.ontology._
import simx.core.svaractor.StateParticle
import simx.core.entity.description.{NamedSValSet, SVal}
import simx.core.svaractor.unifiedaccess.StateParticleInfo

abstract class TreeNode() {
  def label: String
  def hierarchicalOrder: Int
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

  protected def getAnnotationsLabelAndTypeInfoLabel(annotations : Set[Annotation], sVarIdentifier : Symbol) : String = {
    val sb  = new java.lang.StringBuilder()
    sb.append(" : " + sVarIdentifier.name)
    if (annotations.nonEmpty) {
      sb.append(" <")
      sb.append(annotations.head.value)
      annotations.tail.foreach(a => sb.append(", " + a.value) )
      sb.append(">")
    }
    sb.toString
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
  var value: Any
}

abstract class EnSVarBaseNode extends TreeNode {
  def sParInf : StateParticleInfo[_]

  private val _sVar = sParInf.svar
  def sVar: StateParticle[_] = _sVar
 //def sVar_= (value:StateParticle[_]) : Unit = _sVar = value
  def name: Symbol
  def isSVal : Boolean
}

case class EnRoot private(var appName: String, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNode {
    def this(appName: String, children: TreeNode*) = this(appName, None, children.toSeq)
    override def toString = appName
    override def label = toString
    override def hierarchicalOrder = 0
}
case class EnEntity(e: Entity, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  var name: String = e.id.toString
  override def toString = "Entity[" + name + "]"
  override def label = {
    if(e.getSimpleName == "unnamed-entity") e.getSimpleName+" ["+name+"]"
    else e.getSimpleName + getAnnotationsLabelAndTypeInfoLabel(
      e.description.typeDef.annotations, e.description.typeDef.sVarIdentifier )
  }
  override def hierarchicalOrder = 1
}

case class EnSVarCollection(parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  override def toString = "SVars"
  override def label = toString
  override def hierarchicalOrder = 5
}

case class EnSVar(sParInf: StateParticleInfo[_], name: Symbol, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends EnSVarBaseNode {
  def enSVarValue = children.head.asInstanceOf[EnSVarValue]
  def isSVal = enSVarValue.isSVal
  def label : String =
    name.name + getAnnotationsLabelAndTypeInfoLabel(sParInf.annotations, sParInf.typeInfo.sVarIdentifier )
  def hierarchicalOrder = 8
  override def toString = "SVar[" + name.name + "]"
}

case class EnSVarRelation(sParInf: StateParticleInfo[_], name: Symbol,  parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends EnSVarBaseNode {

//  def enSVarRelValue = children.head.asInstanceOf[EnSVarRelationValue]
  def isSVal = children.head.asInstanceOf[EnSVarValue].isSVal
  override def toString = "SVarRel[" + name.name + "]"
  def label = name.name
  def hierarchicalOrder = 6
}

case class EnSVarValue(var value: Any, isSVal : Boolean, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNodeWithValue {
    def this(isSVal : Boolean, parent: Option[TreeNode], children: TreeNode*) = this("Not initialized", isSVal, parent, children.toSeq)
  override def label = value.toString
  override def hierarchicalOrder = 9
}

case class EnSVarRelationValue(var value: Any, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNodeWithValue {
  def this(parent: Option[TreeNode], children: TreeNode*) = this("Not initialized", parent, children.toSeq)
  override def toString : String = {
    value match {
      case e: Entity => e.getSimpleName
      case _ => "Not initialized"
    }
  }
  override def label = toString
  override def hierarchicalOrder = 7
}

case class EnCreateParamSet(
  component: Symbol, cps: NamedSValSet, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  override def label = component.name
  override def hierarchicalOrder = 2
}


case class EnCreateParam(cp: SVal.SValType[_], parent: Option[TreeNode], var children: Seq[TreeNode] = Nil) extends TreeNode {
  def enCreateParamValue = children.head.asInstanceOf[EnCreateParamValue]
  override def label = cp.typedSemantics.sVarIdentifier.name
  override def hierarchicalOrder = 3
}

case class EnCreateParamValue(newvalue: Any, parent: Option[TreeNode], var children: Seq[TreeNode] = Nil)
  extends TreeNodeWithValue {
    override def toString : String = newvalue.toString
    override var value: Any = newvalue
    override def label = toString
  override def hierarchicalOrder = 4
}

