//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simplex3d.math.floatx.ConstVec3f

class ConstVec3fSVarViewGenerator147c9eda882846599cb0f7eda406394f extends SVarViewGenerator[ConstVec3f] {

  def generate: SVarView[ConstVec3f] = new SVarView[ConstVec3f] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */
    //Todo: Implement yourself!
    val component = new Label

    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */
    //Todo: Implement yourself!
    def update(sVarValue: ConstVec3f) {
      component.text = simplex3d.math.float.functions.length(sVarValue).formatted("%4.3f%n")
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Length View [String]"

}