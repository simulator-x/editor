//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simplex3d.math.floatx.ConstVec3f

class ConstVec3fSVarViewGenerator49fa0fd6ad4b49c58c2b442a990e3aef extends SVarViewGenerator[ConstVec3f] {

  def generate: SVarView[ConstVec3f] = new SVarView[ConstVec3f] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the value.
     */
    //Todo: Implement yourself!
    val component = new ProgressBar() {
      min = 0
      max = 1000
    }

    /**
      *  This function is called whenever the visualized value changes.
      *  It should update component accordingly.
      */
    //Todo: Implement yourself!
    def update(sVarValue: ConstVec3f) {
      component.value = (simplex3d.math.float.functions.length(sVarValue) * 1000f).toInt
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Length View [ProgressBar][0-1]"

}