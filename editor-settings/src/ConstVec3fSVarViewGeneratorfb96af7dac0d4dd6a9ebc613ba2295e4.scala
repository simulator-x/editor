//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simplex3d.math.floatx.ConstVec3f

class ConstVec3fSVarViewGeneratorfb96af7dac0d4dd6a9ebc613ba2295e4 extends SVarViewGenerator[ConstVec3f] {

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
      component.value = (simplex3d.math.float.functions.length(sVarValue) * 100f).toInt
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Length View [ProgressBar][0-10]"

}