//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simplex3d.math.floatx.ConstVec3f

class ConstVec3fSVarViewGenerator66ede5a13eab476792d916f301ca73ac extends SVarViewGenerator[ConstVec3f] {

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
      component.text = (for(i <- 0 until 3) yield sVarValue(i)).map(_.formatted("%4.3f%n")).mkString("("," ,",")")
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Vector View"

}