//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

class BufferedImageSVarViewGeneratorf8550edb70414ae285ce22feb791b03a extends SVarViewGenerator[BufferedImage] {

  def generate: SVarView[BufferedImage] = new SVarView[BufferedImage] {

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
    def update(sVarValue: BufferedImage) {
      component.icon = new ImageIcon(sVarValue)
    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "BufferedImage View"

}