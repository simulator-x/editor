//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simx.core.helper.TextureData
import javax.swing.ImageIcon


class TextureDataSVarViewGenerator4857baf5b0ea4a549c63c0bff6c31e72 extends SVarViewGenerator[TextureData] {

  def generate: SVarView[TextureData] = new SVarView[TextureData] {

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
    def update(sVarValue: TextureData) {
      sVarValue.toImage match {
        case Some(img) =>
          component.text = ""
          component.icon = new ImageIcon(img)
        case None =>
          component.icon = null
          component.text = "No texture"
      }

    }

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "TextureData View"

}