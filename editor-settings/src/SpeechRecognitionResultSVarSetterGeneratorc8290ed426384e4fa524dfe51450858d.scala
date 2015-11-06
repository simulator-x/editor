//AutoGenerated Begin
//DO NOT EDIT!
package simx.components.editor.gui

import scala.swing._
import simx.core.helper.{Hypothesized, SpeechRecognitionResult}

import scala.swing.event.EditDone


class SpeechRecognitionResultSVarSetterGeneratorc8290ed426384e4fa524dfe51450858d extends SVarSetterGenerator[SpeechRecognitionResult] {

  def generate: SVarSetter[SpeechRecognitionResult] = new SVarSetter[SpeechRecognitionResult] {

//AutoGenerated END
//Put your code below

    /**
     *  The scala.swing.Component that visualizes the SVar setter.
     *  Call
     *  setSvar(newValue: SpeechRecognitionResult): Unit
     *  to set new svar value.
     */
    //Todo: Implement yourself!
    val component = new TextField() {
      listenTo(this)
      reactions += {
        case event: EditDone =>
          setSvar(Hypothesized(this.text, 1f, System.currentTimeMillis()))
      }
    }

    /**
     * Override update if you want to use the current value of the SVar.
     * This function is initially called once and then every time the value of the SVar changes.
     */
    // override def update(newValue: SpeechRecognitionResult) {}

  }

  /**
   *  The name of this visualizer.
   *  This must not be unique.
   */
  //Todo: Name it!
  val name: String = "Hypothesized Setter"

}