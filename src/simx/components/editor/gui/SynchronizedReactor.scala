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

import swing.Reactor
import javax.swing.SwingUtilities

/*
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 8/24/11
 * Time: 4:50 PM
 */

/**
 * Provides a method to add reactions that are executed in the java swing thread
 */
trait SynchronizedReactor extends Reactor {

  /**
   * Adds a reaction that is executed in the java swing thread by using
   * SwingUtilities.invokeLater
   */
  def addSynchronizedReaction(r: scala.swing.Reactions.Reaction) {
    reactions += {
      case e: scala.swing.event.Event if r.isDefinedAt(e) =>
        SwingUtilities.invokeLater(new Runnable {
          def run() {
            r.apply(e)
          }
        })
    }
  }
}