package tfd.coderover

import junit.framework._
import org.junit.Assert._

class StateTest extends TestCase {

  def testSetEqual() {
    val state1 = new State(2, 3, 3)
    val state2 = new State(6, 7, 1)
    state1.setEqual(state2)
    assertEquals(state1, state2)
  }
}