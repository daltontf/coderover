package tfd.coderover

import org.hamcrest.Matcher

/**
 * Created by IntelliJ IDEA.
 * User: daltontk
 * Date: Jul 8, 2010
 * Time: 6:28:56 PM
 * To change this template use File | Settings | File Templates.
 */

object HamcrestAdapter {
  import org.hamcrest.Matchers

  def is[T](value:T):Matcher[Any] = Matchers.is(value.asInstanceOf[Any])
}