package tfd.coderover

object ResultOrAbend {
  def apply[A](rawValue:A) = new ResultOrAbend(rawValue)
  def apply(rawAbend:Abend) = new ResultOrAbend[Nothing](rawAbend)
}

case class ResultOrAbend[A](val value:Option[A], val abend:Option[Abend]) {
  def this(rawValue:A) = this(Some(rawValue), None)
  def this(rawAbend:Abend) = this(None, Some(rawAbend))

  def success = !value.isEmpty

  def flatMap[B](f: A => ResultOrAbend[B]): ResultOrAbend[B] =
    if (success) f(value.get) else this.asInstanceOf[ResultOrAbend[B]]

  def map[B](f: A => B): ResultOrAbend[B] =
     if (success) new ResultOrAbend(f(value.get)) else this.asInstanceOf[ResultOrAbend[B]]
}

object SuccessResultUnit extends ResultOrAbend[Unit](Some(()), None)