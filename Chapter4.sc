object Option {
  sealed trait Option[+A] {
    /**
     * Exercise 4.1
     */
    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)
    def filter(f: A => Boolean): Option[A] =
      if (map(f).getOrElse(false)) this
      else None
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
   * Exercise 4.2
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)
}