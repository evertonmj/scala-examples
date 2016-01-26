class Rational(n: Int, d: Int) {
  //restriction for variable
  require(d != 0)
  
  private val g = gcd(n.abs, d.abs)

  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1)

  //override to string
  override def toString = numer +"/"+denom

  //rational add method
  def + (that: Rational): Rational = 
      new Rational(
          numer * that.denom + that.numer * denom,
          denom * that.denom
      )
  def + (i: Int): Rational = 
    new Rational(numer + i * denom, denom)

  //rational subtraction method
  def - (that: Rational): Rational = 
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )
  def - (i: Int): Rational = 
    new Rational(numer - i * denom, denom)

  //rational multiplication method
  def * (that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)
  def * (i: Int): Rational = 
    new Rational(numer * i, denom)

  //rational division method
  def / (that: Rational): Rational = 
    new Rational(numer * that.denom, denom * that.numer)
  def / (i: Int): Rational = 
    new Rational(numer, denom * i)

  def lessThan(that: Rational) = 
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = 
    if(this.lessThan(that)) that else this
  
  //greatest common divisor
  private def gcd(a: Int, b: Int): Int = 
    if (b == 0) a else gcd(b, a % b)
}
