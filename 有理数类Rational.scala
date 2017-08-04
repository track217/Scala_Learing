//设计有理数类 Rational
class Rational (n:Int, d:Int) {
	require(d!=0)
	private val g = gcg(n.abs, d.abs)
	val number = n/g  //成员变量使用val，实现“immutable” 类型的类定义
	val denom = d/g
	override def toString = number + "/" + denom //使用override重载基类定义的方法
	
	def +(that: Rational) = 
		new Rational(
			number * that.denom + that.number * denom,
			denom * that.denom
			)	
			
	def *（that: Rational) = 
		new Rational(number * that.number, denom * that.denom)
		
	//自身引用
	def lessThan(that: Rational) =  //访问类成员无需this
		number * that.denom < that.number * denom
	def max(that: Rational) =   //需要引用对象自身，无法省略
		if(lessThan(that)) that else this
	
	//辅助构造函数	
	def this(n: Int) = this(n, 1) 
	
	//注意 gcd 的定义，因为它是个 回溯 函数，必须定义返回值类型。
	private def gcd(a:Int, b:Int): Int =  //私有方法
		if (b==0) a else gcd(b, a % b)
	
	//方法重载，支持r+2，不支持2+r
	def + (i: Int) = 
		new Rational (number + i * denom, denom) 
	
	//隐式类型转换，支持2+r
	implicit def intToRational(x: Int) = new Rational(x) 
}


