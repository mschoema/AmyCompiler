object TestErrors {

	def foo(x: Int, y: String, z: Boolean): Int = {
		x
	}

	def range(from: Int, to: Int): List = {
		if (to < from) {
			Nil() 
		} else {
			Cons(from)
		}
	}
	
	abstract class List
	case class Nil() extends List
	case class Cons(h: Int) extends List

	Std.printString("Correct");
	val y: Int = 4;
	foo(4,"Hello",true);

	val c: List = Cons(3);

	c match {
		case Nil() => 
			Std.printString("Nil")
		case Cons(4) => 
			Std.printString("Cons, 4")
		case Cons(x) => 
			Std.printInt(x)
	};

	Std.printInt(9)
}