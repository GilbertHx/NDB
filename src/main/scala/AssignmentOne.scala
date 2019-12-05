import scala.collection.immutable.List

object AssignmentOne {
  def main(args:Array[String])
  {
    val daysOfTheWeek: List[String] = List("Monday", "Tuesday",
      "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

    println("-----------------Question 1---------------")

    println(questionOneA(daysOfTheWeek))
    println(questionOneB(daysOfTheWeek))
    println(questionOneC(daysOfTheWeek))

    println("-----------------Question 2---------------")

    println(questionTwoA(daysOfTheWeek))
    println(questionTwoB(daysOfTheWeek))

    println("-----------------Question 3---------------")

    println(questionThree(daysOfTheWeek, ""));

    println("-----------------Question 4---------------")

    println(questionFourA(daysOfTheWeek))
    println(questionFourB(daysOfTheWeek))
    println(questionFourC(daysOfTheWeek))

    println("-----------------Question 5---------------")

    val myMap : Map[String, Double] =
      Map("Coffee" -> 45, "Sugar" -> 35, "Milk" -> 10, "Juice" -> 15);
    val newMap = myMap map { case (key, value) => (key, value - value * 10 / 100)}
    println(newMap)

    println("-----------------Question 6---------------")

    val nums: List[Int] = List(1, 2, 3, 4)
    println(questionSix(nums))

    println("-----------------Question 7---------------")

    val numSeven: List[Int] = List(1, -2, -3, 4)
    questionSeven(numSeven)

    println("-----------------Question 8---------------")

    val tupleOne = (1,2.2,"Now")
    questionEight(tupleOne)

    println("-----------------Question 9---------------")

    println(questionNine(-1.3 :: 2.1 :: 0.0 :: 3.4 :: 0.0 :: Nil))

    println("-----------------Question 10---------------")

    println("Option is utilized when a method returns a value which can even be null." + "\n" +
      "The instance of an Option that is returned here can be an instance of Some class or None")

    val ar: Array[Int] = Array(3,4,9,4,2,0,4,7,3,8)

    val res1: Option[Int] = ar.find(_ < 3);
    val res2: Option[Int] = ar.find(_ > 10);

    println(res2.getOrElse("Not Found"))
    println(res1.map(_ * 2));
  }

  def questionOneA(mList: List[String]): String = {
    var mString: String = ""

    for (s <- mList.indices) {
      if(s==0) mString = mString + mList(s) else mString = mString + ", " + mList(s)
    }
    mString
  }

  def questionOneB(mList: List[String]): String = {
    var mString: String = "";

    val filteredList: List[String] = mList.filter(p => p(0) == 'S');

    for (s <- filteredList.indices){
      if(s == 0) mString = mString + filteredList(s) else mString = mString + ", " + filteredList(s)
    }
    mString
  }

  def questionOneC(mList: List[String]): String = {
    var mString: String = "";
    var left: Int = 0
    val right: Int = mList.size;

    while (left < right){
      if(left == 0) mString = mString + mList(left) else mString = mString + ", " + mList(left)
      left += 1
    }

    mString;
  }

  def questionTwoA(mList: List[String]): String = {
    if (mList.tail.isEmpty)
      return mList.head
    mList.head + ", " + questionTwoA(mList.tail);
  }

  def questionTwoB(mList: List[String]): String = {
    if (mList.tail.isEmpty)
      return mList.head
    questionTwoB(mList.tail) + ", " + mList.head
  }

  @scala.annotation.tailrec
  def questionThree(l: List[String], s:String): String = {
    if (l.tail.isEmpty)
      s+l.head
    else   questionThree(l.tail,s+l.head + "")
  }

  def questionFourA(list: List[String]): String = {
    list.slice(1, list.size).foldLeft(list.head)(_ + "," + _)
  }

  def questionFourB(list: List[String]): String = {
    list.slice(0, list.size - 1).foldRight(list.last)(_ + "," + _)
  }

  def questionFourC(list: List[String]): String = {
    (list.filter(p => p(0) == 'S') slice(1, list.count(p => p(0) == 'S'))).foldLeft(list.filter(p => p(0) == 'S')(0))(_ + "," + _)
  }

  def questionSix(myList: List[Int]): List[Int]= {
    val newList = myList.map(e => e + 1)
    newList
  }

  def questionSeven(myList: List[Int]): List[Int]= {
    val newList = myList.map(e => if (e < 0) -e else e )
    newList
  }

  def questionEight(l: (Int, Double, String)): Unit = {
    l.productIterator.foreach{
      i => println(i);
    }
  }

  def questionNine(l: List[Double]): List[Double] = {
    if (l.isEmpty) l
    else if (l.head == 0) questionNine(l.tail)
    else l.head :: questionNine(l.tail)
  }
}
