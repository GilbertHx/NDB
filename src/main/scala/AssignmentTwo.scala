object AssignmentTwo {
  def main(args: Array[String]) {

    println("-----------------Question 1---------------")

    println(weekDays("Monday"));

    println("-----------------Question 2---------------")

    val b1 = new BankAccount();
    val b2 = new BankAccount(30, false)
    println(b1.deposits(amountToDeposit = 3))
    println(b2.withdraw(amountToWithdraw = 5))

    println("-----------------Question 3---------------")

    val p2 = new Person("Jane", "Smith")
    val p4 = new Person("Nshuti", "Rwaka")

    println(greetings(new Person("John", "Doe")))
    println(greetings(p2))
    println(greetings(new Person("50", "Cent")))
    println(greetings(p4))

    println("-----------------Question 4---------------")

    val a = 3;
    println(calc(func1, calc(func1, calc(func1, a))))

    println("-----------------Question 5---------------")

    object pers extends Person5("Joe", "Doe") with Employee
    pers.salary_(2000)
    pers.taxToPay
    object stu extends Person5("Joe", "Doe") with Student
    stu.taxToPay
    object tchr extends Person5("Joe", "Doe") with Teacher
    tchr.salary_(2000)
    tchr.taxToPay
    object w_stud extends Person5("Joe", "Doe") with Employee with Student
    w_stud.salary_(2000)
    w_stud.taxToPay
    object s_wrk extends Person5("Joe", "Doe") with Student with Employee
    s_wrk.salary_(2000)
    s_wrk.taxToPay
  }

  def weekDays(x:String): String = x match {

    case "Monday" => "work"
    case "Tuesday" => "work"
    case "Wednesday" => "work"
    case "Thursday" => "work"
    case "Friday" => "work"
    case "Saturday" => "weekend"
    case "Sunday" => "weekend"
    case _ => "no such day"
  }

  class BankAccount(initialBalance: Int = 0, resetBalance: Boolean = true) {
    val currentBalance: Int = initialBalance

    def deposits(amountToDeposit: Int): Int = currentBalance + amountToDeposit

    def withdraw(amountToWithdraw: Int): Int = currentBalance - amountToWithdraw
  }

  class Person(var firstName: String, var lastName: String)

  def greetings(person: Person): String = person.firstName match{
    case "John" => "Hello, Sir"
    case "50" => "What's up, Homie?"
    case "Jane" => "Hi, Beautiful!"
    case "Nshuti" => "Kiri gute, Musaza?"
    case _ => "Hey"
  }

  def func1(x: Int): Int = {
    x+1
  }

  def calc(f: (Int) => Int, z: Int): Int = {
    f(z)
  }

  abstract class Person5(private var firstName: String, private var lastName: String) {
    def taxToPay: Unit
  }
  trait Employee extends Person5 {
    private var sal: Double = _
    def salary = sal
    def salary_(s: Double): Unit = sal = s
    override def taxToPay: Unit = println("Your tax to pay is: " + sal * 0.2)
  }
  trait Student extends Person5 {
    override def taxToPay: Unit = println("Your tax to pay is: 0")
  }
  trait Teacher extends Employee {
    override def taxToPay: Unit = println("Your tax to pay is: " + salary * 0.1)
  }


}