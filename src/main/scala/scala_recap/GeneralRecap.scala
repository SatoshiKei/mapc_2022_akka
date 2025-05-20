package scala_recap

import scala.annotation.tailrec
import scala.util.Try

object GeneralRecap extends App {
  val aCondition: Boolean = false
  var aVariable = 42
  aVariable += 1

  //expressions
  val aConditionedVal = if (aCondition) 42 else 65

  //code block
  val aCodeBlock = {
    if(aCondition) //74
    56
  }

  //types
  //Unit
  val theUnit: Unit = println("Hello, Scala") //Unit is the equivalent type of Void
  def aFunction(x: Int) = x+1
  //recursion - TAIL recursion

  @tailrec
  def factorial_aux(n: Int, acc: Int): Int =
    if (n<=0) acc
    else factorial_aux(n-1, acc * n)

  def factorial(n: Int): Int = {
    factorial_aux(n, 1)
  }

  println(factorial(5))
//This factorial implementation got no tail recursion annotation
  def fac(n: Int): Int =
    if (n<=0) 1
    else n*fac(n-1)

  println(fac(5))

  //OOP
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch!")
  }

  //method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog

  //anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  aCarnivore eat aDog

  //generics
  abstract class MyList[+A] //Covariant List allows subclass objects append to the superclass List
  // companion objects
  object MyList //Instantiates a Singleton Object, only one instance of a class

  abstract class _MyList[-A] //Contravariant List allows superclass objects append to the subclass List

  //case classes
  case class Person(name: String, age: Int) //Used A Lot

  //Exceptions
  val aPotentialFailure = try {
    throw new RuntimeException("I'm innocent, I swear!") //Nothing
  } catch {
    case e: Exception => "I caught an exception!"
  } finally {
    //side effects
    println("some logs")
  }

  //Functional programming

  //Function1[T1, T2] is the equivalent of f: (T1) => T2
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val incremented = incrementer(42)
  // incrementer.apply(42)

  val anonymousIncrementer = (x: Int) => x + 1
  //Int => Int === Function1[Int, Int]

  //Function Programing is all about working with functions as first-class
  val list = List(1,2,3).map(incrementer)
  //map = Higher Order Function
  println(list)

  // for comprehensions
  val pairs = for {
    num <- List(1,2,3,4)
    char <- List('a','b','c','d')
  } yield num + "_" + char

  //List(1,2,3,4).flatMap(num => List('a','b','c','d').map(char=> num + "_" + char))

  val nestedList = List(List(1,2,3), List(4,5,6))
  for { //for comprehensions can be used to make compound 'for each' clauses
    list <- nestedList //for each list in nestedList
    num <- list //for each num in list
  } print(num + " ")

  val _nestedList = List(List(1,2,3), List(4,5,6))
  val mappedList = for { //for comprehensions can be used to make compound 'for each' clauses
    list <- _nestedList //for each list in nestedList
    num <- list //for each num in list
  } yield num

  println
  mappedList.foreach { num =>
    print(num+" ")
  }

  //Seq, Array, List, Vector, Map, Tuples, Sets

  //"collections"
  //Options and Try
  val anOption = Some(2)
  val aTry = Try {
    throw new RuntimeException
  }

  //pattern matching
  val unknown = 2
  val order = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
    case _ => "I don't know my name"
  }

  //All the Patterns
}

