package actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {
  //1) Actor System
  val actorSystem = ActorSystem("FirstActorSystem")
  println(actorSystem.name)

  //2) Create Actors
  //Word Count Actor
  class WordCountActor extends Actor {
    //Internal Data
    var totalWords = 0

    //Behavior
    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  //3) Instantiate our Actor
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter") //need to be unique
  //4) Communicate!
  wordCounter ! "I am learning Akka and it's pretty damn cool!" // ! known as "tell"
  anotherWordCounter ! "a different message"
  //asynchronous

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _ =>
    }
  }
  //How do we instantiate?

  val person = actorSystem.actorOf(Props(new Person("Bob"))) //discourage
  person ! "hi"


  object Person {
    def props(name: String) = Props(new Person(name))
  }

  val newPerson = actorSystem.actorOf(Person.props("Alice"))
  newPerson ! "hi"
}
