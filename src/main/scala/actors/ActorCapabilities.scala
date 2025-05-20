//package actors
//
//import actors.ActorCapabilities.BankAccount.{BK_Deposit, BK_Withdraw, TransactionFailure, TransactionSuccess}
//import actors.ActorCapabilities.Counter.{DecrementCounter, IncrementCounter, PrintCounter}
//import actors.ActorCapabilities.Increment
//import actors.ActorCapabilities.Person.LiveTheLife
//import akka.actor.FSM.Failure
//import akka.actor.Status.Success
//import akka.actor.{Actor, ActorRef, ActorSystem, Props}
//import org.scalactic.Fail
//
//object ActorCapabilities extends App {
//  class SimpleActor extends Actor {
//    override def receive: Receive = {
//      case "Hi" => context.sender() ! "Hello, There!" //Replying
//      case message: String => println(s"[$self] I have received $message")
//      case number: Int => println(s"[simple actor] I have received a number: $number")
//      case SpecialMessage(contents) => println(s"[simple actor] I have received something special: $contents")
//      case ActorContext() => println(s"[simple actor] I have received ${context.self}")
//      case SendMessageToYourself(content) =>
//        self ! content
//      case SayHiTo(ref) => ref ! "Hi"
//      case WirelessPhoneMessage(content, ref) => ref forward (content + "s")
//
//
//    }
//  }
//  val system = ActorSystem("actorCapabilitiesDemo")
//  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")
//  simpleActor ! "hello, actor"
//  simpleActor ! 42 //who is the sender?
//
//  case class SpecialMessage(contents: String)
//  simpleActor ! SpecialMessage("some special contents")
//
//  case class ActorContext()
//  simpleActor ! ActorContext()
//
//  //1) Messages can be of any type
//    //a) Messages must be immutable
//    //b) Messages must be serializable
//    //In practice use case classes and case objects
//  //2) Actors have information about their content and about themselves
//    //a) context.self is the reference to the Actor
//    //b) you can use 'self' instead of context.self
//
//  case class SendMessageToYourself(content: String)
//  simpleActor ! SendMessageToYourself("I am an actor and I am proud of it")
//
//  //3) Actors can Reply to Messages
//  val alice = system.actorOf(Props[SimpleActor], "alice")
//  val bob = system.actorOf(Props[SimpleActor], "bob")
//
//  case class SayHiTo(ref: ActorRef)
//  alice ! SayHiTo(bob)
//  //alice ! "Hi" //deadLetter
//
//  //4) Dead Letters
//
//  //5) Forwarding Messages
//
//  case class WirelessPhoneMessage(content: String, ref: ActorRef)
//  alice ! WirelessPhoneMessage("Hi", bob)
//
//  /**
//   * Exercises
//   * 1. a Counter Actor
//   *  - Increment
//   *  - Decrement
//   *  - Print
//   *
//   * 2. a Bank account as an Actor
//   *  receives
//   *    - Deposit an amount
//   *    - Withdraw an amount
//   *    - Statement
//   *  replies with
//   *    - Success/Failure
//   * interact with some other kind of actor
//   */
//
//  class CounterActor extends Actor {
//    var counter = 0
//    override def receive: Receive = {
//      case Increment => counter+=1
//      case Decrement => counter-=1
//      case Print => println(s"Counter: $counter")
//    }
//  }
//
//  case class Increment()
//  case class Decrement()
//  case class Print()
//
//  val counterActor = system.actorOf(Props[CounterActor], "counter")
//  counterActor ! Increment
//  counterActor ! Print
//  counterActor ! Decrement
//  counterActor ! Print
//  (1 to 100).foreach(_ => counterActor ! Increment)
//  counterActor ! Print
//  (1 to 100).foreach(_ => counterActor ! Decrement)
//  counterActor ! Print
//
//  class BankActor extends Actor {
//    var balance = 0
//    override def receive: Receive = {
//      case Deposit(amount, ref) => {
//        if (amount > 0) {
//          balance += amount
//          ref ! Success
//        } else ref ! Failure
//      }
//      case Withdraw(amount, ref) => {
//        if (balance >= amount) {
//          balance -= amount
//          ref ! Success
//        } else ref ! Failure
//      }
//      case Statement(ref) => ref ! Print()
//
//      case Success => println("Operation Successful!")
//      case Failure => println("Operation Failed!")
//      case Print => println(s"Balance: $balance")
//    }
//  }
//
//  case class Deposit(amount: Int, ref: ActorRef)
//  case class Withdraw(amount: Int, ref: ActorRef)
//  case class Statement(ref: ActorRef)
//
//  val aBank = system.actorOf(Props[BankActor], "aBank")
//  val aBankAudit = system.actorOf(Props[BankActor], "aBankAudit")
//
//  aBank ! Deposit(1000, aBankAudit)
//  aBank ! Print
//  aBank ! Withdraw(500, aBankAudit)
//  aBank ! Print
//  aBank ! Deposit(-500, aBankAudit)
//  aBank ! Withdraw(1000, aBankAudit)
//  aBank ! Print
//
//  //ANOTHER SOLUTION
//
//  object Counter {
//    case object IncrementCounter
//    case object DecrementCounter
//    case object PrintCounter
//  }
//  class Counter extends Actor {
//    import Counter._
//    var count = 0
//
//    override def receive: Receive = {
//      case IncrementCounter => count += 1
//      case DecrementCounter => count -= 1
//      case PrintCounter => println(s"[counter] My current count is $count")
//
//    }
//  }
//  val counter = system.actorOf(Props[Counter], "myCounter")
//  (1 to 5).foreach(_ => counter ! IncrementCounter)
//  (1 to 3).foreach(_ => counter ! DecrementCounter)
//  counter ! PrintCounter
//
//  object BankAccount {
//    case class BK_Deposit(amount: Int)
//    case class BK_Withdraw(amount: Int)
//    case object Statement
//    case class TransactionSuccess(message: String)
//    case class TransactionFailure(reason: String)
//  }
//
//  class BankAccount extends Actor {
//    var funds = 0
//    override def receive: Receive = {
//      case BK_Deposit(amount) =>
//        if (amount < 0) sender() ! TransactionFailure("Invalid Deposit Amount")
//        else {
//          funds += amount
//          sender() ! TransactionSuccess(s"Successfully Deposited $amount")
//
//        }
//      case BK_Withdraw(amount) =>
//        if (amount < 0) sender() ! TransactionFailure("Inbalid Withdraw Amount")
//        else if (amount > funds) sender() ! TransactionFailure("Insufficient Funds")
//        else {
//          funds -= amount
//          sender() ! TransactionSuccess(s"Successfully Withdrew $amount")
//        }
//      case Statement => sender() ! s"Your Balance is $funds"
//    }
//  }
//  object Person {
//    case class LiveTheLife(account: ActorRef)
//  }
//  class Person extends Actor {
//    import Person._
//
//    override def receive: Receive = {
//      case LiveTheLife(account) =>
//        account ! BK_Deposit(10000)
//        account ! BK_Withdraw(90000)
//        account ! BK_Withdraw(500)
//        account ! Statement
//      case message => println(message.toString)
//    }
//  }
//  val account = system.actorOf(Props[BankAccount], "bankAccount")
//  val person = system.actorOf(Props[Person], "billionaire")
//
//  person ! LiveTheLife(account)
//}
