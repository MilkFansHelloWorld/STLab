package effpi_sandbox.Calculator

import effpi.process._
import effpi.process.dsl._
import effpi.channel.Channel
import effpi.channel.{InChannel, OutChannel}
import scala.concurrent.duration.Duration

case class Pair(x: Int, y: Int)
case class Multiply(x : Pair)
case class Quit()
case class Result(x : Int)
case class Sum(x : Pair)
case class Terminate()

sealed abstract class RecT0[A]() extends RecVar[A]("RecT0")
case object RecT0 extends RecT0[Unit]

type S[C0 <: InChannel[Quit | Multiply | Sum], C1 <: OutChannel[Result], C2 <: OutChannel[Result], C3 <: OutChannel[Terminate]] = Rec[RecT0, In[C0, Quit | Multiply | Sum, (x0 : Quit | Multiply | Sum) => S0[x0.type, C1, C2, C3]]]

type S0[X0 <: Quit | Multiply | Sum, C1 <: OutChannel[Result], C2 <: OutChannel[Result], C3 <: OutChannel[Terminate]] <: Process = X0 match {
  case Quit => Out[C3, Terminate] >>: PNil
  case Multiply => Out[C2, Result] >>: Loop[RecT0]
  case Sum => Out[C1, Result] >>: Loop[RecT0]
}

type Cl[C0 <: OutChannel[Quit | Multiply | Sum], C1 <: InChannel[Result], C2 <: InChannel[Result], C3 <: InChannel[Terminate]] = Rec[RecT0, ((Out[C0, Quit] >>: In[C3, Terminate, (x0 : Terminate) => PNil]) | (Out[C0, Multiply] >>: In[C2, Result, (x0 : Result) => Loop[RecT0]]) | (Out[C0, Sum] >>: In[C1, Result, (x0 : Result) => Loop[RecT0]]))]



implicit val timeout: Duration = Duration("60 seconds")

def s(c0 : InChannel[Quit | Multiply | Sum], c1 : OutChannel[Result], c2 : OutChannel[Result], c3 : OutChannel[Terminate]) : S[c0.type, c1.type, c2.type, c3.type] = {
  rec(RecT0) {
    println("-- S entering recursion body; t = RecT0")
    println(s"-- S expecting (Quit | Multiply | Sum) on c0 ($c0)")
    receive(c0) {(x0 : Quit | Multiply | Sum) =>
      println(s"-- S received (Quit | Multiply | Sum) on c0 ($c0)")
      s0(x0, c1, c2, c3)
    }
  }
}

def s0(x : Quit | Multiply | Sum, c1 : OutChannel[Result], c2 : OutChannel[Result], c3 : OutChannel[Terminate]) : S0[x.type, c1.type, c2.type, c3.type] = x match {
  case y : Quit => {
    println(s"-- S received Quit")
    println(s"-- S sending Terminate on c3 ($c3)")
    send(c3, new Terminate()) >> {
      println(s"-- S sent Terminate on c3 ($c3)")
      println("-- S exits")
      nil
    }
  }
  case y : Multiply => {
    println(s"-- S received Multiply")
    println(s"-- S sending Result on c2 ($c2)")
    send(c2, new Result(y.x.x * y.x.y)) >> {
      println(s"-- S sent Result on c2 ($c2)")
      println("-- S recursing; t = RecT0")
      loop(RecT0)
    }
  }
  case y : Sum => {
    println(s"-- S received Sum")
    println(s"-- S sending Result on c1 ($c1)")
    send(c1, new Result(y.x.x + y.x.y)) >> {
      println(s"-- S sent Result on c1 ($c1)")
      println("-- S recursing; t = RecT0")
      loop(RecT0)
    }
  }
}

def cl(c0 : OutChannel[Quit | Multiply | Sum], c1 : InChannel[Result], c2 : InChannel[Result], c3 : InChannel[Terminate]) : Cl[c0.type, c1.type, c2.type, c3.type] = {
  rec(RecT0) {
    println("-- Cl entering recursion body; t = RecT0")
    val x0 = 2
    if (x0 == 0) {
      send(c0, new Quit()) >> {
        println(s"-- Cl sent Quit on c0 ($c0)")
        println(s"-- Cl expecting Terminate on c3 ($c3)")
        receive(c3) {(x1 : Terminate) => 
          println(s"-- Cl received Terminate on c3 ($c3)")
          println("-- Cl exits")
          nil
        }
      }
    } else if (x0 == 1) {
      send(c0, new Multiply(new Pair(5, 2))) >> {
        println(s"-- Cl sent Multiply on c0 ($c0)")
        println(s"-- Cl expecting Result on c2 ($c2)")
        receive(c2) {(x1 : Result) => 
          val result = x1.x
          println(s"-- Cl received Result $result on c2 ($c2)")
          println("-- Cl recursing; t = RecT0")
          loop(RecT0)
        }
      }
    } else {
      send(c0, new Sum(new Pair(3, 4))) >> {
        println(s"-- Cl sent Sum on c0 ($c0)")
        println(s"-- Cl expecting Result on c1 ($c1)")
        receive(c1) {(x1 : Result) => 
          val result = x1.x
          println(s"-- Cl received Result $result on c1 ($c1)")
          println("-- Cl recursing; t = RecT0")
          loop(RecT0)
        }
      }
    } 

  }
}



object Main {
  def main() : Unit = main(Array())

  def main(args : Array[String]) = {
    var c0 = Channel[Quit | Multiply | Sum]()
    var c1 = Channel[Result]()
    var c2 = Channel[Result]()
    var c3 = Channel[Terminate]()

    eval(par(s(c0, c1, c2, c3), cl(c0, c1, c2, c3)))
  }
}
