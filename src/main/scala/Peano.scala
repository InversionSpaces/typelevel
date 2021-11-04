enum BooleanT:
  case True, False

object BooleanT:
  type TrueT = BooleanT.True.type
  type FalseT = BooleanT.False.type

  trait ToBoolean[B <: BooleanT]:
    def value: Boolean

  given ToBoolean[TrueT] with
    val value = true

  given ToBoolean[FalseT] with
    val value = false

  def toBoolean[B <: BooleanT](using
      tb: ToBoolean[B]
  ): Boolean = tb.value

enum Nat:
  case Zero
  case Succ[N <: Nat]()

object Nat:
  type _0 = Nat.Zero.type
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]

  trait ToInt[N <: Nat]:
    def value: Int

  given ToInt[_0] with
    val value = 0

  given [N <: Nat](using ti: ToInt[N]): ToInt[Succ[N]] with
    val value = ti.value + 1

  def toInt[N <: Nat](using
      ti: ToInt[N]
  ): Int = ti.value

object NatComparision:
  import BooleanT._
  import Nat._

  type Compare[
      A <: Nat,
      B <: Nat,
      Lt <: BooleanT,
      Eq <: BooleanT,
      Gt <: BooleanT
  ] <: BooleanT =
    A match
      case _0 =>
        B match
          case _0      => Eq
          case Succ[_] => Lt
      case Succ[na] =>
        B match
          case _0       => Gt
          case Succ[nb] => Compare[na, nb, Lt, Eq, Gt]

  type <[A <: Nat, B <: Nat] = Compare[A, B, TrueT, FalseT, FalseT]
  type <=[A <: Nat, B <: Nat] = Compare[A, B, TrueT, TrueT, FalseT]
  type >=[A <: Nat, B <: Nat] = Compare[A, B, FalseT, TrueT, TrueT]
  type >[A <: Nat, B <: Nat] = Compare[A, B, FalseT, FalseT, TrueT]

object NatFold:
  import Nat._

  type FoldL[N <: Nat, R, I <: R, Fold[_ <: Nat, _ <: R] <: R] <: R =
    N match
      case _0      => I
      case Succ[n] => FoldL[n, R, Fold[N, I], Fold]

object NatArithmetics:
  import Nat._
  import NatFold._

  type :+:[A <: Nat, B <: Nat] = FoldL[A, Nat, B, :+:.Inc]
  object :+: :
    type Inc[_, A <: Nat] = Succ[A]

  type :*:[A <: Nat, B <: Nat] = FoldL[A, Nat, _0, :*:.Add[B]#Curried]
  object :*: :
    // Why does not this work?
    //type Add[I <: Nat] = [_, A <: Nat] =>> I :+: A
    trait Add[I <: Nat]:
      type Curried[_, A <: Nat] = I :+: A

  type :^:[A <: Nat, B <: Nat] = FoldL[B, Nat, _1, :^:.Mul[A]#Curried]
  object :^: :
    trait Mul[I <: Nat]:
      type Curried[_, A <: Nat] = I :*: A

import BooleanT._
import Nat._
import NatComparision._
import NatFold._
import NatArithmetics._

@main def testing: Unit =
  println(toBoolean[_0 < _1])
  println(toBoolean[_2 > _1])
  println(toBoolean[_2 <= _1])

  println(toInt[_5 :+: _4])
  // println(toInt[(_4 :*: _5) :+: _3])
  // println(toInt[_3 :^: _5]) // does not compile =(
