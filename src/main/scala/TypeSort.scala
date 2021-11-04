import org.tpolecat.typename.TypeName

object Peano:
  enum Nat:
    case _0
    case Succ[N <: Nat]()

  type _0 = Nat._0.type
  type _1 = Nat.Succ[_0]
  type _2 = Nat.Succ[_1]
  type _3 = Nat.Succ[_2]
  type _4 = Nat.Succ[_3]
  type _5 = Nat.Succ[_4]

  trait <[A <: Nat, B <: Nat]
  object < :
    given base[N <: Nat]: <[_0, Nat.Succ[N]]()
    given ind[A <: Nat, B <: Nat](using A < B): <[Nat.Succ[A], Nat.Succ[B]]()

    def apply[A <: Nat, B <: Nat](using A < B) = summon[A < B]

  trait <=[A <: Nat, B <: Nat]
  object <= :
    given base[A <: Nat, B <: Nat](using A < B): <=[A, B]()
    given ind[A <: Nat, B <: Nat](using A =:= B): <=[A, B]()

    def apply[A <: Nat, B <: Nat](using A <= B) = summon[A <= B]

object NatList:
  import Peano._

  enum HList:
    case HNil
    case ::[H <: Nat, T <: HList]()

  type HNil = HList.HNil.type
  type ::[H <: Nat, T <: HList] = HList.::[H, T]

  trait Concat[A <: HList, B <: HList, R <: HList]
  object Concat:
    given base[A <: HList]: Concat[HNil, A, A]()
    given ind[H <: Nat, A <: HList, B <: HList, R <: HList](using
        Concat[A, B, R]
    ): Concat[H :: A, B, H :: R]()

    def apply[A <: HList, B <: HList, R <: HList](using Concat[A, B, R]) =
      summon[Concat[A, B, R]]

object QSort:
  import Peano._
  import NatList._

  trait Partition[H <: HList, L <: HList, R <: HList]
  object Partition:
    given base1: Partition[HNil, HNil, HNil]()
    given base2[N <: Nat]: Partition[N :: HNil, N :: HNil, HNil]()
    given ind1[N <: Nat, P <: Nat, H <: HList, L <: HList, R <: HList](using
        Partition[N :: H, N :: L, R],
        N <= P
    ): Partition[N :: P :: H, N :: L, P :: R]()
    given ind2[N <: Nat, P <: Nat, H <: HList, L <: HList, R <: HList](using
        Partition[N :: H, N :: L, R],
        P < N
    ): Partition[N :: P :: H, N :: P :: L, R]()

    def apply[H <: HList, L <: HList, R <: HList](using Partition[H, L, R]) =
      summon[Partition[H, L, R]]

  trait Sort[H <: HList] { type Result <: HList }
  object Sort:
    type Aux[H <: HList, R <: HList] = Sort[H] { type Result = R }

    given base: Aux[HNil, HNil] = new Sort[HNil] { type Result = HNil }
    given ind[
        N <: Nat,
        H <: HList,
        L <: HList,
        R <: HList,
        SL <: HList,
        SR <: HList,
        S <: HList
    ](using
        Partition[N :: H, N :: L, R],
        Aux[L, SL],
        Aux[R, SR],
        Concat[SL, N :: SR, S]
    ): Aux[N :: H, S] = new Sort[N :: H] { type Result = S }

    def apply[H <: HList](using s: Sort[H]): Aux[H, s.Result] = s

import Peano.{_, given}
import NatList.{_, given}
import QSort.{_, given}

def typeRepr[A](value: A)(using typename: TypeName[A]): String =
  typename.value

@main def main: Unit =
  val testNat = <=[_1, _1]
  val testList = Concat[
    _0 :: _1 :: HNil,
    _3 :: _4 :: HNil,
    _0 :: _1 :: _3 :: _4 :: HNil
  ]
  val testPartition = Partition[
    _3 :: _3 :: _2 :: _4 :: _4 :: _0 :: HNil,
    _3 :: _2 :: _0 :: HNil,
    _3 :: _4 :: _4 :: HNil
  ]
  val testSort = Sort[
    _5 :: _5 :: _3 :: _4 :: _0 :: _1 :: HNil
  ]
  println(
    typeRepr(testSort)
      .replace("Peano.Nat.", "")
      .replace("NatList.HList.", "")
  )
