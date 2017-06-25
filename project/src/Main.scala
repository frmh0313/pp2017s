package pp201701.proj
import pp201701.proj.Data.DataBundle._
//import scala.collection.immutable._
//import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap

object Value {
  sealed abstract class Val
  case class VInt(a: String) extends Val
  case class VTrue() extends Val
  case class VFalse() extends Val
  case class VPair(a: Val, b: Val) extends Val
  case class VNil() extends Val
  case class VName(a: String) extends Val
//  case class VFun(p: List[String], body: Val) extends Val // body는 Expr..?
  case class VFun(p: List[String], body: Expr) extends Val
  case class VException(ex: Main.EvalException) extends Val

  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isFun(a:A) : Boolean
  }

  implicit val valConv : ConvertToScala[Val] = new ConvertToScala[Val] {
    def toInt(a: Val) : Option[Int] =
      a match {
        case VInt(n) => Some(Integer.parseInt(n))
        case _ => None
      }

    def toBool(a: Val) : Option[Boolean] =
      a match {
        case VTrue() => Some(true)
        case VFalse() => Some(false)
        case _ => None
      }

    def toPair(a: Val) : Option[(Val, Val)] =
      a match {
        case VPair(a, b) => Some((a, b))
        case _ => None
      }

    def isNil(a: Val) : Boolean = a == VNil()
    def isFun(a: Val) : Boolean = a match {
      case VFun(l, b) => true
      case _ => false
    }
  }
}

object Main {

  import Value._

  class EvalException(val msg: String) extends Exception

  sealed abstract class Binded
  case class ValBinded(val name: String, val value: Val) extends Binded
  case class DefBinded(val name: String, val expr: Expr) extends Binded

  def myeval(e: Expr): Val = {
    def assignArgs(e: Expr, fargs: List[String], args: List[Expr]): Expr = {
      def assigntoList(l: List[Expr], farg: String, arg: Expr): List[Expr] = {
        def assignToListAux(res: List[Expr], list: List[Expr], farg: String, arg: Expr): List[Expr] =
          list match {
            case Nil => res
            case hd :: tl => assignToListAux(res ++ (assign(hd, farg, arg) :: Nil), tl, farg, arg)
          }

        assignToListAux(Nil, l, farg, arg)
      }

      def assign(e: Expr, farg: String, arg: Expr): Expr = {
        e match {
          case EConst(c) => e
          case EName(n) =>
            if (farg == n) arg
            else e

          case ECons(e1, e2) =>
            e1 match {
              case EName(farg) =>
                e2 match {
                  case EName(farg) =>
                    val ret = ECons(arg, arg)
                    ret
                  case EName(_) | EConst(_) =>
                    val ret = ECons(arg, e2)
                    ret
                  case _ =>
                    val ret = ECons(arg, assign(e2, farg, arg))
                    ret
                }
              case EName(_) | EConst(_) =>
                e2 match {
                  case EName(farg) =>
                    val ret = ECons(e1, e2)
                    ret
                  case EName(_) | EConst(_) =>
                    val ret = ECons(e1, e2)
                    ret
                  case _ =>
                    val ret = ECons(e1, assign(e2, farg, arg))
                    ret
                }
              case _ =>
                e2 match {
                  case EName(farg) =>
                    val ret = ECons(assign(e1, farg, arg), arg)
                    ret
                  case EName(_) | EConst(_) =>
                    val ret = ECons(assign(e1, farg, arg), e2)
                    ret
                  case _ =>
                    val ret = ECons(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret
                }
            }

          case EHd(el) =>
            el match {
              case EName(farg) =>
                val ret = EHd(arg)
                ret
              case EName(_) | EConst(_) =>
                val ret = EHd(el)
                ret
              case _ =>
                val ret = EHd(assign(el, farg, arg))
                ret
            }

          case ETl(el) =>
            el match {
              case EName(farg) =>
                val ret = ETl(arg)
                ret
              case EName(_) | EConst(_) =>
                val ret = ETl(el)
                ret
              case _ =>
                val ret = ETl(assign(el, farg, arg))
                ret
            }


          case EFun(p, eb) => EFun(p, assign(eb, farg, arg))

          case EApp(ef, eargs) =>
            eargs match {
              case Nil => ef
              case _ => EApp(assign(ef, farg, arg), assigntoList(eargs, farg, arg))
            }

          case EPlus(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EPlus(arg, arg)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EPlus(arg, e2)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EPlus(arg, e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EPlus(arg, assign(e2, farg, arg))
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EPlus(e1, arg)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EPlus(e1, e2)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EPlus(e1, e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EPlus(e1, assign(e2, farg, arg))
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EPlus(e1, arg)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EPlus(e1, e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EPlus(e1, e2)
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EPlus(e1, assign(e2, farg, arg))
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EPlus(assign(e1, farg, arg), arg)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EPlus(assign(e1, farg, arg), e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EPlus(assign(e1, farg, arg), e2)
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EPlus(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                }
            }

          case EMinus(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMinus(arg, arg)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMinus(arg, e2)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMinus(arg, e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMinus(arg, assign(e2, farg, arg))
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMinus(e1, arg)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMinus(e1, e2)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMinus(e1, e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMinus(e1, assign(e2, farg, arg))
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMinus(e1, arg)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMinus(e1, e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMinus(e1, e2)
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMinus(e1, assign(e2, farg, arg))
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                      case _ => ret
                    }
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMinus(assign(e1, farg, arg), arg)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMinus(assign(e1, farg, arg), e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMinus(assign(e1, farg, arg), e2)
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMinus(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                }
            }

          case EMult(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMult(arg, arg)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMult(arg, e2)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMult(arg, e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMult(arg, assign(e2, farg, arg))
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMult(e1, arg)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMult(e1, e2)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMult(e1, e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMult(e1, assign(e2, farg, arg))
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMult(e1, arg)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMult(e1, e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMult(e1, e2)
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMult(e1, assign(e2, farg, arg))
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                      case _ => ret
                    }
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMult(assign(e1, farg, arg), arg)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMult(assign(e1, farg, arg), e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMult(assign(e1, farg, arg), e2)
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMult(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                      case _ => ret
                    }
                }
            }

          case EEq(e1, e2) =>
            e1 match {
              case EName(x) =>
                if (x == farg)
                  e2 match {
                    case EName(y) =>
                      if (y == farg) {
                        val ret = EEq(arg, arg)
                        ret
                      }
                      else {
                        val ret = EEq(arg, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EEq(arg, e2)
                      ret
                    case _ =>
                      val ret = EEq(arg, assign(e2, farg, arg))
                      ret
                  }
                else
                  e2 match {
                    case EName(y) =>
                      if (y == farg) {
                        val ret = EEq(e1, arg)
                        ret
                      }
                      else {
                        val ret = EEq(e1, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EEq(e1, e2)
                      ret
                    case _ =>
                      val ret = EEq(e1, assign(e2, farg, arg))
                      ret
                  }
              case EConst(_) =>
                e2 match {
                  case EName(y) =>
                    if (y == farg) {
                      val ret = EEq(e1, arg)
                      ret
                    }
                    else {
                      val ret = EEq(e1, e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = EEq(e1, e2)
                    ret
                  case _ =>
                    val ret = EEq(e1, assign(e2, farg, arg))
                    ret
                }
              case _ =>
                val ret = EEq(assign(e1, farg, arg), assign(e2, farg, arg))
                ret
            }

          case ELt(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = ELt(arg, arg)
                        ret
                      }
                      else {
                        val ret = ELt(arg, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = ELt(arg, e2)
                      ret
                    case _ =>
                      val ret = ELt(arg, assign(e2, farg, arg))
                      ret
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = ELt(e1, arg)
                        ret
                      }
                      else {
                        val ret = ELt(e1, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = ELt(e1, e2)
                      ret
                    case _ =>
                      val ret = ELt(e1, assign(e2, farg, arg))
                      ret
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = ELt(e1, arg)
                      ret
                    }
                    else {
                      val ret = ELt(e1, e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = ELt(e1, e2)
                    ret
                  case _ =>
                    val ret = ELt(e1, assign(e2, farg, arg))
                    ret
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = ELt(assign(e1, farg, arg), arg)
                      ret
                    }
                    else {
                      val ret = ELt(assign(e1, farg, arg), e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = ELt(assign(e1, farg, arg), e2)
                    ret
                  case _ =>
                    val ret = ELt(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret
                }
            }

          case EGt(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EGt(arg, arg)
                        ret
                      }
                      else {
                        val ret = EGt(arg, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EGt(arg, e2)
                      ret
                    case _ =>
                      val ret = EGt(arg, assign(e2, farg, arg))
                      ret
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EGt(e1, arg)
                        ret
                      }
                      else {
                        val ret = EGt(e1, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EGt(e1, e2)
                      ret
                    case _ =>
                      val ret = EGt(e1, assign(e2, farg, arg))
                      ret
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EGt(e1, arg)
                      ret
                    }
                    else {
                      val ret = EGt(e1, e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = EGt(e1, e2)
                    ret
                  case _ =>
                    val ret = EGt(e1, assign(e2, farg, arg))
                    ret
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EGt(assign(e1, farg, arg), arg)
                      ret
                    }
                    else {
                      val ret = EGt(assign(e1, farg, arg), e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = EGt(assign(e1, farg, arg), e2)
                    ret
                  case _ =>
                    val ret = EGt(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret
                }
            }

          case EIf(econd, et, ef) =>
            val cond = assign(econd, farg, arg)
            val newet = assign(et, farg, arg)
            val newef = assign(ef, farg, arg)
            EIf(cond, newet, newef)
        }
      }

      fargs match {
        case Nil => e
        case hd :: tl =>
          assignArgs(assign(e, hd, args.head), tl, args.tail)
      }
    }

    def searchEnvironment(name: String, env: List[Binded]): Option[Binded] = {
      def go(l: List[Binded]): Option[Binded] =
        l match {
          case Nil => None
          case ValBinded(n, v) :: tl =>
            if (name == n) Some(ValBinded(n, v))
            else go(tl)
          case DefBinded(n, e) :: tl =>
            if (name == n) Some(DefBinded(n, e))
            else go(tl)
        }

      go(env)
    }

    def evalBinds(b: List[Bind]): List[Binded] = {

      def evalBindsAux(res: List[Binded], bl: List[Bind]): List[Binded] =
        bl match {
          case Nil => res
          case (bind, name, exp) :: tl =>
            bind match {
              case BDEF() => evalBindsAux(new DefBinded(name, exp) :: res, tl)
              case BVAL() => evalBindsAux(new ValBinded(name, evalWithEnvironment(exp, res)) :: res, tl)
            }
        }
      evalBindsAux(Nil, b)
    }

    def valToExpr(a: Val): Expr =
      a match {
        case VInt(a) => EConst(CInt(Integer.parseInt(a)))
        case VTrue() => EConst(CTrue())
        case VFalse() => EConst(CFalse())
        case VPair(a, b) => ECons(valToExpr(a), valToExpr(b))
        case VNil() => EConst(CNil())
        case VName(a) => EName(a)
      }

    def evalWithEnvironment(e: Expr, env: List[Binded]): Val = {
      env match {
        case Nil => {
          e match {
            case EConst(c) =>
              val str = ExprToString.const_toString(c)
              if (str == "true") VTrue()
              else if (str == "false") VFalse()
              else if (str == "nil") VNil()
              else VInt(str)

            case EName(x) => VException(new EvalException("Definition not found"))

            case EIf(e1, e2, e3) =>
              evalWithEnvironment(e1, Nil) match {
                case VTrue() => evalWithEnvironment(e2, Nil)
                case VFalse() => evalWithEnvironment(e3, Nil)
                case _ => VException(new EvalException("Invalid expression for condition."))
              }

            case ECons(e1, e2) => VPair(evalWithEnvironment(e1, Nil), evalWithEnvironment(e2, Nil))

            case EHd(el) =>
              evalWithEnvironment(el, Nil) match {
                case VPair(a, b) => a
                case _ => VException(new EvalException("Invalid expression for head operation."))
              }

            case ETl(el) =>
              evalWithEnvironment(el, Nil) match {
                case VPair(a, b) => b
                case _ => VException(new EvalException("Invalid expression for tail operation."))
              }

            case EFun(p, eb) =>
              p match {
                case Nil => evalWithEnvironment(eb, Nil) // need check
                case _ =>
                  def listEval(l: List[String]): List[Val] =
                    l match {
                      case Nil => Nil
                      case hd :: tl => VName(hd) :: listEval(tl)
                    }

                  VFun(p, eb)
              }

            case EApp(ef, eargs) =>
              ef match {
                case EFun(fargs, eb) =>
                  if (fargs.length != eargs.length) VException(new EvalException("Invalid number of arguments."))
                  else evalWithEnvironment(assignArgs(eb, fargs, eargs), Nil)
                case _ => VException(new EvalException("Invalid function call")) // EName()도 마찬가지.
              }
            case ELet(binds, eb) => evalWithEnvironment(eb, evalBinds(binds))

            case EPlus(e1, e2) =>
              evalWithEnvironment(e1, Nil) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, Nil) match {
                    case VInt(b) => VInt((Integer.parseInt(a) + Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for addition."))
                  }
                case _ => VException(new EvalException("Invalid operands for addition."))
              }

            case EMinus(e1, e2) =>
              evalWithEnvironment(e1, Nil) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, Nil) match {
                    case VInt(b) => VInt((Integer.parseInt(a) - Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
              }

            case EMult(e1, e2) =>
              evalWithEnvironment(e1, Nil) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, Nil) match {
                    case VInt(b) => VInt((Integer.parseInt(a) * Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
              }

            case EEq(e1, e2) =>
              evalWithEnvironment(e1, Nil) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, Nil) match {
                    case VInt(b) => if (a == b) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for EQ of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for EQ of integers."))
              }

            case ELt(e1, e2) =>
              evalWithEnvironment(e1, Nil) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, Nil) match {
                    case VInt(b) => if (Integer.parseInt(a) < Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for less than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for less than operation."))
              }

            case EGt(e1, e2) =>
              evalWithEnvironment(e1, Nil) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, Nil) match {
                    case VInt(b) => if (Integer.parseInt(a) > Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for greater than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for greater than operation."))
              }
            case _ => VException(new EvalException("Invalid expression."))
          }
        }

        case _ =>
          e match {
            case EConst(c) =>
              val str = ExprToString.const_toString(c)
              if (str == "true") VTrue()
              else if (str == "false") VFalse()
              else if (str == "nil") VNil()
              else VInt(str)

            case EName(x) =>
              searchEnvironment(x, env) match {
                case None => VException(new EvalException("Definition not found."))
                case Some(ValBinded(x, v)) => v
                case Some(DefBinded(x, ex)) => evalWithEnvironment(ex, env) // check.
              }

            case EIf(e1, e2, e3) =>
              evalWithEnvironment(e1, env) match {
                case VTrue() => evalWithEnvironment(e2, env)
                case VFalse() => evalWithEnvironment(e3, env)
              }

            case ECons(e1, e2) => VPair(evalWithEnvironment(e1, env), evalWithEnvironment(e2, env))

            case EHd(el) =>
              evalWithEnvironment(el, env) match {
                case VPair(a, b) => a
                case _ => VException(new EvalException("Invalid expression for head operation."))
              }

            case ETl(el) =>
              evalWithEnvironment(el, env) match {
                case VPair(a, b) => b
                case _ => VException(new EvalException("Invalid expression for tail operation."))
              }

            case EFun(p, eb) =>
              p match {
                case Nil => VFun(Nil, eb)
                case _ =>
                  def listEval(l: List[String]): List[Val] =
                    l match {
                      case Nil => Nil
                      case hd :: tl => VName(hd) :: listEval(tl)
                    }
                  VFun(p, eb)
              }

            case EApp(ef, eargs) =>
              ef match {
                case EFun(fargs, eb) =>
                  if (fargs.length != eargs.length) VException(new EvalException("Invalid number of arguments."))
                  else evalWithEnvironment(assignArgs(eb, fargs, eargs), env)
                case EName(x) =>
                  searchEnvironment(x, env) match {
                    case Some(binded) =>
                      binded match {
                        case DefBinded(n, expr) => evalWithEnvironment(EApp(expr, eargs), env)
                        case ValBinded(n, vl) => evalWithEnvironment(EApp(valToExpr(vl), eargs), env)
                      }
                    case None => VException(new EvalException("Invalid in EApp"))
                  } // environment에서 찾아서 expression을 가져다 놓고 eargs에 있는 거 대입
                case _ => evalWithEnvironment(ef, env) // need to check??
              }

            case ELet(binds, eb) => {
              def joinBinds(inner: List[Binded], outer: List[Binded]): List[Binded] = {
                def singleJoin(bind: Binded, l: List[Binded]): List[Binded] = {
                  def go(prev: List[Binded], next: List[Binded]): List[Binded] =
                    next match {
                      case Nil => prev
                      case hd :: tl =>
                        hd match {
                          case ValBinded(n, v) =>
                            bind match {
                              case ValBinded(ni, vi) =>
                                if (ni == n) prev ++ (bind :: tl)
                                else go(prev ++ (hd :: Nil), tl)
                              case DefBinded(ni, vi) =>
                                if (ni == n) prev ++ (bind :: tl)
                                else go(prev ++ (hd :: Nil), tl)
                            }

                          case DefBinded(n, v) =>
                            bind match {
                              case ValBinded(ni, vi) =>
                                if (ni == n) prev ++ (bind :: tl)
                                else go(prev ++ (hd :: Nil), tl)
                            }
                        }
                    }

                  go(Nil, l)
                }

                inner match {
                  case Nil => outer
                  case hd :: tl => joinBinds(tl, singleJoin(hd, outer))
                }
              }

              val environment = joinBinds(evalBinds(binds), env)
              evalWithEnvironment(eb, environment)
            }

            case EPlus(e1, e2) =>
              evalWithEnvironment(e1, env) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, env) match {
                    case VInt(b) => VInt((Integer.parseInt(a) + Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for addition of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for addition of integers."))
              }

            case EMinus(e1, e2) =>
              evalWithEnvironment(e1, env) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, env) match {
                    case VInt(b) => VInt((Integer.parseInt(a) - Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
              }

            case EMult(e1, e2) =>
              evalWithEnvironment(e1, env) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, env) match {
                    case VInt(b) => VInt((Integer.parseInt(a) * Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
              }

            case EEq(e1, e2) =>
              evalWithEnvironment(e1, env) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, env) match {
                    case VInt(b) => if (a == b) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for EQ of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for EQ of integers."))
              }

            case ELt(e1, e2) =>
              evalWithEnvironment(e1, env) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, env) match {
                    case VInt(b) => if (Integer.parseInt(a) < Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for less than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for less than operation."))
              }

            case EGt(e1, e2) =>
              evalWithEnvironment(e1, env) match {
                case VInt(a) =>
                  evalWithEnvironment(e2, env) match {
                    case VInt(b) => if (Integer.parseInt(a) > Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for greater than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for greater than operation."))
              }
          }
      }
    }

    evalWithEnvironment(e, Nil)
  }

  def myeval_memo(e: Expr): Val = {
    val memo = collection.mutable.HashMap.empty[String, Val]
    def assignArgs(e: Expr, fargs: List[String], args: List[Expr]): Expr = {
      def assigntoList(l: List[Expr], farg: String, arg: Expr): List[Expr] = {
        def assignToListAux(res: List[Expr], list: List[Expr], farg: String, arg: Expr): List[Expr] =
          list match {
            case Nil => res
            case hd :: tl => assignToListAux(res ++ (assign(hd, farg, arg) :: Nil), tl, farg, arg)
          }

        assignToListAux(Nil, l, farg, arg)
      }

      def assign(e: Expr, farg: String, arg: Expr): Expr = {
        e match {
          case EConst(c) => e
          case EName(n) =>
            if (farg == n) arg
            else e

          case ECons(e1, e2) =>
            e1 match {
              case EName(farg) =>
                e2 match {
                  case EName(farg) =>
                    val ret = ECons(arg, arg)
                    ret
                  case EName(_) | EConst(_) =>
                    val ret = ECons(arg, e2)
                    ret
                  case _ =>
                    val ret = ECons(arg, assign(e2, farg, arg))
                    ret
                }
              case EName(_) | EConst(_) =>
                e2 match {
                  case EName(farg) =>
                    val ret = ECons(e1, e2)
                    ret
                  case EName(_) | EConst(_) =>
                    val ret = ECons(e1, e2)
                    ret
                  case _ =>
                    val ret = ECons(e1, assign(e2, farg, arg))
                    ret
                }
              case _ =>
                e2 match {
                  case EName(farg) =>
                    val ret = ECons(assign(e1, farg, arg), arg)
                    ret
                  case EName(_) | EConst(_) =>
                    val ret = ECons(assign(e1, farg, arg), e2)
                    ret
                  case _ =>
                    val ret = ECons(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret
                }
            }

          case EHd(el) =>
            el match {
              case EName(farg) =>
                val ret = EHd(arg)
                ret
              case EName(_) | EConst(_) =>
                val ret = EHd(el)
                ret
              case _ =>
                val ret = EHd(assign(el, farg, arg))
                ret
            }

          case ETl(el) =>
            el match {
              case EName(farg) =>
                val ret = ETl(arg)
                ret
              case EName(_) | EConst(_) =>
                val ret = ETl(el)
                ret
              case _ =>
                val ret = ETl(assign(el, farg, arg))
                ret
            }


          case EFun(p, eb) => EFun(p, assign(eb, farg, arg))

          case EApp(ef, eargs) =>
            eargs match {
              case Nil => e
              case _ => EApp(assign(ef, farg, arg), assigntoList(eargs, farg, arg))
            }

          case EPlus(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EPlus(arg, arg)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EPlus(arg, e2)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EPlus(arg, e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EPlus(arg, assign(e2, farg, arg))
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EPlus(e1, arg)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EPlus(e1, e2)
                        ret match {
                          case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EPlus(e1, e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EPlus(e1, assign(e2, farg, arg))
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EPlus(e1, arg)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EPlus(e1, e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EPlus(e1, e2)
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EPlus(e1, assign(e2, farg, arg))
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EPlus(assign(e1, farg, arg), arg)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EPlus(assign(e1, farg, arg), e2)
                      ret match {
                        case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EPlus(assign(e1, farg, arg), e2)
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EPlus(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret match {
                      case EPlus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                }
            }

          case EMinus(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMinus(arg, arg)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMinus(arg, e2)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMinus(arg, e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMinus(arg, assign(e2, farg, arg))
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMinus(e1, arg)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMinus(e1, e2)
                        ret match {
                          case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMinus(e1, e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMinus(e1, assign(e2, farg, arg))
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMinus(e1, arg)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMinus(e1, e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMinus(e1, e2)
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMinus(e1, assign(e2, farg, arg))
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                      case _ => ret
                    }
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMinus(assign(e1, farg, arg), arg)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMinus(assign(e1, farg, arg), e2)
                      ret match {
                        case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMinus(assign(e1, farg, arg), e2)
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a - b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMinus(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret match {
                      case EMinus(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                }
            }

          case EMult(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMult(arg, arg)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMult(arg, e2)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMult(arg, e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMult(arg, assign(e2, farg, arg))
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EMult(e1, arg)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                      else {
                        val ret = EMult(e1, e2)
                        ret match {
                          case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                          case _ => ret
                        }
                      }
                    case EConst(_) =>
                      val ret = EMult(e1, e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    case _ =>
                      val ret = EMult(e1, assign(e2, farg, arg))
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMult(e1, arg)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMult(e1, e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMult(e1, e2)
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMult(e1, assign(e2, farg, arg))
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                      case _ => ret
                    }
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EMult(assign(e1, farg, arg), arg)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                        case _ => ret
                      }
                    }
                    else {
                      val ret = EMult(assign(e1, farg, arg), e2)
                      ret match {
                        case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                        case _ => ret
                      }
                    }
                  case EConst(_) =>
                    val ret = EMult(assign(e1, farg, arg), e2)
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a + b))
                      case _ => ret
                    }
                  case _ =>
                    val ret = EMult(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret match {
                      case EMult(EConst(CInt(a)), EConst(CInt(b))) => EConst(CInt(a * b))
                      case _ => ret
                    }
                }
            }

          case EEq(e1, e2) =>
            e1 match {
              case EName(x) =>
                if (x == farg)
                  e2 match {
                    case EName(y) =>
                      if (y == farg) {
                        val ret = EEq(arg, arg)
                        ret
                      }
                      else {
                        val ret = EEq(arg, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EEq(arg, e2)
                      ret
                    case _ =>
                      val ret = EEq(arg, assign(e2, farg, arg))
                      ret
                  }
                else
                  e2 match {
                    case EName(y) =>
                      if (y == farg) {
                        val ret = EEq(e1, arg)
                        ret
                      }
                      else {
                        val ret = EEq(e1, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EEq(e1, e2)
                      ret
                    case _ =>
                      val ret = EEq(e1, assign(e2, farg, arg))
                      ret
                  }
              case EConst(_) =>
                e2 match {
                  case EName(y) =>
                    if (y == farg) {
                      val ret = EEq(e1, arg)
                      ret
                    }
                    else {
                      val ret = EEq(e1, e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = EEq(e1, e2)
                    ret
                  case _ =>
                    val ret = EEq(e1, assign(e2, farg, arg))
                    ret
                }
              case _ =>
                val ret = EEq(assign(e1, farg, arg), assign(e2, farg, arg))
                ret
            }

          case ELt(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = ELt(arg, arg)
                        ret
                      }
                      else {
                        val ret = ELt(arg, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = ELt(arg, e2)
                      ret
                    case _ =>
                      val ret = ELt(arg, assign(e2, farg, arg))
                      ret
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = ELt(e1, arg)
                        ret
                      }
                      else {
                        val ret = ELt(e1, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = ELt(e1, e2)
                      ret
                    case _ =>
                      val ret = ELt(e1, assign(e2, farg, arg))
                      ret
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = ELt(e1, arg)
                      ret
                    }
                    else {
                      val ret = ELt(e1, e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = ELt(e1, e2)
                    ret
                  case _ =>
                    val ret = ELt(e1, assign(e2, farg, arg))
                    ret
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = ELt(assign(e1, farg, arg), arg)
                      ret
                    }
                    else {
                      val ret = ELt(assign(e1, farg, arg), e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = ELt(assign(e1, farg, arg), e2)
                    ret
                  case _ =>
                    val ret = ELt(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret
                }
            }

          case EGt(e1, e2) =>
            e1 match {
              case EName(s1) =>
                if (s1 == farg)
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EGt(arg, arg)
                        ret
                      }
                      else {
                        val ret = EGt(arg, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EGt(arg, e2)
                      ret
                    case _ =>
                      val ret = EGt(arg, assign(e2, farg, arg))
                      ret
                  }
                else
                  e2 match {
                    case EName(s2) =>
                      if (s2 == farg) {
                        val ret = EGt(e1, arg)
                        ret
                      }
                      else {
                        val ret = EGt(e1, e2)
                        ret
                      }
                    case EConst(_) =>
                      val ret = EGt(e1, e2)
                      ret
                    case _ =>
                      val ret = EGt(e1, assign(e2, farg, arg))
                      ret
                  }

              case EConst(_) =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EGt(e1, arg)
                      ret
                    }
                    else {
                      val ret = EGt(e1, e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = EGt(e1, e2)
                    ret
                  case _ =>
                    val ret = EGt(e1, assign(e2, farg, arg))
                    ret
                }

              case _ =>
                e2 match {
                  case EName(s2) =>
                    if (s2 == farg) {
                      val ret = EGt(assign(e1, farg, arg), arg)
                      ret
                    }
                    else {
                      val ret = EGt(assign(e1, farg, arg), e2)
                      ret
                    }
                  case EConst(_) =>
                    val ret = EGt(assign(e1, farg, arg), e2)
                    ret
                  case _ =>
                    val ret = EGt(assign(e1, farg, arg), assign(e2, farg, arg))
                    ret
                }
            }

          case EIf(econd, et, ef) =>
            val cond = assign(econd, farg, arg)
            val newet = assign(et, farg, arg)
            val newef = assign(ef, farg, arg)
            EIf(cond, newet, newef)
        }
      }

      fargs match {
        case Nil => e
        case hd :: tl =>
          assignArgs(assign(e, hd, args.head), tl, args.tail)
      }
    }

    def searchEnvironment(name: String, env: List[Binded]): Option[Binded] = {
      def go(l: List[Binded]): Option[Binded] =
        l match {
          case Nil => None
          case ValBinded(n, v) :: tl =>
            if (name == n) Some(ValBinded(n, v))
            else go(tl)
          case DefBinded(n, e) :: tl =>
            if (name == n) Some(DefBinded(n, e))
            else go(tl)
        }

      go(env)
    }

    def evalBinds(b: List[Bind]): List[Binded] = {

      def evalBindsAux(res: List[Binded], bl: List[Bind]): List[Binded] =
        bl match {
          case Nil => res
          case (bind, name, exp) :: tl =>
            bind match {
              case BDEF() => evalBindsAux(new DefBinded(name, exp) :: res, tl)
              case BVAL() => evalBindsAux(new ValBinded(name, evalWithEnvironmentMemo(exp, res, memo)) :: res, tl)
            }
        }

      evalBindsAux(Nil, b)
    }

    def valToExpr(a: Val): Expr =
      a match {
        case VInt(a) => EConst(CInt(Integer.parseInt(a)))
        case VTrue() => EConst(CTrue())
        case VFalse() => EConst(CFalse())
        case VPair(a, b) => ECons(valToExpr(a), valToExpr(b))
        case VNil() => EConst(CNil())
      }

    def evalWithEnvironmentMemo(e: Expr, env: List[Binded], map: HashMap[String, Val]): Val = {

      env match {
        case Nil => {
          e match {
            case EConst(c) =>
              val str = ExprToString.const_toString(c)
              if (str == "true") VTrue()
              else if (str == "false") VFalse()
              else if (str == "nil") VNil()
              else VInt(str)

            case EName(x) => VException(new EvalException("Definition not found"))

            case EIf(e1, e2, e3) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VTrue() => evalWithEnvironmentMemo(e2, Nil, map)
                case VFalse() => evalWithEnvironmentMemo(e3, Nil, map)
                case _ => VException(new EvalException("Invalid expression for condition."))
              }

            case ECons(e1, e2) => VPair(evalWithEnvironmentMemo(e1, Nil, map), evalWithEnvironmentMemo(e2, Nil, map))

            case EHd(el) =>
              evalWithEnvironmentMemo(el, Nil, map) match {
                case VPair(a, b) => a
                case _ => VException(new EvalException("Invalid expression for head operation."))
              }

            case ETl(el) =>
              evalWithEnvironmentMemo(el, Nil, map) match {
                case VPair(a, b) => b
                case _ => VException(new EvalException("Invalid expression for tail operation."))
              }

            case EFun(p, eb) =>
              p match {
                case Nil => evalWithEnvironmentMemo(eb, Nil, map)
                case _ =>
                  def listEval(l: List[String]): List[Val] =
                    l match {
                      case Nil => Nil
                      case hd :: tl => VName(hd) :: listEval(tl)
                    }

                  VFun(p, eb)
              }

            case EApp(ef, eargs) =>
              ef match {
                case EFun(fargs, eb) =>
                  if (fargs.length != eargs.length) VException(new EvalException("Invalid number of arguments."))
                  else evalWithEnvironmentMemo(assignArgs(eb, fargs, eargs), Nil, map)
                case _ => VException(new EvalException("Invalid function call")) // EName()도 마찬가지.
              }
            case ELet(binds, eb) => evalWithEnvironmentMemo(eb, evalBinds(binds), map)

            case EPlus(e1, e2) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, Nil, map) match {
                    case VInt(b) => VInt((Integer.parseInt(a) + Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for addition."))
                  }
                case _ => VException(new EvalException("Invalid operands for addition."))
              }

            case EMinus(e1, e2) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, Nil, map) match {
                    case VInt(b) => VInt((Integer.parseInt(a) - Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
              }

            case EMult(e1, e2) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, Nil, map) match {
                    case VInt(b) => VInt((Integer.parseInt(a) * Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
              }

            case EEq(e1, e2) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, Nil, map) match {
                    case VInt(b) => if (a == b) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for EQ of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for EQ of integers."))
              }

            case ELt(e1, e2) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, Nil, map) match {
                    case VInt(b) => if (Integer.parseInt(a) < Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for less than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for less than operation."))
              }

            case EGt(e1, e2) =>
              evalWithEnvironmentMemo(e1, Nil, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, Nil, map) match {
                    case VInt(b) => if (Integer.parseInt(a) > Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for greater than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for greater than operation."))
              }
            case _ => VException(new EvalException("Invalid expression."))
          }
        }

        case _ =>
          e match {
            case EConst(c) =>
              val str = ExprToString.const_toString(c)
              if (str == "true") VTrue()
              else if (str == "false") VFalse()
              else if (str == "nil") VNil()
              else VInt(str)

            case EName(x) =>
              searchEnvironment(x, env) match {
                case None => VException(new EvalException("Definition not found."))
                case Some(ValBinded(x, v)) => v
                case Some(DefBinded(x, ex)) => evalWithEnvironmentMemo(ex, env, map) // check.
              }

            case EIf(e1, e2, e3) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VTrue() => evalWithEnvironmentMemo(e2, env, map)
                case VFalse() => evalWithEnvironmentMemo(e3, env, map)
              }

            case ECons(e1, e2) => VPair(evalWithEnvironmentMemo(e1, env, map), evalWithEnvironmentMemo(e2, env, map))

            case EHd(el) =>
              evalWithEnvironmentMemo(el, env, map) match {
                case VPair(a, b) => a
                case _ => VException(new EvalException("Invalid expression for head operation."))
              }

            case ETl(el) =>
              evalWithEnvironmentMemo(el, env, map) match {
                case VPair(a, b) => b
                case _ => VException(new EvalException("Invalid expression for tail operation."))
              }

            case EApp(ef, eargs) =>
              def getKeyFromArgs(eargs: List[Expr]): String = {
                def go(res: String, l: List[Expr]): String =
                  l match {
                    case Nil => res
                    case hd :: tl =>
                      go(res + "," + hd, tl)
                  }
                go("", eargs)
              }
              ef match {
                case EFun(fargs, eb) =>
                  if (fargs.length != eargs.length) VException(new EvalException("Invalid number of arguments."))
                  else {
                    val argsAssigned = assignArgs(eb, fargs, eargs)
                    val ret = evalWithEnvironmentMemo(argsAssigned, env, map)
                    ret
                  }

                case EName(x) =>
                  searchEnvironment(x, env) match {
                    case Some(binded) =>
                      binded match {
                        case DefBinded(n, expr) =>
                          expr match {
                            case EFun(params, eb) =>
                              val key = n + getKeyFromArgs(eargs)
                              map.get(key) match {
                                case Some(v) => v
                                case None =>
                                  //   val v = evalWithEnvironmentMemo(e, env, map)
                                  val v = evalWithEnvironmentMemo(EApp(expr, eargs), env, map)
                                  //   val mem = map + ((key, v))
                                  val mem = map += ((key, v))
                                  evalWithEnvironmentMemo(e, env, mem)
                              }
                            case _ => evalWithEnvironmentMemo(EApp(expr, eargs), env, map)
                          }

                        case ValBinded(n, vl) => evalWithEnvironmentMemo(EApp(valToExpr(vl), eargs), env, map)
                      }
                    case None => VException(new EvalException("Invalid in EApp"))
                  }
                case _ => evalWithEnvironmentMemo(ef, env, map)
              }

            case ELet(binds, eb) => {
              def joinBinds(inner: List[Binded], outer: List[Binded]): List[Binded] = {
                def singleJoin(bind: Binded, l: List[Binded]): List[Binded] = {
                  def go(prev: List[Binded], next: List[Binded]): List[Binded] =
                    next match {
                      case Nil => prev
                      case hd :: tl =>
                        hd match {
                          case ValBinded(n, v) =>
                            bind match {
                              case ValBinded(ni, vi) =>
                                if (ni == n) prev ++ (bind :: tl)
                                else go(prev ++ (hd :: Nil), tl)
                              case DefBinded(ni, vi) =>
                                if (ni == n) prev ++ (bind :: tl)
                                else go(prev ++ (hd :: Nil), tl)
                            }

                          case DefBinded(n, v) =>
                            bind match {
                              case ValBinded(ni, vi) =>
                                if (ni == n) prev ++ (bind :: tl)
                                else go(prev ++ (hd :: Nil), tl)
                            }
                        }
                    }

                  go(Nil, l)
                }

                inner match {
                  case Nil => outer
                  case hd :: tl => joinBinds(tl, singleJoin(hd, outer))
                }
              }

              val environment = joinBinds(evalBinds(binds), env)
              evalWithEnvironmentMemo(eb, environment, map)
            }

            case EPlus(e1, e2) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, env, map) match {
                    case VInt(b) => VInt((Integer.parseInt(a) + Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for addition of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for addition of integers."))
              }

            case EMinus(e1, e2) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, env, map) match {
                    case VInt(b) => VInt((Integer.parseInt(a) - Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for subtraction of integers."))
              }

            case EMult(e1, e2) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, env, map) match {
                    case VInt(b) => VInt((Integer.parseInt(a) * Integer.parseInt(b)).toString)
                    case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for multiplication of integers."))
              }

            case EEq(e1, e2) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, env, map) match {
                    case VInt(b) => if (a == b) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for EQ of integers."))
                  }
                case _ => VException(new EvalException("Invalid operands for EQ of integers."))
              }

            case ELt(e1, e2) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, env, map) match {
                    case VInt(b) => if (Integer.parseInt(a) < Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for less than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for less than operation."))
              }

            case EGt(e1, e2) =>
              evalWithEnvironmentMemo(e1, env, map) match {
                case VInt(a) =>
                  evalWithEnvironmentMemo(e2, env, map) match {
                    case VInt(b) => if (Integer.parseInt(a) > Integer.parseInt(b)) VTrue() else VFalse()
                    case _ => VException(new EvalException("Invalid operands for greater than operation."))
                  }
                case _ => VException(new EvalException("Invalid operands for greater than operation."))
              }
          }
      }
    }
    evalWithEnvironmentMemo(e, Nil, memo)
  }
}
