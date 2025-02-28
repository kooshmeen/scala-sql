import scala.annotation.tailrec
import scala.language.implicitConversions

trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    r.get(colName).map(predicate)
  }
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    conditions.map(_.eval(r)).foldLeft(Some(true): Option[Boolean]) {
      case (Some(acc), Some(value)) => Some(op(acc, value))
      case _ => None
    }
  }
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    f.eval(r).map(!_)
  }
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = All(List(f1, f2))
def Or(f1: FilterCond, f2: FilterCond): FilterCond = Any(List(f1, f2))
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ == _, List(f1, f2))

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    @tailrec
    def evalRec(conds: List[FilterCond]): Option[Boolean] = conds match {
      case Nil => Some(false)
      case h :: t => {
        h.eval(r) match {
          case Some(true) => Some(true)
          case None => None
          case Some(false) => evalRec(t)
        }

      }
    }
    evalRec(fs)
  }
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    @tailrec
    def evalRec(conds: List[FilterCond]): Option[Boolean] = conds match {
      case Nil => Some(true)
      case h :: t => {
        h.eval(r) match {
          case Some(false) => Some(false)
          case None => None
          case Some(true) => evalRec(t)
        }
      }
    }
    evalRec(fs)
  }
}

implicit def tuple2Field(t: (String, String => Boolean)): Field = Field(t._1, t._2)

extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def !! = Not(f)
}