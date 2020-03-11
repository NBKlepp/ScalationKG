package knowledgeGraph
import scala.collection.immutable.Vector
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
    
abstract class Elem(id : String){
    val _id = id
    //val properties : Vector[Complex | Rational | Real | StrO | StrNum | TimeO]
    var properties = Vector[Option[Complex | Rational | Real | StrNum]]()
}