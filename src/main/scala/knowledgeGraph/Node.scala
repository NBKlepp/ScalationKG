package knowledgeGraph

import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector
import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
    
class Node(id : String, nt : NodeType) extends Elem(id) {

    properties = Vector.fill(nt.numProperties())(None)

    def getProperties() = properties

    val sid = nt.addNode(this)
    
    def getId() = id

    def getSid() = sid
    
    def nodeType() = nt.getName()
    
    //def this(id : String,nt : NodeType, _properties : Vector[Complex | Rational | Real | StrO | StrNum | TimeO]) =
    def this(id : String,nt : NodeType, _properties : Vector[Complex | Rational | Real | StrNum ]) =
    {
        this(id,nt)
        for (p <- 0 until _properties.length){
            val t1 = _properties(p).getClass().toString
            val t2 = nt.getPropertyDomain(p) 
            if ( t1 == t2 ) properties = properties :+ Some(_properties(p))
            else {
                println(s"Warning : incompatible property and domain:")
                println(s"\t${t1},${t2}.")
                println(s"Node ${id} not added...")
                throw new Exception()
            }
        } // for
    } // this

    def updateProperty(pName : String, property : Complex | Rational | Real | StrNum ) =
    {
        nt.updateNode(sid,pName,property) match
        {
            case Some(i)    =>  println(s"Node not updated: \n\t${i}")                                
            case None       =>  println(s"Node ${sid} property ${pName} updated to ${property}")
        }
    }
}

object NodeTester extends App{
    val roadProperties = HashMap[String,String](
                            "name"->"String",
                            "type"->"String",
                            "district"->"int")

    val sensorProperties = HashMap[String,String](
                                "id"->"int",
                                "district"->"int")

    val road       = new NodeType("road",roadProperties)
    val sensor     = new NodeType("sensor",sensorProperties)

    val r1 = new Node("r1",road)
    val r2 = new Node("r2",road)
    val s1 = new Node("s1",sensor)
    val s2 = new Node("s2",sensor)

}