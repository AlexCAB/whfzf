package test
import componentwork._, 
       scala.collection.mutable.{Map => MMap},
       java.awt.{Point,Rectangle},
       java.awt.{Component => SC}

       
//ITestViewDraw
class ITestViewDrawF extends Interface {
  def refresh() = {}
} 
class ITestViewDrawS extends Interface {
  val objects = MMap[Component, Obj]() 
  var lastCollision:Coll = null
} 


//Types
class Obj(     
  var visible:Boolean,
  val centre:Point,  
  var perimeter:List[Point], 
  val describesRect:Rectangle,
  val lineSpeedVector:Point,
  var angularVelocity:Int,
  var mass:Int
) 
class Coll(
  val point:Point,
  val segmentBegin:Point,
  val segmentEnd:Point,
  val firstSpeed:Point,
  val secondSpeed:Point
)

