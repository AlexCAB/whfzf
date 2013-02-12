/****************************************************************** Description *******************************************************************************
  Max line speed = 1000  
  Rotation speed it angular velocity
  To max radius 150 max angular velocity ~ 700 
  Angle value 0-628318 
  Max friction = 500
  Max mass = 10000
**************************************************************************************************************************************************************/

package matrix
import componentwork._,
       java.awt.Image,
       scala.collection.mutable.{Map => MMap}


//ILayout
class ILayoutF extends Interface {
  val centers = MMap[Component, Dot]() //id -> (center, been changed)
} 
class ILayoutS extends Interface 
//ICamera
class ICameraF extends Interface 
class ICameraS extends Interface {
  def getTexture():(Boolean,Image,Int) = {(false,null,0)}
} 
//ISpace
class ISpaceF extends Interface 
class ISpaceS extends Interface {
  val preferredPost = new Dot(0,0)
  var shape:List[Dot] = null
  var maxRadius:Double = 0
  val describesRect = new Rect(new Dot(0,0) ,new Dot(0,0))
  val center = new Dot(0,0)
  def tick():(Boolean,Double,Double) = (false,0,0) //perimeter change, x-move, y-move
} 
//IControl
class IControlF extends Interface 
class IControlS extends Interface {
  def setMovement(f:Boolean) = {}
  def setArea(as:Area) = {}
} 
//IInteracting
class IInteractingF extends Interface 
class IInteractingS extends Interface {
  def performInteracting(ftr:Features):(Collision, Features) = {(null,null)}
} 
//ITestCamera
class ITestCameraF extends Interface 
class ITestCameraS extends Interface {
  var shape:List[Dot] = null
  var perimeterChange = false
  val describesRect = new Rect(new Dot(0,0) ,new Dot(0,0))
  val lineSpeedVector = new Dot(0,0)
  var angularVelocity:Double = 0
  var mass:Double = 0
  var lastCollision:Collision = null
} 


//Types
class Rect(val top:Dot, val bottom:Dot) 
class Segment (val begin:Dot, val end:Dot) 
class Dot(var x:Double, var y:Double) 
class Area(var w:Double, var h:Double) 
class Features(
  var polygon:List[Dot],
  var shape:List[Dot],
  var describesRect:Rect,
  var lineSpeedVector:Dot,
  var angularVelocity:Double,
  var mass:Double,
  var maxRadius:Double, 
  var objectType:Int,
  val center:Dot,
  var angle:Double
) 
class Collision(
  val point:Dot,        //Collision point in self coordinates
  val segment:Segment,  //Collision segment(self shape to the right of collision segment) 
  val selfSpeed:Dot,    //Collision point speed about other
  val otherSpeed:Dot,   //Collision point speed about self 
  val otherCenter:Dot   //In self coordinates 
)


//Exception type
class MatrixException(s:String) extends Exception(s) {}  

  
 


