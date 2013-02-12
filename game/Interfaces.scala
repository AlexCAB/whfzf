package game
import componentwork._,
       matrix.{Dot,Segment,Area},
       java.awt.{Point,Dimension}


//ITarget
class ITargetF extends Interface {
  val avatarPosition = new Dot(0,0) 
  var avatarLive = false
} 
class ITargetS extends Interface {
  val preferredPost = new Dot(0,0)
  var angle:Double = 0
  var mass:Double = 0
  var live:Double = 0
  def iDie() = {}
} 
//ITarget
class IAvatarF extends Interface {
  def getPosition():(Double,Double) = {null}
  def push(k:Int) = {}
  def newShot(s:Int) = {}
  def setArea(as:Area) = {}
} 
class IAvatarS extends Interface {
  val preferredPost = new Dot(0,0)
  var preferredMass:Double = 0
  var preferredAngle:Double = 0
  def iDie(s:Int) = {}
  def shot() = {}
}
//IWall
class IWallF extends Interface {
  def chengeSize(w:Int, h:Int) = {} 
} 
class IWallS extends Interface {
  var position:Int = 0 // 1-top,2-right,3-bottom,4-left
  var width:Int = 0
  var height:Int = 0 
  def chengeWidth(w:Double) = {} 
  def chengeHeight(h:Double) = {} 
}
//ITracking
class ITrackingF extends Interface{
  def updateArenaArea(w:Double, h:Double) = {}
}
class ITrackingS extends Interface {
  def updateAvatarPosition(x:Double, y:Double, l:Boolean) = {}
  def updateArenaArea(w:Double, h:Double) = {}
} 
//ITarget
class IBulletF extends Interface {
  val avatarPosition = new Dot(0,0) 
} 
class IBulletS extends Interface {
  val preferredPost = new Dot(0,0)
  val preferredLineSpeed = new Dot(0,0)
  var preferredAngle:Double = 0
} 
//IAvatarWatch
class IAvatarWatchF extends Interface
class IAvatarWatchS extends Interface {
  def change(s:Int) = {}
  def shot() = {}
} 
//ITargetGen
class ITargetGenF extends Interface {
  def setMovement(f:Boolean) = {}
} 
class ITargetGenS extends Interface 
//ITargetWatch
class ITargetWatchF extends Interface 
class ITargetWatchS extends Interface {
  def kill() = {}  
}


//Exception type
class GameException(s:String) extends Exception(s) {}  
