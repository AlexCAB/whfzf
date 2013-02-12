package game
import componentwork._,
       matrix.{IControlF,IControlS,Area},
       javax.swing.Timer,
       java.awt.event.{ActionListener, ActionEvent},  
       scala.collection.mutable.{Map => MMap},
       scala.math.random


class TargetGen (val name:MMap[String, Component]) extends Component {
  //Multiinterface "ITarget"
  val ITargetEx:ITargetSR = new ITargetSR;  var ITargetIm = MMap[Component, ITargetF]()
  interfaces += ("ITarget" -> (ITargetEx,"ITargetS","ITargetF", (c:Component, i:Interface) => {ITargetIm += (c -> i.asInstanceOf[ITargetF])}, (c:Component) => {ITargetIm -= c; false}, true))
  //Interface "ITargetGen"
  val ITargetGenEx:ITargetGenFR = new ITargetGenFR; var ITargetGenIm:ITargetGenS = null   
  interfaces += ("ITargetGen" -> (ITargetGenEx,"ITargetGenF","ITargetGenS", (c:Component, i:Interface) => {ITargetGenIm = i.asInstanceOf[ITargetGenS]}, (c:Component) => {ITargetGenIm = null; false}, false))
  //Multiinterface "ITracking"
  val ITrackingEx:ITrackingSR = new ITrackingSR;  var ITrackingIm = MMap[Component, ITrackingF]()
  interfaces += ("ITracking" -> (ITrackingEx,"ITrackingS","ITrackingF", (c:Component, i:Interface) => {ITrackingIm += (c -> i.asInstanceOf[ITrackingF])}, (c:Component) => {ITrackingIm -= c; false}, true))
  //Interface "ITargetWatch"
  val ITargetWatchEx:ITargetWatchFR = new ITargetWatchFR; var ITargetWatchIm:ITargetWatchS = null   
  interfaces += ("ITargetWatch" -> (ITargetWatchEx,"ITargetWatchF","ITargetWatchS", (c:Component, i:Interface) => {ITargetWatchIm = i.asInstanceOf[ITargetWatchS]}, (c:Component) => {ITargetWatchIm = null; false}, false))
  //Interfaces export realization
  class ITargetSR extends ITargetS{
     override def iDie() = {if(ITargetWatchIm != null){ITargetWatchIm.kill()}}
     override def disconnection(component:Component) = {targetCount -= 1}
  } 
  class ITargetGenFR extends ITargetGenF {
    override def setMovement(f:Boolean) = {movement = f; if(f){targetLoop.synchronized{targetLoop.notify()}}}
  }
  class ITrackingSR extends ITrackingS {
    override def updateAvatarPosition(x:Double, y:Double, l:Boolean) = {
      ITargetIm.foreach((item:(Component, ITargetF)) => {item._2.avatarPosition.x = x; item._2.avatarPosition.y = y; item._2.avatarLive = l})
    }
    override def updateArenaArea(w:Double, h:Double) = {
      ITrackingIm.foreach((item:(Component, ITrackingF)) => {item._2.updateArenaArea(w:Double, h:Double)}) //Retranslation to all
      arenaArea.w = w; arenaArea.h = h
    }
  }
  class ITargetWatchFR extends ITargetWatchF  
  //Constructor/deconstructor
  override def construction() = {
    super.construction()
    targetLoop.start()
  }   
  override def deconstruction() = {
    super.deconstruction()
    work = false; targetLoop.synchronized{targetLoop.notify()}
  }   
  //Target generator loop     
  private val targetLoop:Thread = new Thread(){
    override def run() = {
      while(work){   
        this.synchronized{wait(((random * 200) + 2000).asInstanceOf[Int])} 
        if(work && movement &&(arenaArea.w >= 400)&&(arenaArea.h >= 200)&&(targetCount < 6)){  
          //Create new target
          ITargetEx.preferredPost.x = (((arenaArea.w - 200) * random) + 100).asInstanceOf[Int]
          ITargetEx.preferredPost.y = (((arenaArea.h - 200) * random) + 100).asInstanceOf[Int]
          ITargetEx.angle = (random * 628318).asInstanceOf[Int] //628318
          ITargetEx.mass = (random * 100) + 100
          ITargetEx.live = (random * 800) + 200 
          new Target(name, "ITarget", thisComponent)
          targetCount += 1}}
    }
  }
  //Fields
  private var work = true
  private var movement = false
  private val arenaArea = new Area(0,0)
  private var targetCount = 0 
  //Service code  
  construction()
} 