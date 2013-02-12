package game
import componentwork._, componentswing.{IFrameF,IFrameS},
       matrix.{IControlF,IControlS,Area},
       javax.swing.Timer,
       java.awt.event.{ActionListener, ActionEvent},  
       scala.collection.mutable.{Map => MMap}


class Joystick (val name:MMap[String, Component]) extends  Component {
  //Interface "IFrame"
  val IFrameEx:IFrameSR = new IFrameSR; var IFrameIm:IFrameF = null   
  interfaces += ("IFrame" -> (IFrameEx,"IFrameS","IFrameF", (c:Component, i:Interface) => {IFrameIm = i.asInstanceOf[IFrameF]}, (c:Component) => {IFrameIm = null; false}, false))
  //Multiinterface "IControl"
  val IControlEx:IControlFR = new IControlFR;  var IControlIm = MMap[Component, IControlS]()
  interfaces += ("IControl" -> (IControlEx,"IControlF","IControlS", (c:Component, i:Interface) => {IControlIm += (c -> i.asInstanceOf[IControlS])}, (c:Component) => {IControlIm -= c; false}, true))
  //Multiinterface "IAvatar"
  val IAvatarEx:IAvatarSR = new IAvatarSR;  var IAvatarIm = MMap[Component, IAvatarF]()
  interfaces += ("IAvatar" -> (IAvatarEx,"IAvatarS","IAvatarF", (c:Component, i:Interface) => {IAvatarIm += (c -> i.asInstanceOf[IAvatarF])}, (c:Component) => {IAvatarIm -= c; false}, true))
  //Interface "ITracking"
  val ITrackingEx:ITrackingFR = new ITrackingFR; var ITrackingIm:ITrackingS = null   
  interfaces += ("ITracking" -> (ITrackingEx,"ITrackingF","ITrackingS", (c:Component, i:Interface) => {ITrackingIm = i.asInstanceOf[ITrackingS]}, (c:Component) => {ITrackingIm = null; false}, false))  
  //Interface "IAvatarWatch"
  val IAvatarWatchEx:IAvatarWatchFR = new IAvatarWatchFR; var IAvatarWatchIm:IAvatarWatchS = null   
  interfaces += ("IAvatarWatch" -> (IAvatarWatchEx,"IAvatarWatchF","IAvatarWatchS", (c:Component, i:Interface) => {IAvatarWatchIm = i.asInstanceOf[IAvatarWatchS]}, (c:Component) => {IAvatarWatchIm = null; false}, false))
  //Interface "ITargetGen" 
  val ITargetGenEx:ITargetGenSR = new ITargetGenSR; var ITargetGenIm:ITargetGenF = null   
  interfaces += ("ITargetGen" -> (ITargetGenEx,"ITargetGenS","ITargetGenF", (c:Component, i:Interface) => {ITargetGenIm = i.asInstanceOf[ITargetGenF]}, (c:Component) => {ITargetGenIm = null; false}, false))
  //Interfaces export realization
  class IFrameSR extends IFrameS {  
     override def connection(component:Component) = {
       IFrameIm.setTitle("WHFZT") 
       IFrameIm.show()
    }
    override def closing() = {
      name.foreach((item:(String, Component)) => {destroyComponent(item._2)}) //Destroy all components when main window closing  
    } 
    override def keyPressed(k:Int) = {
      if(k == 19){ //pause
        if(state == 1){
           IControlIm.foreach((item:(Component, IControlS)) => {item._2.setMovement(false)})
           if(ITargetGenIm != null){ITargetGenIm.setMovement(false)}
           state = 2
           if(IAvatarWatchIm != null){IAvatarWatchIm.change(state)}}
        else if(state == 2){
          IControlIm.foreach((item:(Component, IControlS)) => {item._2.setMovement(true)})
          if(ITargetGenIm != null){ITargetGenIm.setMovement(true)} 
          state = 1
          if(IAvatarWatchIm != null){IAvatarWatchIm.change(state)}}}
      if(state == 1){
        if(k == 32){ //space    
          if(shotCount == 0){shotCount = 400}else{if(shotCount < 800){shotCount += 10}}}}
    }
    override def keyReleased(k:Int) = {
      if(state == 1){
        if(k == 32){ //space
          if((avatatHandle != null)&&(shotCount != 0)){IAvatarIm(avatatHandle).newShot(shotCount); shotCount = 0}}
        else{
          val p = k match{case 39/*left*/=> 11; case 37/*right*/=> 12; case 38/*up*/=> 13; case 40/*down*/=> 14; case 69/*e(up)*/=> 23; case 81/*q(down)*/=> 24; case _ => 0} 
          if((p != 0)&&(avatatHandle != null)){IAvatarIm(avatatHandle).push(p)}}}
      if(k == 10){ //enter 
        if((state == 0)||(state == 3)){
          //Create avatar
          IAvatarEx.preferredPost.x = 350; IAvatarEx.preferredPost.y = 200
          IAvatarEx.preferredMass = 300
          IAvatarEx.preferredAngle = 314100     
          avatatHandle = new Avatar(name, "IAvatar", thisComponent)
          avatarLive = true; state = 1 
          if(IAvatarWatchIm != null){IAvatarWatchIm.change(state)}
          if(ITargetGenIm != null){ITargetGenIm.setMovement(true)}}}
    }     
  }
  class IControlFR extends IControlF {
    override def connection(c:Component) = {
      IControlIm(c).setMovement(true)  //Start matrix
      IControlIm(c).setArea(area) 
    }
  }
  class IAvatarSR extends IAvatarS {
    override def connection(c:Component) = {
      IAvatarIm(c).setArea(area)    
    }
    override def shot() = {
      if(IAvatarWatchIm != null){IAvatarWatchIm.shot()}    
    }  
    override def iDie(s:Int) = {  //1-Bullet,2-Zombie
      avatarDie()
    }  
    override def disconnection(c:Component) = {
      if(avatatHandle == c){avatarDie()}      
    }
  }
  class ITrackingFR extends ITrackingF {
    override def updateArenaArea(w:Double, h:Double) = {
      area.w = w; area.h = h 
      IControlIm.foreach((item:(Component,IControlS)) => {item._2.setArea(area)})  
      IAvatarIm.foreach((item:(Component,IAvatarF)) => {item._2.setArea(area)})  
    }
  } 
  class IAvatarWatchFR extends IAvatarWatchF {
    override def connection(c:Component) = {
      IAvatarWatchIm.change(state)
    }
  }
  class ITargetGenSR extends ITargetGenS 
  //Constructor/deconstructor
  override def construction() = {
    super.construction()
    getPost.start()
  }   
  override def deconstruction() = {
    super.deconstruction()
    work = false; getPost.synchronized{getPost.notify()}
  }   
  //Get position loop 
  private val getPost:Thread = new Thread(){
    override def run() = {
      while(work){   
        this.synchronized{wait(200)} 
        if(work){  
          try{
            if(ITrackingIm != null){
              if(avatarLive){
                if(avatatHandle != null){
                  val (x,y) = IAvatarIm(avatatHandle).getPosition
                  ITrackingIm.updateAvatarPosition(x,y,true)}}
              else{
                ITrackingIm.updateAvatarPosition(0,0,false)}}}
          catch{
            case ex:NoSuchElementException => {/*do nothing*/}}}}
    }
  }
  //Internal function  
  private def avatarDie() = {   
    avatatHandle = null; avatarLive = false; state = 3 
    if(ITargetGenIm != null){ITargetGenIm.setMovement(false)}
    if(IAvatarWatchIm != null){IAvatarWatchIm.change(state)}
  }    
  //Fields
  private var work = true
  private var state = 0 //0-start, 1-game, 2-pause, 3-game over
  private var avatarLive = false //Test(false)
  private var avatatHandle:Component = null
  private var angularVelocityCount:Int = 0
  private var angularVelocityRiseUp = false 
  private var angularVelocityRiseDown = false 
  private var shotCount:Int = 0
  private val area = new Area(0,0)
  //Service code 
  construction()
}




