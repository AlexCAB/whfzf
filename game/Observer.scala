package game
import  componentwork._,
        componentswing.{IStatusBarF,IStatusBarS},
        scala.collection.mutable.{Map => MMap}  


class Observer (val name:MMap[String, Component]) extends Component {
  //Interface "IAvatarWatch"
  val IAvatarWatchEx:IAvatarWatchSR = new IAvatarWatchSR; var IAvatarWatchIm:IAvatarWatchF = null   
  interfaces += ("IAvatarWatch" -> (IAvatarWatchEx,"IAvatarWatchS","IAvatarWatchF", (c:Component, i:Interface) => {IAvatarWatchIm = i.asInstanceOf[IAvatarWatchF]}, (c:Component) => {IAvatarWatchIm = null; false}, false))
  //Interface "ITargetWatch"
  val ITargetWatchEx:ITargetWatchSR = new ITargetWatchSR; var ITargetWatchIm:ITargetWatchF = null   
  interfaces += ("ITargetWatch" -> (ITargetWatchEx,"ITargetWatchS","ITargetWatchF", (c:Component, i:Interface) => {ITargetWatchIm = i.asInstanceOf[ITargetWatchF]}, (c:Component) => {ITargetWatchIm = null; false}, false))
  //Interface "IStatusBar"
  val IStatusBarEx:IStatusBarSR = new IStatusBarSR; var IStatusBarIm:IStatusBarF = null   
  interfaces += ("IStatusBar" -> (IStatusBarEx,"IStatusBarS","IStatusBarF", (c:Component, i:Interface) => {IStatusBarIm = i.asInstanceOf[IStatusBarF]}, (c:Component) => {IStatusBarIm = null; false}, false))
  //Interfaces export realization
  class IAvatarWatchSR extends IAvatarWatchS {
    override def change(s:Int) = {
      if((s == 1)&&(state != 2)){shotNumber = 0; killNumber = 0}
      state = s
      if(IStatusBarIm != null){IStatusBarIm.setText(buildString())}} 
    override def shot() = {
      shotNumber += 1
      if(IStatusBarIm != null){IStatusBarIm.setText(buildString())}}
  }  
  class ITargetWatchSR extends ITargetWatchS {
    override def kill() = {
      killNumber += 1
      if(IStatusBarIm != null){IStatusBarIm.setText(buildString())}}
  }  
  class IStatusBarSR extends IStatusBarS {
    override def connection(c:Component) = {
      IStatusBarIm.setText(buildString()) 
    }    
  }
  //Internal function   
  private def buildString():String = {
    state match{
      case 0 => "Use arrows, 'q', 'e' to move and space to shoot. Press Enter and go!" 
      case 1 => {"Shots: " + shotNumber + ", kills: " + killNumber}
      case 2 => "paused"  
      case 3 => {"Game over with " + shotNumber + " shots and " + killNumber + " killed. Press Enter to start new game."}
    }
  }
  //Fields 
  private var state = 0 //0-start, 1-game, 2-pause, 3-game over
  private var shotNumber = 0
  private var killNumber = 0
  //Service code  
  construction()
} 