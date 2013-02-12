package game
import componentwork._,
       componentswing.{IViewSettingsF,IViewSettingsS},
       matrix.{Dot,Segment,Area},
       java.awt.Point,
       javax.imageio.ImageIO,  
       java.awt.{Image,Toolkit},
       scala.collection.mutable.{Map => MMap}

       
class Arena (val name:MMap[String, Component]) extends Component {
  //Multiinterface "IWall"
  val IWallEx:IWallSR = new IWallSR;  var IWallIm = MMap[Component, IWallF]()
  interfaces += ("IWall" -> (IWallEx,"IWallS","IWallF", (c:Component, i:Interface) => {IWallIm += (c -> i.asInstanceOf[IWallF])}, (c:Component) => {IWallIm -= c; false}, true))
  //Interface "IViewSettings" 
  val IViewSettingsEx:IViewSettingsSR = new IViewSettingsSR; var IViewSettingsIm:IViewSettingsF = null   
  interfaces += ("IViewSettings" -> (IViewSettingsEx,"IViewSettingsS","IViewSettingsF", (c:Component, i:Interface) => {IViewSettingsIm = i.asInstanceOf[IViewSettingsF]}, (c:Component) => {IViewSettingsIm = null; false}, false))
  //Interface "ITracking"
  val ITrackingEx:ITrackingFR = new ITrackingFR; var ITrackingIm:ITrackingS = null   
  interfaces += ("ITracking" -> (ITrackingEx,"ITrackingF","ITrackingS", (c:Component, i:Interface) => {ITrackingIm = i.asInstanceOf[ITrackingS]}, (c:Component) => {ITrackingIm = null; false}, false))
  //Interfaces export realization
  class IWallSR extends IWallS {
    override def chengeWidth(w:Double) = {
      area.w = w + 4  
      if(ITrackingIm != null){ITrackingIm.updateArenaArea(area.w, area.h)}
    } 
    override def chengeHeight(h:Double) = {
      area.h = h + 4 
      if(ITrackingIm != null){ITrackingIm.updateArenaArea(area.w, area.h)}
    }    
  }
  class IViewSettingsSR extends IViewSettingsS {
    override def resize(w:Int, h:Int) = {     
     IWallIm.foreach(c => {c._2.chengeSize(w,h)}) 
    }
    override def connection(c:Component) = {
      //Start wall creating
      doConstruction.synchronized{doConstruction.notify()}
      //Load and set background image
      val tk = Toolkit.getDefaultToolkit()  //Load image
      val url = getClass().getResource("Background.png")   
      val bi = ImageIO.read(url)
      IViewSettingsIm.setBackground(bi)   
    } 
  }
  class ITrackingFR extends ITrackingF {
    override def connection(c:Component) = {ITrackingIm.updateArenaArea(area.w, area.h)}  
  }
  //Construction/deconstruction
  override def construction() = {
    super.construction()
    doConstruction = new Thread(){
      override def run() = {
        //Wait for connect IViewDraw 
        while(IViewSettingsIm == null){  
          this.synchronized{wait()}} 
        //Create wall
        IWallEx.width = IViewSettingsIm.dimension.width; IWallEx.height = IViewSettingsIm.dimension.height 
        IWallEx.position = 1        
        topWall = new Wall(name, "IWall", thisComponent)
        IWallEx.position = 2
        rightWall = new Wall(name, "IWall", thisComponent)
        IWallEx.position = 3
        bottomWall = new Wall(name, "IWall", thisComponent)
        IWallEx.position = 4
        leftWall = new Wall(name, "IWall", thisComponent)
      }
    }
    doConstruction.start()  
  }
  //Fields 
  private var doConstruction:Thread = null
  private var topWall: Component = null
  private var rightWall:Component = null 
  private var bottomWall:Component = null
  private var leftWall:Component = null
  private val area = new Area(0,0)
  //Service code  
  construction()
} 