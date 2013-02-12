package test
import componentwork._,
       matrix.{IControlF,IControlS,Area},
       componentswing.{IFrameF,IFrameS,IViewSettingsF,IViewSettingsS},
       javax.imageio.ImageIO,  
       java.awt.{Image,Toolkit},
       scala.collection.mutable.{Map => MMap}


class TestManager (val name:MMap[String, Component]) extends  Component {
  //Interface "IFrame"
  val IFrameEx:IFrameSR = new IFrameSR; var IFrameIm:IFrameF = null   
  interfaces += ("IFrame" -> (IFrameEx,"IFrameS","IFrameF", (c:Component, i:Interface) => {IFrameIm = i.asInstanceOf[IFrameF]}, (c:Component) => {IFrameIm = null; false}, false))
  //Multiinterface "IControl"
  val IControlEx:IControlFR = new IControlFR;  var IControlIm = MMap[Component, IControlS]()
  interfaces += ("IControl" -> (IControlEx,"IControlF","IControlS", (c:Component, i:Interface) => {IControlIm += (c -> i.asInstanceOf[IControlS])}, (c:Component) => {IControlIm -= c; false}, true))
  //Interface "IViewSettings" 
  val IViewSettingsEx:IViewSettingsSR = new IViewSettingsSR; var IViewSettingsIm:IViewSettingsF = null   
  interfaces += ("IViewSettings" -> (IViewSettingsEx,"IViewSettingsS","IViewSettingsF", (c:Component, i:Interface) => {IViewSettingsIm = i.asInstanceOf[IViewSettingsF]}, (c:Component) => {IViewSettingsIm = null; false}, false))
  //Interfaces export realization
  class IFrameSR extends IFrameS {  
     override def connection(component:Component) = {
       IFrameIm.setTitle("WHFZT") 
       IFrameIm.show()
    }
    override def closing() = {
      name.foreach((item:(String, Component)) => {destroyComponent(item._2)}) //Destroy all components when main window closing  
    }    
  }
  class IControlFR extends IControlF {
    override def connection(c:Component) = {
      IControlIm(c).setMovement(true)  //Start matrix
      IControlIm(c).setArea(new Area(600,300))
    }
  }
  class IViewSettingsSR extends IViewSettingsS {
    override def connection(c:Component) = {
      val tk = Toolkit.getDefaultToolkit()  //Load image
      val url = getClass().getResource("Background.png")   
      val bi = ImageIO.read(url)
      IViewSettingsIm.setBackground(bi)   
    }
  }
  //Service code 
  construction() 
}


 
  
  
  
