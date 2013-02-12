package main
import componentwork._,
   componentswing.{IFrameF,IFrameS,IWidgetF,IWidgetS,Frame,StatusBar,ViewDraw},
   matrix.{Space, Camera},
   game.{Arena,Joystick,TargetGen,Observer},
   java.awt.event.{KeyListener, KeyEvent},
   java.awt.{KeyboardFocusManager,KeyEventDispatcher,BorderLayout},
   javax.swing.{JApplet,JPanel},   
   scala.collection.mutable.{Map => MMap} 

   
class AppletBuilding extends JApplet with Component {
  val name = MMap[String, Component](("frame" -> this))
  //Build and destroy prigramm
  override def init(): Unit = {
    super.init()
    /*Build program*/   
    //View
    name += ("viewDraw" -> new ViewDraw(name, "IWidgets",  name("frame")))
    name += ("statusBar" -> new StatusBar(name, "IWidgets",  name("frame")))
    //Matrix
    name += ("space" -> new Space(name))
    name += ("camera" -> new Camera(name))
    connect("ILayout", name("space"), name("camera"))
    connect("IViewDraw", name("viewDraw"), name("camera"))
    //Game
    name += ("arena" -> new Arena(name))
    connect("IViewSettings", name("viewDraw"), name("arena"))
    name += ("joystick" -> new Joystick(name))
    connect("IFrame",name("frame"),name("joystick"))
    connect("IControl",name("space"),name("joystick"))
    connect("IControl",name("camera"),name("joystick"))
    name += ("targetGen" -> new TargetGen(name))
    connect("ITargetGen",name("targetGen"),name("joystick"))
    connect("ITracking",name("targetGen"),name("joystick"))
    connect("ITracking",name("targetGen"),name("arena")) 
    name += ("observer" -> new Observer(name))
    connect("IAvatarWatch",name("observer"),name("joystick"))
    connect("ITargetWatch",name("observer"),name("targetGen"))
    connect("IStatusBar",name("observer"),name("statusBar")) 
  }
  override def destroy(): Unit = {
    super.destroy()
    if(IFrameIm != null){IFrameIm.closing}   
  }
   //Interface "IFrame"
  val IFrameEx:IFrameFR = new IFrameFR; var IFrameIm:IFrameS = null   
  interfaces += ("IFrame" -> (IFrameEx,"IFrameF","IFrameS", (c:Component, i:Interface) => {IFrameIm = i.asInstanceOf[IFrameS]}, (c:Component) => {IFrameIm = null; false}, false))
  //Multiinterface "IWidgets"
  val IWidgetsEx:IWidgetFR = new IWidgetFR;  var IWidgetsIm = MMap[Component, IWidgetS]()
  interfaces += ("IWidgets" -> (IWidgetsEx,"IWidgetF","IWidgetS", (c:Component, i:Interface) => {IWidgetsIm += (c -> i.asInstanceOf[IWidgetS])}, (c:Component) => {IWidgetsIm -= c; false}, true))
  //Interfaces export realization
  class IFrameFR extends IFrameF 
  class IWidgetFR extends IWidgetF {  
    override def connection(c:Component) = {   
      panel.add(IWidgetsIm(c).component, IWidgetsIm(c).position)
      pack()
    }
    override def disconnection(c:Component) = {
      panel.remove(IWidgetsIm(c).component);
      pack() 
    }
  }
  //Self-assembly
  private val panel = new JPanel
  panel setLayout new BorderLayout
  add(panel)
  //Listeners 
  KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher{
    override def dispatchKeyEvent(e:KeyEvent):Boolean = {
      val k = e.getKeyCode()  
      e.getID() match {
        case KeyEvent.KEY_PRESSED => {if(IFrameIm != null){IFrameIm.keyPressed(e.getKeyCode())}}  
        case KeyEvent.KEY_RELEASED => {if(IFrameIm != null){IFrameIm.keyReleased(e.getKeyCode())}}
        case _ => }
      true
    }
  })  
}