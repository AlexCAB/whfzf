package componentswing
import componentwork._,
       javax.swing.{JFrame, JPanel}, 
       java.awt.{BorderLayout, Component => SC,Color},
       java.awt.event.{WindowAdapter,WindowEvent, ComponentAdapter, ComponentEvent, KeyListener, KeyEvent},
       scala.collection.mutable.{Map => MMap}


class Frame(val name:MMap[String, Component]) extends JFrame with Component{
  //Interface "IFrame"
  val IFrameEx:IFrameFR = new IFrameFR; var IFrameIm:IFrameS = null   
  interfaces += ("IFrame" -> (IFrameEx,"IFrameF","IFrameS", (c:Component, i:Interface) => {IFrameIm = i.asInstanceOf[IFrameS]}, (c:Component) => {IFrameIm = null; false}, false))
  //Multiinterface "IWidgets"
  val IWidgetsEx:IWidgetFR = new IWidgetFR;  var IWidgetsIm = MMap[Component, IWidgetS]()
  interfaces += ("IWidgets" -> (IWidgetsEx,"IWidgetF","IWidgetS", (c:Component, i:Interface) => {IWidgetsIm += (c -> i.asInstanceOf[IWidgetS])}, (c:Component) => {IWidgetsIm -= c; false}, true))
  //Interfaces export realization
  class IFrameFR extends IFrameF {  
    override def setTitle(s:String) = {thisComponent.asInstanceOf[Frame].setTitle(s)}
    override def show () = {thisComponent.asInstanceOf[Frame].show()}
  }
  class IWidgetFR extends IWidgetF {  
    override def connection(c:Component) = {    
      panel.add(IWidgetsIm(c).component, IWidgetsIm(c).position)
      pack()
    }
    override def disconnection(c:Component) = {
      panel.remove(IWidgetsIm(c).component);
      pack() 
    }
    override def pack () = {thisComponent.asInstanceOf[Frame].pack()}
  }
  //Self-assembly
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  private val panel = new JPanel
  panel setLayout new BorderLayout
  add(panel)
  //Listeners   
  addWindowListener(new WindowAdapter{ 
    override def windowClosing(e:WindowEvent) = {if(IFrameIm != null){IFrameIm.closing}}
  }) 
  addKeyListener(new KeyListener{
    def keyPressed(e:KeyEvent) = {if(IFrameIm != null){IFrameIm.keyPressed(e.getKeyCode())}}  
    def keyReleased(e:KeyEvent) = {if(IFrameIm != null){IFrameIm.keyReleased(e.getKeyCode())}}  
    def keyTyped(e:KeyEvent) = {}
  }) 
  //Service code  
  construction()
} 
