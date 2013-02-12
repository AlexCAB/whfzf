package componentswing
import componentwork._,
       javax.swing.{JLabel,BorderFactory},
       java.awt.{Component => SC, BorderLayout, Dimension, Color},
       scala.collection.mutable.{Map => MMap} 


class StatusBar (val name:MMap[String, Component], rootConName:String, rootConComponent:Component) extends JLabel with Component {
  //Interface "IWidget"(root)
  val IWidgetEx:IWidgetSR = new IWidgetSR; var IWidgetIm:IWidgetF = null   
  interfaces += ("IWidget" -> (IWidgetEx,"IWidgetS","IWidgetF", (c:Component, i:Interface) => {IWidgetIm = i.asInstanceOf[IWidgetF]}, (c:Component) => {IWidgetIm = null; true}, false))
   //Interface "IStatusBar" 
  val IStatusBarEx:IStatusBarFR = new IStatusBarFR; var IStatusBarIm:IStatusBarS = null   
  interfaces += ("IStatusBar" -> (IStatusBarEx,"IStatusBarF","IStatusBarS", (c:Component, i:Interface) => {IStatusBarIm = i.asInstanceOf[IStatusBarS]}, (c:Component) => {IStatusBarIm = null; false}, false))  
  //Interfaces export realization
  class IWidgetSR extends IWidgetS
  class IStatusBarFR extends IStatusBarF {
    override def setText(s:String) = {thisComponent.asInstanceOf[StatusBar].setText(s)}
  }
  //Self-assembly
  setPreferredSize(new Dimension(100,20))
  setBorder(BorderFactory.createLineBorder(Color.GRAY, 2))
  setText("")
  IWidgetEx.component = thisComponent.asInstanceOf[StatusBar]
  IWidgetEx.position  = BorderLayout.SOUTH
  //Service code  
  connectto("IWidget",rootConName,rootConComponent) //Connect root interface
  construction()
} 
