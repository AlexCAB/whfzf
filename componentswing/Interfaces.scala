package componentswing
import componentwork._,
       java.awt.Image,
       scala.collection.mutable.{Map => MMap},
       java.awt.{Point,Dimension,Image},
       java.awt.{Component => SC}

       
//IFrame
class IFrameF extends Interface {
  def setTitle(s:String) = {}
  def show () = {}
} 
class IFrameS extends Interface {
   def closing () = {}
   def keyPressed (k:Int) = {}
   def keyReleased (k:Int) = {}
} 
//IWidget
class IWidgetF extends Interface {
  def pack () = {}  
} 
class IWidgetS extends Interface {
  var component:SC = null
  var position:String = null  
} 
//IStatusBar
class IStatusBarF extends Interface {
   def setText(s:String) = {}
} 
class IStatusBarS extends Interface 
//IViewSettings
class IViewSettingsF extends Interface {
  def setBackground(img:Image) = {}
  val dimension = new Dimension(0,0)
} 
class IViewSettingsS extends Interface {
  def resize(w:Int, h:Int) = {}
} 
//IViewDraw
class IViewDrawF extends Interface {
  def refresh() = {}
} 
class IViewDrawS extends Interface {
  val objects = MMap[Component, ObjTexture]() 
} 


//Types
class ObjTexture(var texture:Image, val coordinates:Point, val center:Point)
















 
