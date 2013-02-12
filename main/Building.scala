package main
import componentwork._,
       componentswing.{Frame,ViewDraw,StatusBar},
       matrix.{Space, Camera},
       game.{Arena,Joystick,TargetGen,Observer},
       test.{TestCamera, TestManager,TestObj1,TestObj2,TestViewDraw},
       scala.collection.mutable.{Map => MMap}     
        
       
//Create named components       
object Building extends Component {def main(args:Array[String]){}
  val name = MMap[String, Component]()
  
  
  /*Build program*/   
  //View
  name += ("frame" -> new Frame(name))
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
    
  
//  /*Build test game*/  
//  //View
//  name += ("frame" -> new Frame(name))
//  name += ("testViewDraw"  -> new TestViewDraw(name, "IWidgets",  name("frame")))
//  //Matrix
//  name += ("space"  -> new Space(name))
//  name += ("testCamera"  -> new TestCamera(name))
//  connect("ILayout", name("space"), name("testCamera"))
//  connect("ITestViewDraw", name("testViewDraw"), name("testCamera"))
//  //Game
//  name += ("arena" -> new Arena(name))
//  connect("IViewSettings", name("testViewDraw"), name("arena"))
//  name += ("joystick" -> new Joystick(name))
//  connect("IFrame",name("frame"),name("joystick"))
//  connect("IControl",name("space"),name("joystick"))
//  connect("IControl",name("testCamera"),name("joystick"))
//  name += ("targetGen" -> new TargetGen(name))
//  connect("ITargetGen",name("targetGen"),name("joystick"))
//  connect("ITracking",name("targetGen"),name("joystick"))
//  connect("ITracking",name("targetGen"),name("arena")) 
  
 
//  /*Build test matrix physics*/  
//  //Manager
//  name += ("testManager" -> new TestManager(name))
//  //View
//  name += ("frame" -> new Frame(name))
//  connect("IFrame",name("frame"),name("testManager"))
//  name += ("testViewDraw"  -> new TestViewDraw(name, "IWidgets",  name("frame")))
//  //Matrix
//  name += ("space"  -> new Space(name))
//  connect("IControl",name("space"),name("testManager"))
//  name += ("testCamera"  -> new TestCamera(name))
//  connect("IControl",name("testCamera"),name("testManager"))
//  connect("ITestViewDraw", name("testViewDraw"), name("testCamera"))
//  connect("ILayout", name("space"), name("testCamera"))
//  //Objects
//  name += ("obj1" -> new TestObj1(name))
//  name += ("obj2" -> new TestObj2(name))
  
  
  
//  /*Build test matrix texture*/  
//  //Manager
//  name += ("testManager" -> new TestManager(name))
//  //View
//  name += ("frame" -> new Frame(name))
//  connect("IFrame",name("frame"),name("testManager"))
//  name += ("viewDraw"  -> new ViewDraw(name, "IWidgets",  name("frame")))
//  connect("IViewSettings",name("viewDraw"),name("testManager"))
//  //Matrix
//  name += ("space"  -> new Space(name))
//  connect("IControl",name("space"),name("testManager"))
//  name += ("camera"  -> new Camera(name))
//  connect("IControl",name("camera"),name("testManager"))
//  connect("IViewDraw", name("viewDraw"), name("camera"))
//  connect("ILayout", name("space"), name("camera"))
//  //Objects
//  name += ("obj1" -> new TestObj1(name))
//  name += ("obj2" -> new TestObj2(name))  
  
  
}