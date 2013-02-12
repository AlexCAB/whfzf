package test
import componentwork._,
       matrix.{Dot,VirtualObject},
       javax.imageio.ImageIO,        
       java.awt.{Image,Toolkit},
       scala.collection.mutable.{Map => MMap}

       
class TestObj2 (val names:MMap[String, Component]) extends VirtualObject(names) {
  //Self-assembly
  ISpaceEx.preferredPost.x = 300; ISpaceEx.preferredPost.y = 200
  setPolygon(List[Dot](new Dot(-20,-30),new Dot(20,-30),new Dot(20,30),new Dot(-20,30)))
  setFriction(0.0000000001)
  setLineSpeedVector(new Dot(0,0))
  setAngularVelocity(00)
  setMass(2000)
  private val tk = Toolkit.getDefaultToolkit()  
  private val url = getClass().getResource("Obj2.png")       
  private val tx = ImageIO.read(url)
  setTexture(tx,new Dot(20,30))
  //Service code  
  construction()  
}