package test
import componentwork._,
       matrix.{Dot,VirtualObject},
       java.awt.{Image,Toolkit},
       javax.imageio.ImageIO, 
       scala.collection.mutable.{Map => MMap}

       
class TestObj1 (val names:MMap[String, Component]) extends VirtualObject(names) {
  //Self-assembly
  ISpaceEx.preferredPost.x = 100; ISpaceEx.preferredPost.y = 200
  setPolygon(List[Dot](new Dot(0,35),new Dot(6,31),new Dot(9,26),new Dot(17,+23), new Dot(12,3), new Dot(23,0), new Dot(25,-3), new Dot(10,-21), new Dot(7,-21), new Dot(4,-16),
    new Dot(-4,-16), new Dot(-7,-21), new Dot(-10,-21), new Dot(-25,-3), new Dot(-23,0), new Dot(-12,3), new Dot(-17,+23), new Dot(-9,26), new Dot(-6,31)))
  setFriction(0.000000001)
  setLineSpeedVector(new Dot(300,0))
  setAngularVelocity(200)
  setMass(1000)
  private val tk = Toolkit.getDefaultToolkit()  
  private val url = getClass().getResource("Obj1.png")       
  private val tx = ImageIO.read(url)
  setTexture(tx,new Dot(24,21))
  //Service code  
  construction()  
} 