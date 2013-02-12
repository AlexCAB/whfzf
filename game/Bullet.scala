package game

import componentwork._,
       matrix.{VirtualObject,Dot,Features,Collision},
       javax.swing.Timer,
       java.awt.{Image,Toolkit},
       javax.imageio.ImageIO, 
       java.awt.event.{ActionListener, ActionEvent},  
       scala.collection.mutable.{Map => MMap},
       scala.math.{random,sqrt,pow,acos,Pi,cos,sin}

class Bullet (val names:MMap[String, Component], rootConName:String, rootConComponent:Component) extends VirtualObject(names) {
  //Root interface "IBullet" 
  val IBulletEx:IBulletFR = new IBulletFR; var IBulletIm:IBulletS = null   
  interfaces += ("IBullet" -> (IBulletEx,"IBulletF","IBulletS", (c:Component, i:Interface) => {IBulletIm = i.asInstanceOf[IBulletS]}, (c:Component) => {IBulletIm = null; true}, false)) 
  //Interfaces export realization
  class IBulletFR extends IBulletF {
    override def connection(c:Component) = {
      ISpaceEx.preferredPost.x = IBulletIm.preferredPost.x; ISpaceEx.preferredPost.y = IBulletIm.preferredPost.y
      setLineSpeedVector(IBulletIm.preferredLineSpeed)
      setAngle(IBulletIm.preferredAngle)
    }
  } 
  //Self-assembly
  setPolygon(List[Dot](new Dot(0,6),new Dot(-4,3),new Dot(-4,-5),new Dot(4,-5), new Dot(4,3)))
  setFriction(0.4)
  setMass(40)
  private val url = getClass().getResource("Bullet.png")       
  private val tx = ImageIO.read(url)
  setTexture(tx,new Dot(4,5))
  objectType = 2 //Bullet
  //Override methods 
  override def calcMove() = {
    val lsv = getLineSpeedVector()
    if((lsv.x == 0) && (lsv.y == 0)){selfdestruction}
  }
  override def calcInteracting(clls:Collision,sftr:Features, oftr:Features):(List[Dot], Dot, Double, Double) = {    
    if((oftr.objectType == 1) || (oftr.objectType == 3)){ //Collision with avatar or target 
      selfdestruction
      (null, new Dot(0,0), 0, 0)  
    }
    else{
      super.calcInteracting(clls, sftr, oftr)
    }
  }
  //Service code  
  connectto("IBullet",rootConName,rootConComponent) //Connect root interface 
  construction()  
} 