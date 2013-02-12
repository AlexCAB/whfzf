package game
import componentwork._,
       matrix.{VirtualObject,Dot,Segment,Collision,Features},
       java.awt.{Point,Dimension,Color},
       java.awt.image.{BufferedImage},
       scala.collection.mutable.{Map => MMap},
       scala.math.{sqrt,pow}

       
class Wall (val names:MMap[String, Component], rootConName:String, rootConComponent:Component) extends VirtualObject(names) {
  //Root interface "IWall" 
  val IWallEx:IWallFR = new IWallFR; var IWallIm:IWallS = null   
  interfaces += ("IWall" -> (IWallEx,"IWallF","IWallS", (c:Component, i:Interface) => {IWallIm = i.asInstanceOf[IWallS]}, (c:Component) => {IWallIm = null; true}, false)) 
  //Interfaces export realization
  class IWallFR extends IWallF {
    override def connection(c:Component) = {
      //Set parameters 
      position = IWallIm.position
      val pg = buildPolygon(position, IWallIm.width, IWallIm.height)
      setPolygon(pg)
      val (x, y) = calcPost(position, IWallIm.width, IWallIm.height)
      ISpaceEx.preferredPost.x = x; ISpaceEx.preferredPost.y = y 
      center.x = x; center.y = y
      setMass(10000000) 
      val (tx,tcn) = buildTexture(pg); setTexture(tx,tcn)
      //Report of change area
      position match{
        case 2 => if(IWallIm != null){IWallIm.chengeWidth(x - 2)}
        case 3 => if(IWallIm != null){IWallIm.chengeHeight(y - 2)}  
        case _ =>}
    }
    override def chengeSize(w:Int, h:Int) = {
      if(ISpaceIm != null){
        val (x, y) = calcPost(position, w, h)
        center.x = x;  center.y = y
        val cn = ISpaceEx.center
        setLineSpeedVector(calcSpeed(position, new Dot(cn.x,cn.y) , center))}
    } 
  }
  //Self-assembly
  objectType = 4 //Wall
  //Override methods 
  override def calcMove() = {
    //Change parameters 
    val cn = ISpaceEx.center
    setLineSpeedVector(calcSpeed(position, new Dot(cn.x,cn.y), center)) 
    val pg = buildPolygon(position, (cn.x * 2), (cn.y * 2 )); setPolygon(pg)   
    val (tx,tcn) = buildTexture(pg); setTexture(tx,tcn)
    //Report of change area
    position match{
      case 2 => if(IWallIm != null){IWallIm.chengeWidth(cn.x - 2)}
      case 3 => if(IWallIm != null){IWallIm.chengeHeight(cn.y - 2)}  
      case _ =>}
  }
  override def calcInteracting(clls:Collision,sftr:Features, oftr:Features):(List[Dot], Dot, Double, Double) = {
    (null,sftr.lineSpeedVector,sftr.angularVelocity,sftr.mass)  
  }  
  //Internal function   
  private def buildPolygon(p:Int,wd:Double,hd:Double):List[Dot] = {
     if((p == 1) || (p ==3)){ //top or bottom
      val w = (wd - 14) / 2
      List[Dot](new Dot(-(w), -2), new Dot(w, -2), new Dot(w, 2), new Dot(-(w),2))}
    else{//right or left
      val h = (hd - 14) / 2
      List[Dot](new Dot(2, -(h)), new Dot(2, h), new Dot(-2, h), new Dot(-2,-(h)))}
  }
  private def calcPost(p:Int,w:Double,h:Double):(Double,Double) = {     
    p match {
      case 1 => ((w / 2),2)
      case 2 => ((w - 2),(h / 2)) 
      case 3 => ((w / 2),(h - 2))
      case 4 => (2,(h / 2))}
  }
  private def calcSpeed(p:Int, c:Dot, nc:Dot):Dot = {    
    val x = nc.x - c.x; val y = nc.y - c.y
    val v = sqrt((x * x) + (y * y)) 
    
    new Dot((if(v > 800){(800 / v) *  x}else{if(v < 1.1){0}else{if(v < 100){(100 / v) * x}else{x}}}),
      ((if(v > 800){(800 / v) *  y}else{if(v < 1.1){0}else{if(v < 100){(100 / v) * y}else{y}}})))  
  }  
  private def buildTexture(pg:List[Dot]):(BufferedImage,Dot) = {  
    val w = (pg(0).x).abs + pg(1).x
    val h = (pg(1).y).abs + pg(2).y 
    val tx = new BufferedImage(w.asInstanceOf[Int],h.asInstanceOf[Int],BufferedImage.TYPE_INT_ARGB)
    val g2d = tx.createGraphics()
    g2d.setColor(Color.red);
    g2d.fillRect(0,0, w.asInstanceOf[Int],h.asInstanceOf[Int]) 
    g2d.dispose()
    (tx, new Dot((w / 2),(h / 2)))
  }  
  //Fields
  private var position = 0 // 1-top,2-right,3-bottom,4-left
  private val center = new Dot(0,0)
  //Service code  
  connectto("IWall",rootConName,rootConComponent) //Connect root interface 
  construction()  
} 