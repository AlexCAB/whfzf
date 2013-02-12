package game
import componentwork._,
       matrix.{VirtualObject,Dot,Features,Collision},
       javax.swing.Timer,
       java.awt.{Image,Color},
       java.awt.image.{BufferedImage},
       javax.imageio.ImageIO, 
       java.awt.event.{ActionListener, ActionEvent},  
       scala.collection.mutable.{Map => MMap},
       scala.math.{random,sqrt,pow,acos,Pi,cos,sin}

class Target (val names:MMap[String, Component], rootConName:String, rootConComponent:Component) extends VirtualObject(names) {
  //Root interface "ITarget" 
  val ITargetEx:ITargetFR = new ITargetFR; var ITargetIm:ITargetS = null   
  interfaces += ("ITarget" -> (ITargetEx,"ITargetF","ITargetS", (c:Component, i:Interface) => {ITargetIm = i.asInstanceOf[ITargetS]}, (c:Component) => {ITargetIm = null; true}, false)) 
  //Interfaces export realization
  class ITargetFR extends ITargetF {
    override def connection(c:Component) = {
      ISpaceEx.preferredPost.x = ITargetIm.preferredPost.x; ISpaceEx.preferredPost.y = ITargetIm.preferredPost.y
      setAngle(ITargetIm.angle)
      setMass(ITargetIm.mass)
      setAngularVelocity(((random * 150) + 20).asInstanceOf[Int] * (if(random > 0.5){1}else{-1}))
      live = ITargetIm.live; liveConut = ITargetIm.live
      drawLiveLevel()
    }
  }
  //Self-assembly
  objectType = 3 //Target
  setPolygon(List[Dot](new Dot(6,34),new Dot(-7,34),new Dot(-23,-4),new Dot(-11,-21), new Dot(11,-21), new Dot(24,-5)))
  setFriction(1)
  private val url = getClass().getResource("Target.png")       
  private val tx = ImageIO.read(url)
  setTexture(tx,new Dot(23,21))
  //Constructor/deconstructor
  override def construction() = {
    super.construction()    
    timer.start()
  }  
  override def deconstruction() = {
    super.deconstruction()    
    timer.stop()    
  }  
  //Behavior loop 
  private var counter = 0 
  private val timer:Timer = new Timer(500, new ActionListener(){ 
    def actionPerformed(e: ActionEvent) = {
      val lsv = getLineSpeedVector()
      val a = getAngle() / 100000
      if((lsv.x == 0)&&(lsv.y == 0)){
        if(ITargetEx.avatarLive){
          if(counter < 4){
            val cn = ISpaceEx.center
            val acn = new Dot((ITargetEx.avatarPosition.x - cn.x),(ITargetEx.avatarPosition.y - cn.y))
            val ra = sqrt(pow(acn.x,2) + pow((acn.y - 10),2)) 
            val rc = sqrt((acn.x * acn.x)+(acn.y * acn.y))
            val a = acos(if(rc == 0){0}else{(100 + (rc * rc)-(ra * ra))/(2 * 10 * rc)})
            val aa = if(acn.x < 0){a * 100000}else{((2 * Pi) - a) * 100000}
            val sa = getAngle()
            if((sa - aa).abs < 30000){
              setAngularVelocity(0)
              val s = ((random * 400) + 30).asInstanceOf[Int] //Line speed
              val a = sa / 100000
              setLineSpeedVector(new Dot(-(s * sin(a)), (s * cos(a))))
              counter = 0}
            else{
              counter += 1}}
          else{
            setAngularVelocity(((random * 300) + 40) * (if(random > 0.5){1}else{-1}))
            timer.setDelay(((random * 2800) + 200).asInstanceOf[Int])
            counter = 0}}
        else{
          if(counter < 3){
            setAngularVelocity(((random * 150) + 40) * (if(random > 0.5){1}else{-1}))
            timer.setDelay(((random * 5000) + 500).asInstanceOf[Int])
            counter += 1}
          else{
            setAngularVelocity(0)
            val s = (random * 200) + 20
            setLineSpeedVector(new Dot(-(s * sin(a)), (s * cos(a)))) 
            counter = 0}}}
      else{
        counter = 0}
    }})
  //Override methods 
  override def calcInteracting(clls:Collision,sftr:Features, oftr:Features):(List[Dot], Dot, Double, Double) = {  
    oftr.objectType match {
      case 1 => { //Avatar                              
        super.calcInteracting(clls, sftr, oftr)}
      case 2 => { //Bullet   
        val x = oftr.lineSpeedVector.x
        val y = oftr.lineSpeedVector.y
        val ls = sqrt((x * x)+(y * y))
        if(ls < liveConut){
          liveConut -= ls
          drawLiveLevel()
          super.calcInteracting(clls, sftr, oftr)} 
        else{
          if(ITargetIm != null){ITargetIm.iDie}
          selfdestruction  
          (null, new Dot(0,0), 0, 0)}}  
      case _ => {
        super.calcInteracting(clls, sftr, oftr)}} 
  }
  //Internal function   
  private def drawLiveLevel() = {
    val (tx,tcn) = getTexture()
    val bi = tx.asInstanceOf[BufferedImage]
    val g2d = bi.createGraphics()
    val l = (20 * (if(live == 0){0}else{liveConut/live})).asInstanceOf[Int]
    if(l > 0){g2d.setColor(fColor); g2d.fillRect((tcn.x.asInstanceOf[Int] - 2), (tcn.y.asInstanceOf[Int] - 10), 4, l)}
    if(l < 20){g2d.setColor(lColor); g2d.fillRect((tcn.x.asInstanceOf[Int] - 2), ((tcn.y.asInstanceOf[Int] - 10) + l), 4, 20 - l)}
    g2d.dispose()
    setTexture(tx,tcn)
  }
  //Fields 
  private var live:Double = 0
  private var liveConut:Double = 0
  private val fColor = new Color(48,98,214)
  private val lColor = new Color(139,10,32)
  //Service code  
  connectto("ITarget",rootConName,rootConComponent) //Connect root interface 
  construction()  
} 