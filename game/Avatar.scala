package game
import componentwork._,
       matrix.{VirtualObject,Dot,Features,Collision,Area},
       javax.swing.Timer,
       java.awt.{Image,Toolkit},
       javax.imageio.ImageIO, 
       java.awt.event.{ActionListener, ActionEvent},  
       scala.collection.mutable.{Map => MMap},
       scala.math.{cos,sin,sqrt,atan,asin,tan,acos,Pi,pow,random}

       
class Avatar (val names:MMap[String, Component], rootConName:String, rootConComponent:Component) extends VirtualObject(names) {
  //Root interface "IAvatar" 
  val IAvatarEx:IAvatarFR = new IAvatarFR; var IAvatarIm:IAvatarS = null   
  interfaces += ("IAvatar" -> (IAvatarEx,"IAvatarF","IAvatarS", (c:Component, i:Interface) => {IAvatarIm = i.asInstanceOf[IAvatarS]}, (c:Component) => {IAvatarIm = null; true}, false)) 
  //Multiinterface "IBullet"
  val IBulletEx:IBulletSR = new IBulletSR;  var IBulletIm = MMap[Component, IBulletF]()
  interfaces += ("IBullet" -> (IBulletEx,"IBulletS","IBulletF", (c:Component, i:Interface) => {IBulletIm += (c -> i.asInstanceOf[IBulletF])}, (c:Component) => {IBulletIm -= c; false}, true))
  //Root interface "ITarget" 
  val ITargetEx:ITargetFR = new ITargetFR; var ITargetIm:ITargetS = null   
  interfaces += ("ITarget" -> (ITargetEx,"ITargetF","ITargetS", (c:Component, i:Interface) => {ITargetIm = i.asInstanceOf[ITargetS]}, (c:Component) => {ITargetIm = null; false}, false)) 
  //Interfaces export realization
  class IAvatarFR extends IAvatarF {
    override def connection(c:Component) = {
      ISpaceEx.preferredPost.x = IAvatarIm.preferredPost.x; ISpaceEx.preferredPost.y = IAvatarIm.preferredPost.y
      setMass(IAvatarIm.preferredMass) 
      setAngle(IAvatarIm.preferredAngle) 
    }
    override def getPosition():(Double,Double) = {
      var x:Double = 0; var y:Double = 0
      if(ISpaceIm != null){
        val cn = ISpaceEx.center
        x = cn.x; y = cn.y}
      (x,y)
    }
    override def push(p:Int) = {
      val ads = 40; val lsv = getLineSpeedVector(); var av = getAngularVelocity()
      p match{
        case 11 /*left*/    => lsv.x += ads  
        case 12 /*right*/   => lsv.x -= ads
        case 13 /*up*/      => lsv.y -= ads
        case 14 /*down*/    => lsv.y += ads  
        case 23 /*e(up)*/   => av += ads
        case 24 /*q(down)*/ => av -= ads}   
      if(lsv.x.abs > 450){val s = if(lsv.x > 0){1}else{-1}; lsv.x = 450 * s}
      if(lsv.y.abs > 450){val s = if(lsv.y > 0){1}else{-1}; lsv.y = 450 * s}
      if(av.abs > 300){val s = if(av > 0){1}else{-1}; av = 300 * s}
      setLineSpeedVector(lsv); setAngularVelocity(av)     
    }      
    override def newShot(s:Int) = {
      bulletSpeed = s
      shotLoop.synchronized{shotLoop.notify()}
    }
    override def setArea(as:Area) = {area.w = as.w; area.h = as.h}
  }
  class IBulletSR extends IBulletS 
  class ITargetFR extends ITargetF 
  //Self-assembly
  setPolygon(List[Dot](new Dot(-5,45),new Dot(-13,45),new Dot(-24,7),new Dot(-16,-15),new Dot(20,-15),new Dot(24,-7)))
  setFriction(0.7)
  private val url = getClass().getResource("Avatar.png")       
  private val tx = ImageIO.read(url)
  setTexture(tx,new Dot(24,15))
  objectType = 1 //Avatar
  //Constructor/deconstructor
  override def construction() = {
    super.construction()
    shotLoop.start() 
    state = 1 //Live
  }  
  override def deconstruction() = {
    super.deconstruction()
    dieLoop.stop() 
    work = false; shotLoop.synchronized{shotLoop.notify()}
    state = 0   
  }  
  //Shot loop
  private val shotLoop:Thread = new Thread(){ 
    override def run() = {
      while(work){
        this.synchronized{wait()}  
        if(work){   
          val cn = ISpaceEx.center
          val a = getAngle(); val sina = sin(a / 100000); val cosa = cos(a / 100000)
          val blsv = new Dot(-(bulletSpeed * sina),(bulletSpeed * cosa)) //Calculate bullet speed vector
          val pp = new Dot((((-10 * cosa) - (57 * sina)) + cn.x),(((-10 * sina) + (57 * cosa)) + cn.y))
          if((pp.x > 0)&&(pp.y > 0)&&(pp.x < area.w)&&(pp.y < area.h)){
            //Create bullet
            IBulletEx.preferredPost.x = pp.x; IBulletEx.preferredPost.y = pp.y  //Calculate bullet position(-10,57)
            IBulletEx.preferredLineSpeed.x = blsv.x; IBulletEx.preferredLineSpeed.y = blsv.y 
            IBulletEx.preferredAngle = a
            new Bullet(name, "IBullet", thisComponent)}
          //Calculate recoil from the shot
          blsv.x *= -0.15; blsv.y *= -0.15
          val slsv = getLineSpeedVector()
          slsv.x += blsv.x;  slsv.y += blsv.y
          setLineSpeedVector(slsv)
          val sav = getAngularVelocity()
          setAngularVelocity(getAngularVelocity + (bulletSpeed * 0.1))
          //Count
          if(IAvatarIm != null){IAvatarIm.shot()}
          //Timeout(reload gun)
          Thread.sleep(50)}}
    }
  }   
  //Die loop
  private var counter = 0
  private val dieLoop:Timer = new Timer(300, new ActionListener(){
    def actionPerformed(e: ActionEvent) {
      state match {
        case 2 => { //die
          counter = if(counter >= 4){
            state = 3; dieLoop.setDelay(5000)           
            0}
          else{
            counter match{
              case 0 => {
                setPolygon(List[Dot](new Dot(11,50),new Dot(-10,50),new Dot(-21,20),new Dot(-21,-13),
                  new Dot(-8,-36),new Dot(8,-36),new Dot(23,-15),new Dot(23,16)))
                val url = getClass().getResource("AvatarD1.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(22,15))}
              case 1 => {
                val url = getClass().getResource("AvatarD2.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(22,13))}
              case 2 => {
                val url = getClass().getResource("AvatarD3.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(22,17))}
              case 3 => {
                val url = getClass().getResource("AvatarD4.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(21,38))}}
            dieLoop.setDelay(300)
            counter + 1}}
        case 3 => {
          state = 4; dieLoop.setDelay(3000)} 
        case 4 => { //revives
          counter = if(counter >= 4){
            state = 5; dieLoop.setDelay(50)           
            0}
          else{            
            counter match{
              case 0 => {
                val url = getClass().getResource("AvatarR1.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(21,38))}
              case 1 => {
                val url = getClass().getResource("AvatarR2.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(21,19))}
              case 2 => {
                val url = getClass().getResource("AvatarR3.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(23,21))}
              case 3 => {
                setPolygon(List[Dot](new Dot(15,19),new Dot(-15,18),new Dot(-21,-4),new Dot(-14,-11),new Dot(15,-11),new Dot(23,-2)))
                val url = getClass().getResource("AvatarR4.png"); val tx = ImageIO.read(url); setTexture(tx,new Dot(21,11))}}
            dieLoop.setDelay(600)
            counter + 1}}
        case 5 => { //zombie 
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
                dieLoop.setDelay(((random * 2800) + 200).asInstanceOf[Int])
                counter = 0}}
            else{
              if(counter < 3){
                setAngularVelocity(((random * 150) + 40) * (if(random > 0.5){1}else{-1}))
                dieLoop.setDelay(((random * 5000) + 500).asInstanceOf[Int])
                counter += 1}
              else{
                setAngularVelocity(0)
                val s = (random * 200) + 20
                setLineSpeedVector(new Dot(-(s * sin(a)), (s * cos(a)))) 
                counter = 0}}}
          else{
            counter = 0}}
        case _ => dieLoop.stop(); counter = 0} 
    }
  }) 
  //Override methods 
  override def calcInteracting(clls:Collision,sftr:Features, oftr:Features):(List[Dot], Dot, Double, Double) = {  
    if(state == 1){ //If live
      oftr.objectType match {
        case 2 => {                            
          state = 2
          if(IAvatarIm != null){IAvatarIm.iDie(1)}
          selfdestruction
          (null, new Dot(0,0), 0, 0)}
        case 3 => {  
          val ocp = new Dot((clls.point.x - clls.otherCenter.x), (clls.point.y - clls.otherCenter.y))
          val cosa = cos(oftr.angle / 100000); val sina = sin(oftr.angle / 100000) 
          val zc = new Dot(((ocp.x * cosa) + (ocp.y * sina)),(-(ocp.x * sina) + (ocp.y * cosa)))       
          if((zc.y > 0)&&((zc.x).abs <= 8)){ //If front collision 
            state = 2; objectType = 3 //Target
            connectto("ITarget",names("targetGen"))
            if(IAvatarIm != null){IAvatarIm.iDie(2)}
              dieLoop.start()
              (null,new Dot(0,0),0,sftr.mass)}
            else{
              super.calcInteracting(clls, sftr, oftr)}}
        case _ => {
          super.calcInteracting(clls, sftr, oftr)}}}
    else if(state == 5) {//If zombie
      if(oftr.objectType == 2){
        if(ITargetIm != null){ITargetIm.iDie}
        selfdestruction  
        (null, new Dot(0,0), 0, 0)}  
      else if(oftr.objectType == 1){
        (null, new Dot(0,0), 0, sftr.mass)}
      else{
        super.calcInteracting(clls, sftr, oftr)}
    (null,new Dot(0,0),0,sftr.mass)}
    else{      
    (null,new Dot(0,0),0,sftr.mass)}
  }
  //Fields
  private var state = 0 //1-live,2-die, 3-timeout, 4-revives, 4-zombie
  private var work = true
  private var bulletSpeed = 0
  private val area = new Area(0,0)
  //Service code  
  connectto("IAvatar",rootConName,rootConComponent) //Connect root interface 
  construction()  
} 