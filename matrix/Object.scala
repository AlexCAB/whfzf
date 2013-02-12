package matrix
import componentwork._,
       java.awt.Point,
       java.awt.{Image,Dimension,Graphics2D,Transparency,AlphaComposite},
       java.awt.image.{BufferedImage,RenderedImage},
       scala.collection.mutable.{Map => MMap},
       scala.math.{cos,sin,sqrt,atan,asin,tan,acos,Pi,pow}
 
       
class VirtualObject (val name:MMap[String, Component]) extends Component {
  //Interface "ISpace" 
  val ISpaceEx:ISpaceSR = new ISpaceSR; var ISpaceIm:ISpaceF = null   
  interfaces += ("ISpace" -> (ISpaceEx,"ISpaceS","ISpaceF", (c:Component, i:Interface) => {ISpaceIm = i.asInstanceOf[ISpaceF]}, (c:Component) => {ISpaceIm = null; false}, false))
  //Interface "ICamera" 
  val ICameraEx:ICameraSR = new ICameraSR; var ICameraIm:ICameraF = null   
  interfaces += ("ICamera" -> (ICameraEx,"ICameraS","ICameraF", (c:Component, i:Interface) => {ICameraIm = i.asInstanceOf[ICameraF]}, (c:Component) => {ICameraIm = null; false}, false)) 
  //Interface "ITestCamera" 
  val ITestCameraEx:ITestCameraSR = new ITestCameraSR; var ITestCameraIm:ITestCameraF = null   
  interfaces += ("ITestCamera" -> (ITestCameraEx,"ITestCameraS","ITestCameraF", (c:Component, i:Interface) => {ITestCameraIm = i.asInstanceOf[ITestCameraF]}, (c:Component) => {ITestCameraIm = null; false}, false))  
  //Interface "IInteracting" 
  val IInteractingEx:IInteractingFR = new IInteractingFR; var IInteractingIm:IInteractingS = null   
  interfaces += ("IInteracting" -> (IInteractingEx,"IInteractingF","IInteractingS", (c:Component, i:Interface) => {IInteractingIm = i.asInstanceOf[IInteractingS]}, (c:Component) => {IInteractingIm = null; false}, false))
  //Interface "ISynergy" 
  val ISynergyEx:IInteractingSR = new IInteractingSR; var ISynergyIm:IInteractingF = null   
  interfaces += ("ISynergy" -> (ISynergyEx,"IInteractingS","IInteractingF", (c:Component, i:Interface) => {ISynergyIm = i.asInstanceOf[IInteractingF]}, (c:Component) => {ISynergyIm = null; false}, false))
  //Interfaces export realization
  class ISpaceSR extends ISpaceS {
    override def tick():(Boolean, Double, Double) = {
      var mx,my:Double = 0.0; var pcf = false; var mf = false
      this.synchronized{
        //Calc movement
        if(work){  
          if(lineSpeed != 0){ //Calc line speed
            if((moveCounter <= 1) || (lineSpeed > 1000)){
              if(moveCounter != 0){
                mx = stepLineSpeed.x; my = stepLineSpeed.y; mf = true
                if(slowdown != 0){ //If slowdown - calculation new line speed
                  val sd = slowdown + ((slowdown - ((sqrt(lineSpeed * 1000) / 1000) * slowdown)) * 2)
                  val nls = if((sd + 10) >= lineSpeed){0}else{lineSpeed - sd}
                  val k = if(lineSpeed == 0){0}else{nls / lineSpeed}; lineSpeedVector.x = lineSpeedVector.x * k; lineSpeedVector.y = lineSpeedVector.y * k
                  lineSpeedChange()}}
              moveCounter = if(lineSpeed == 0){0}else{1000 / lineSpeed}}
            else{
              moveCounter -= 1}}
          else{
            moveCounter = 0}
          if(angularVelocity != 0 ){ //Calc rotation
            if((angularVelocityCounter <= 1)  || (angularVelocity.abs > maxAngularVelocity)){
              if(angularVelocityCounter != 0){   
                if(angularVelocity < 0){
                  angle -= maxAngularVelocity
                  if(angle < 0){angle = 628318}}
                else{
                  angle += maxAngularVelocity
                  if(angle > 628318){angle = 0}}
                rotatePoints()
                ITestCameraEx.perimeterChange = true; pcf = true; mf = true
                if(slowdown != 0){ //If slowdown - calculation new angular velocity                
                  val aav = angularVelocity.abs
                  val sd = slowdown + ((slowdown - ((sqrt(aav * 1000) / 1000) * slowdown)) * 4)
                  val nav = if((sd + 10) >= aav){0}else{aav - sd}
                  angularVelocity = if(angularVelocity > 0){nav}else{nav * -1} 
                  ITestCameraEx.angularVelocity = angularVelocity}}
              angularVelocityCounter = if(angularVelocity == 0){0}else{maxAngularVelocity / angularVelocity.abs}}
            else{
              angularVelocityCounter -= 1}}
          else{
            angularVelocityCounter = 0}}//}
        //Call override calcMove
        if(mf){calcMove()}}
      return (pcf, mx, my)
    } 
  }
  class ICameraSR extends ICameraS {
    override def getTexture():(Boolean,Image,Int) = {
      if(turnedTexture != null){
        val f = if(textureAngle != angle){
          rotateTexture(angle); textureAngle = angle
          true}
        else{
          false}
       val rf = if(textureSet == true){textureSet = false; true}else{f}        
       (rf,turnedTexture,turnedTextureSize)}
      else{
        (false, null,0)}
    } 
  }          
  class ITestCameraSR extends ITestCameraS
  class IInteractingFR extends IInteractingF{ //IInteracting
    override def connection(c:Component) = { 
      interacting +=1
      doInteracting.synchronized{doInteracting.notify()}
    } 
  }
  class IInteractingSR extends IInteractingS { //ISynergy
    override def performInteracting(oftr:Features):(Collision, Features) = {
      var clls:Collision = null; var rftr:Features = null
      this.synchronized{//Get res
        //Clone current state   
        val dr = new Rect(new Dot(ISpaceEx.describesRect.top.x,ISpaceEx.describesRect.top.y),new Dot(ITestCameraEx.describesRect.bottom.y, ITestCameraEx.describesRect.bottom.y)) 
        val cn = ISpaceEx.center
        rftr = new Features(null,shape.filter(p => true), dr, new Dot(lineSpeedVector.x, lineSpeedVector.y), angularVelocity, mass, ISpaceEx.maxRadius, objectType, new Dot(cn.x,cn.y),angle)
        //Correct oftr to self coordinates 
        val ocn = new Dot((oftr.center.x - cn.x),(oftr.center.y - cn.y))
        var csp = List[Dot](); oftr.shape.foreach((p:Dot) => {csp :+= new Dot((p.x + ocn.x),(p.y + ocn.y))}); oftr.shape = csp  
        oftr.describesRect.top.x += ocn.x; oftr.describesRect.top.y += ocn.y; oftr.describesRect.bottom.x += ocn.x; oftr.describesRect.bottom.y += ocn.y
        //Calc collision
        clls = calcCollision(rftr,oftr,ocn)
        //Calc interaction
        if(clls != null){
          if(ITestCameraIm != null){ //It test interface connected copy collision data
            ITestCameraEx.lastCollision = new Collision(new Dot(clls.point.x, clls.point.y),
              new Segment(new Dot(clls.segment.begin.x, clls.segment.begin.y), new Dot(clls.segment.end.x, clls.segment.end.y)),
              new Dot(clls.selfSpeed.x,clls.selfSpeed.y),
              new Dot(clls.otherSpeed.x,clls.otherSpeed.y),
              new Dot(clls.otherCenter.x,clls.otherCenter.y))} 
          val (np,nlsv,nav,nm) = calcInteracting(clls, new Features(polygon, shape, ISpaceEx.describesRect, lineSpeedVector, angularVelocity, mass, ISpaceEx.maxRadius, objectType, cn, angle), oftr)//Return: new self polygon, line speed vector, self angular velocity
          //Update state    
          if(work){ //If this component not be destroy
            if(np != null){setPolygon(np)}
            setLineSpeedVector(nlsv)
            setAngularVelocity(nav)
            setMass(nm)}}}
      (clls, rftr)
    }
  }
  //Constructor/deconstructor
  override def construction() = {
    super.construction()
    connectto("ISpace",name("space")) 
    if(name.contains("camera")){connectto("ICamera",name("camera"))} 
    if(name.contains("testCamera")){connectto("ITestCamera",name("testCamera"))} 
    doInteracting.start()
  }   
  override def deconstruction() = {
    super.deconstruction()
    work = false; doInteracting.synchronized{doInteracting.notify()}
  }   
  //Set-get function
  protected def setPolygon(pr:List[Dot]) = {
    //Set
    polygon = pr
    rotatePoints() 
    ITestCameraEx.perimeterChange = true 
    //Calc max radius
    ISpaceEx.maxRadius = 0
    polygon.foreach((p:Dot) => {
      val r = sqrt((p.x * p.x)+(p.y * p.y))
      if(r > ISpaceEx.maxRadius){ISpaceEx.maxRadius = r}})
    //Calc max angular velocity  
    maxAngularVelocity = atan(if(ISpaceEx.maxRadius == 0){0}else{1.0 / ISpaceEx.maxRadius}) * 100000  
  }
  protected def setLineSpeedVector(lsv:Dot) = {
    val ls = sqrt((lsv.x * lsv.x)+(lsv.y * lsv.y))
    val clsv = if(ls > 1000){
      val k = if(ls == 0){0}else{1000 / ls}; new Dot((lsv.x * k),(lsv.y * k))} 
    else{
      lsv}
    if((clsv.x != lineSpeedVector.x) || (clsv.y != lineSpeedVector.y)){
      this.synchronized{
        lineSpeedVector.x = clsv.x; lineSpeedVector.y = clsv.y
        lineSpeedChange()}}
  }
  protected def getLineSpeedVector():Dot = new Dot(lineSpeedVector.x, lineSpeedVector.y)
  protected def setAngle(a:Double) = {
    //Check and correct
    angle = a
    if(a < 0){angle = 0}
    if(a > 628318){angle = 628318}
    //Apply
    rotatePoints() 
    ITestCameraEx.perimeterChange = true 
  }
  protected def getAngle():Double = angle
  protected def setAngularVelocity(av:Double) = {
    this.synchronized{
      angularVelocity = if(av > maxAngularVelocity){maxAngularVelocity}else{av} 
      ITestCameraEx.angularVelocity = angularVelocity}    
  } 
  protected def getAngularVelocity():Double = angularVelocity
  protected def setMass(m:Double) = {
    mass = if(m > 10000){10000}else{m}
    ITestCameraEx.mass = mass 
    slowdown = friction - (friction * ( mass / 15000))  
  }
  protected def getMass():Double = mass 
  protected def setFriction(f:Double) = {
    friction = if(f > 500){500}else{f}
    slowdown = friction - (friction * ( mass / 15000))   
  }
  protected def getFriction():Double = friction 
  protected def setTexture(tx:Image,cn:Dot) = {
    texture = tx; textureCenter.x = cn.x; textureCenter.y = cn.y; textureAngle = 0
    if(tx != null){
      val dm = new Dimension(tx.getWidth(null),tx.getHeight(null))
      var d:Double = 0
      for(i <- 1 to 4){
        val cd = i match{
          case 1 => sqrt(pow(cn.x,2) + pow(cn.y,2)) 
          case 2 => sqrt(pow((cn.x - dm.width),2) + pow(cn.y,2))  
          case 3 => sqrt(pow((cn.x - dm.width),2) + pow((cn.y - dm.height),2)) 
          case 4 => sqrt(pow(cn.x,2) + pow((cn.y - dm.height),2))}  
        d = if(cd > d){cd}else{d}}  
      turnedTextureSize = (d.asInstanceOf[Int]) * 2
      val pg2d = (tx.getGraphics).asInstanceOf[Graphics2D]      
      val dc = pg2d.getDeviceConfiguration();
      turnedTexture = dc.createCompatibleImage(turnedTextureSize, turnedTextureSize, Transparency.TRANSLUCENT)   
      rotateTexture(angle); textureAngle = angle}
    else{
      turnedTexture = null}
    textureSet = true
  }
  protected def getTexture():(Image,Dot) = {(texture,textureCenter)}  
  //Overridden methods 
  protected def calcMove() = {}
  protected def calcInteracting(clls:Collision,sftr:Features, oftr:Features):(List[Dot], Dot, Double, Double) = {
    //Calculation new self absolute speed vector
    val sas = clls.selfSpeed; val oas = clls.otherSpeed  
    val sm = sftr.mass; val om = oftr.mass; val smom = sm + om
    val nsasx = if(smom == 0){0}else{((2 * om * oas.x)+(sm - om) * sas.x) / smom}
    val nsasy = if(smom == 0){0}else{((2 * om * oas.y)+(sm - om) * sas.y) / smom} 
    val nsas = new Dot(nsasx,nsasy) 
    //Reflect  a. speed from collision segment 
    val cs = clls.segment
    val rsa = sqrt(pow((cs.end.x - (nsas.x + cs.begin.x)),2) + pow((cs.end.y - (nsas.y + cs.begin.y)),2)) 
    val rsb = sqrt(pow((cs.begin.x - cs.end.x),2) + pow((cs.begin.y - cs.end.y),2)) 
    val rsc = sqrt(pow(nsas.x,2) + pow(nsas.y,2)) 
    val ps = 2 * rsb * rsc
    val ra = acos(if(ps == 0){0}else{((rsb * rsb) + (rsc * rsc)-(rsa * rsa))/ps}) 
    val ar = if(ra < (Pi / 2)){Pi - (ra * 2)}else{Pi + ((Pi - ra) * 2)}
    val cosar = cos(ar); val sinar = sin(ar)   
    val nrsas = new Dot(((nsas.x * cosar)-(nsas.y * sinar)),((nsas.x * sinar)+(nsas.y * cosar))) 
    //Calculation new line speed and angular velocity
    val bvp = clls.point; val evp = new Dot((nrsas.x + bvp.x), (nrsas.y + bvp.y))
    val ld = sqrt(pow((evp.x - bvp.x),2)+ pow((evp.y - bvp.y),2))
    val vd = if(ld == 0){0}else{((bvp.x * evp.y) - (evp.x * bvp.y)) / ld} 
    val k =  if(sftr.maxRadius == 0){0}else{vd / sftr.maxRadius}
    val ck = if(k > 0.9){0.9}else{if(k.abs < 0.000001){0}else{k}}
    val vl = sqrt(pow(nrsas.x,2) + pow(nrsas.y,2))  
    val nav = vl * ck
    val nlsv = new Dot((nrsas.x - (nrsas.x * ck.abs)),(nrsas.y - (nrsas.y * ck.abs)))
    val cnav = if(nav == nav){nav}else{0}
    if(nlsv.x != nlsv.x){nlsv.x = 0}; if(nlsv.y != nlsv.y){nlsv.y = 0} 
    (null, nlsv, cnav, sftr.mass)
  }
  //Internal threads
  private  val doInteracting:Thread = new Thread(){
    override def run() = {
      while(work){   
        this.synchronized{while((interacting == 0) && (work == true)) {wait()}} 
        if(work){ 
          //Get res
          this.synchronized{
            try{   
              //Clone current state  
              val dr = new Rect(new Dot(ISpaceEx.describesRect.top.x,ISpaceEx.describesRect.top.y),new Dot(ITestCameraEx.describesRect.bottom.y, ITestCameraEx.describesRect.bottom.y)) 
              val cn = ISpaceEx.center
              val ftr = new Features(null,shape.filter(p => true), dr, new Dot(lineSpeedVector.x, lineSpeedVector.y), angularVelocity, mass, ISpaceEx.maxRadius, objectType, new Dot(cn.x,cn.y),angle)
              //Call other calcInteracting
              val (clls,oftr) = IInteractingIm.performInteracting(ftr) 
              if(clls != null){
                //Call self calcInteracting
                val ocn = new Dot((oftr.center.x - cn.x),(oftr.center.y - cn.y))
                var csp = List[Dot](); oftr.shape.foreach((p:Dot) => {csp :+= new Dot((p.x + ocn.x),(p.y + ocn.y))}); oftr.shape = csp 
                oftr.describesRect.top.x += ocn.x; oftr.describesRect.top.y += ocn.y; oftr.describesRect.bottom.x += ocn.x; oftr.describesRect.bottom.y += ocn.y
                clls.point.x += ocn.x; clls.point.y += ocn.y 
                val sbpx = clls.segment.begin.x; val sbpy = clls.segment.begin.y
                clls.segment.begin.x = clls.segment.end.x + ocn.x; clls.segment.begin.y = clls.segment.end.y + ocn.y
                clls.segment.end.x = sbpx + ocn.x; clls.segment.end.y = sbpy + ocn.y
                val sasx = clls.selfSpeed.x; val sasy = clls.selfSpeed.y
                clls.selfSpeed.x = clls.otherSpeed.x; clls.selfSpeed.y = clls.otherSpeed.y
                clls.otherSpeed.x = sasx; clls.otherSpeed.y = sasy
                clls.otherCenter.x = ocn.x; clls.otherCenter.y = ocn.y
                val (np,nlsv,nav,nm) = calcInteracting(clls, new Features(polygon,shape, ITestCameraEx.describesRect, lineSpeedVector, angularVelocity, mass,ISpaceEx.maxRadius, objectType, cn,angle), oftr) //Return: new self polygon, line speed vector, self angular velocity
                //Update state
                if(work){ //If this component not be destroy
                  if(np != null){setPolygon(np)}
                  setLineSpeedVector(nlsv)
                  setAngularVelocity(nav)
                  setMass(nm)}}}
            catch{ 
              case ex:NoSuchElementException => {/*do nothing*/}}}      
          //Disconnect
          interacting -= 1
          try{
            disconnectoff("IInteracting")}
          catch{
            case ex:ComponentException => {
              if((ex.number != 6) && ((ex.number != 1))){throw new MatrixException("Component exception: "+ ex.sting)}}
            case ex:NoSuchElementException => {/*do nothing*/}}}}        
    }
  }
  //Internal function
  private def rotatePoints() = { //Copy and rotate(on "angle") from "perimeter" to "figure.list".
    //Equalize
    val sn = polygon.length; var rn = shape.length
    while(sn != rn){  //Equalize
      if(sn < rn){
        shape = shape.drop(1); rn -= 1} 
      else{
        shape :+= new Dot(0,0); rn += 1}}
    //Rotate 
    var i = 0; var xb,yb,xt,yt:Double = 0
    val cosa = cos(angle / 100000); val sina = sin(angle / 100000) 
    polygon.foreach((p:Dot) => {
      val x = (p.x * cosa) - (p.y * sina); val y = (p.x * sina) + (p.y * cosa)
      shape(i).x = x; shape(i).y  = y
      if(x > 0){if(x > xb){xb = x}}else{if(x < xt){xt = x}}
      if(y > 0){if(y > yb){yb = y}}else{if(y < yt){yt = y}}
      i += 1})
    ISpaceEx.describesRect.top.x = xt; ISpaceEx.describesRect.top.y = yt
    ISpaceEx.describesRect.bottom.x = xb; ISpaceEx.describesRect.bottom.y = yb
    ITestCameraEx.describesRect.top.x = xt; ITestCameraEx.describesRect.top.y = yt
    ITestCameraEx.describesRect.bottom.x = xb; ITestCameraEx.describesRect.bottom.y = yb
    ISpaceEx.shape = shape; ITestCameraEx.shape = shape              
  }
  private def rotateTexture(a:Double) = {  
    val ar = a / 100000
    val g = (turnedTexture.asInstanceOf[BufferedImage]).createGraphics()
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR))
    g.fillRect(0,0, turnedTextureSize, turnedTextureSize) 
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER)) 
    g.translate((turnedTextureSize / 2) - textureCenter.x, (turnedTextureSize / 2) - textureCenter.y)  
    g.rotate(ar, textureCenter.x, textureCenter.y)    
    g.drawRenderedImage(texture.asInstanceOf[RenderedImage], null)
    g.dispose()
  }
  private def calcCollision(sftr:Features, oftr:Features, cocn:Dot):Collision = { 
    var cis = true; var cs:Segment = null; var ssbp:Dot = null; var cp:Dot = null; var ass:Dot = null; var aos:Dot = null
    //Search for the point of intersection 
    var si = 0; val ss = sftr.shape.size; var oi = 0; val os = oftr.shape.size; var ipl = List[(Dot,Dot)]()   
    while(si < ss){      
      val sbp = sftr.shape(si)
      val sep = if(si == (ss - 1)){sftr.shape(0)}else{sftr.shape(si + 1)}
      oi = 0
      while(oi < os){      
        val obp = oftr.shape(oi)
        val oep = if(oi == (os - 1)){oftr.shape(0)}else{oftr.shape(oi + 1)}
        val ip = calckIntersection(sbp, sep, obp, oep) 
        if(ip != null){
          ipl :+= (ip, sbp)}
        oi += 1}
      si += 1}
    if(ipl.size < 2){cis = false}
    //Search for the max distance point to collision segment
    if(cis){
      var md:Double = 0; var mfp:Dot = null; var msp:Dot = null; var mssbp:Dot = null
      ipl.foreach((fp:(Dot,Dot)) => {ipl.foreach((sp:(Dot,Dot)) => {  
        val fx = fp._1.x - sp._1.x; val fy = fp._1.y - sp._1.y
        val d = (sqrt((fx * fx)+(fy * fy))) 
        if(d > md){ md = d; mfp = fp._1; mssbp = fp._2; msp = sp._1}})})
        if((mfp == null) || (msp == null)){throw new MatrixException("Internal error: Cannot calculate the collision segment")}
        cs = new Segment(mfp,msp); ssbp = mssbp}
    //Calc collision point
    if(cis){
      cp = new Dot((cs.begin.x + cs.end.x) / 2,(cs.begin.y + cs.end.y) / 2)} //Calculation middle
    //Calculation absolute speed vector
    if(cis){
      val ss = calcAbsSpeed(cp, sftr.angularVelocity, sftr.maxRadius, sftr.lineSpeedVector)
      val os = calcAbsSpeed(new Dot((cp.x - cocn.x),(cp.y - cocn.y)), oftr.angularVelocity, oftr.maxRadius, oftr.lineSpeedVector)
      if((ss.x == 0) && (ss.y == 0) && (os.x == 0) && (os.y == 0)){cis = false}else{ass = ss; aos = os}}
    //Check and correcting begin-end of collision segment(self shape to the right of collision segment)
    if(cis){
      if(checkHit(ssbp, oftr.describesRect,  oftr.shape)){
        cs = new Segment(cs.end,cs.begin)}}
    //Create and return collision
    if(cis){new Collision(cp,cs,ass,aos,cocn)}else{null}  
  }
  private def checkHit(p:Dot, dr:Rect, shp:List[Dot]):Boolean = {
    if((p.x > dr.top.x) && (p.x < dr.bottom.x) && (p.y > dr.top.y) && (p.y < dr.bottom.y)){ //Check hit some point of other polygon(spr) in self discribes rect(fdr) 
      var f = false; val fprl = shp.size; var i = 0; var j = fprl - 1
      while( i < fprl) { //Check hit found point(x,y) in self polygon(fpr)
        val g = (shp(j).y-shp(i).y) + shp(i).x 
        if((shp(i).y > p.y) != (shp(j).y > p.y) && (p.x < (shp(j).x - shp(i).x) * (if(g == 0){0}else{(p.y - shp(i).y) / g}))){
          f = !f}
        j = i; i += 1}
      return f}
    false
  }
  private def calcAbsSpeed(cp:Dot, av:Double, mr:Double,ls:Dot):Dot = {//Return: absolute speed
    val cpd = sqrt((cp.x * cp.x)+(cp.y * cp.y)) //collision point distance      
    val cav = if(mr == 0){0}else{av * (cpd / mr)} //angular velocity * (collision point distance / max radius) = angular velocity in collision point    
    if(cav != 0){ //Return: absolute self speed (ass)
      val g = sqrt(((cp.x - cav) * (cp.x - cav)) + (cp.y * cp.y)) //opposite side "a" of triangle "a", "b"(cpd), "c"(cav)
      val pt = 2 * cpd * cav.abs
      val a = acos(if(pt == 0){0}else{(((cpd * cpd) + (cav * cav)) - (g * g)) / pt})//angle
      val fa = (if(cp.y <= 0){(2 * Pi) - a}else{a}) +  (Pi / 2) //fool angle + 90
      val ca = if(fa > (Pi * 2)){fa - (Pi * 2)}else{fa} //correct 
      val cosa = cos(ca); val sina = sin(ca)
      new Dot((((cav.abs * cosa) - (0 * sina)) +  ls.x) ,(((cav.abs * sina) + (0 * cosa)) + ls.y))} //Rotate cav(vector) on fa, and add lineSpeedVector
    else{
      ls}  
  }
  private def calckIntersection(sbp:Dot, sep:Dot, obp:Dot, oep:Dot):Dot ={
    val d  = (sbp.x - sep.x)*(oep.y - obp.y) - (sbp.y - sep.y)*(oep.x - obp.x)
    val da = (sbp.x - obp.x)*(oep.y - obp.y) - (sbp.y - obp.y)*(oep.x - obp.x)
    val db = (sbp.x - sep.x)*(sbp.y - obp.y) - (sbp.y - sep.y)*(sbp.x - obp.x)  
   if(d.abs < 0.000001){ 
     return null}
   else{
     val ta = if(d == 0){0}else{da / d}; val tb = if(d == 0){0}else{db / d}
     if((0 < ta) && (ta < 1) && (0 < tb) && (tb < 1)){
       return new Dot(sbp.x + ta * (sep.x - sbp.x), sbp.y + ta * (sep.y - sbp.y))}
     else{ 
       return null}}
  }
  private def lineSpeedChange() = {  
      //Calc line speed, step
      if(((lineSpeedVector.x == 0)&&(lineSpeedVector.y == 0))||(lineSpeedVector.x !=lineSpeedVector.x)||(lineSpeedVector.y != lineSpeedVector.y)){
        lineSpeed = 0}
      else{
        lineSpeed = sqrt((lineSpeedVector.x * lineSpeedVector.x)+(lineSpeedVector.y * lineSpeedVector.y))}
      //Calc line step
      stepLineSpeed.x = 0; stepLineSpeed.y = 0
      if((lineSpeedVector.x != 0) && (lineSpeedVector.y != 0)){
        val a = if(lineSpeedVector.x == 0){0}else{atan((lineSpeedVector.y).abs / (lineSpeedVector.x).abs)}
        stepLineSpeed.x = cos(a)
        stepLineSpeed.y = stepLineSpeed.x * tan(a)}
      else{
        if(lineSpeedVector.x != 0){stepLineSpeed.x = 1}
        if(lineSpeedVector.y != 0){stepLineSpeed.y = 1}}
      if(lineSpeedVector.x < 0){stepLineSpeed.x *= -1}
      if(lineSpeedVector.y < 0){stepLineSpeed.y *= -1}
      ITestCameraEx.lineSpeedVector.x = lineSpeedVector.x; ITestCameraEx.lineSpeedVector.y = lineSpeedVector.y
  }
  //Fields 
  private var polygon = List[Dot]()
  private var shape = List[Dot]()  
  private val lineSpeedVector = new Dot(0,0)      
  private var lineSpeed:Double = 0           
  private val stepLineSpeed = new Dot(0,0)     
  private var moveCounter:Double = 0 
  private var angularVelocity:Double = 0
  private var maxAngularVelocity:Double = 0   
  private var angularVelocityCounter:Double = 0 
  private var angle:Double = 0  //0-628318 
  private var mass:Double = 0   
  private var friction:Double = 0   
  private var slowdown:Double = 0  
  private var work = true
  private var interacting = 0 
  private var syncTr:Thread = null
  private var syncCount = 0
  private var texture:Image = null
  private var turnedTexture:Image = null
  private var turnedTextureSize = 0
  private val textureCenter = new Dot(0,0)    
  private var textureAngle:Double = 0 
  private var textureSet = false
  protected var objectType = 0
} 


