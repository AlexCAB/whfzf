package test
import componentwork._,   
       matrix.{ILayoutF,ILayoutS,IControlF,IControlS,Dot,Collision,ITestCameraF,ITestCameraS},
       javax.swing.Timer,
       java.awt.event.{ActionListener, ActionEvent},
       java.awt.{Point,Rectangle},
       scala.collection.mutable.{Map => MMap},
       scala.math.sqrt, 
       scala.collection.mutable.{Map => MMap}
 

class TestCamera(val name:MMap[String, Component]) extends Component {
  //Multiinterface "ITestCamera"
  val ITestCameraEx:ITestCameraFR = new ITestCameraFR;  var ITestCameraIm = MMap[Component, ITestCameraS]()
  interfaces += ("ITestCamera" -> (ITestCameraEx,"ITestCameraF","ITestCameraS", (c:Component, i:Interface) => {ITestCameraIm += (c -> i.asInstanceOf[ITestCameraS])}, (c:Component) => {ITestCameraIm -= c; false}, true))
  //Interface "ITestViewDraw" 
  val ITestViewDrawEx:ITestViewDrawSR = new ITestViewDrawSR; var ITestViewDrawIm:ITestViewDrawF = null   
  interfaces += ("ITestViewDraw" -> (ITestViewDrawEx,"ITestViewDrawS","ITestViewDrawF", (c:Component, i:Interface) => {ITestViewDrawIm = i.asInstanceOf[ITestViewDrawF]}, (c:Component) => {ITestViewDrawIm = null; false}, false))
  //Interface "ILayout" 
  val ILayoutEx:ILayoutSR = new ILayoutSR; var ILayoutIm:ILayoutF = null   
  interfaces += ("ILayout" -> (ILayoutEx,"ILayoutS","ILayoutF", (c:Component, i:Interface) => {ILayoutIm = i.asInstanceOf[ILayoutF]}, (c:Component) => {ILayoutIm = null; false}, false))
  //Interface "IControl" 
  val IControlEx:IControlSR = new IControlSR; var IControlIm:IControlF = null   
  interfaces += ("IControl" -> (IControlEx,"IControlS","IControlF", (c:Component, i:Interface) => {IControlIm = i.asInstanceOf[IControlF]}, (c:Component) => {IControlIm = null; false}, false))
  //Interfaces export realization
  class ITestCameraFR extends ITestCameraF {
    override def connection(c:Component) = {
      ITestViewDrawEx.objects += (c -> new Obj(false, new Point(0,0), List[Point](), new Rectangle(0,0,0,0), new Point(0,0), 0, 0))  //New virtual object
    }   
    override def disconnection(c:Component) = {
      ITestViewDrawEx.objects -= c 
      if(ITestViewDrawIm != null){ITestViewDrawIm.refresh()}
    }  
  }
  class ITestViewDrawSR extends ITestViewDrawS 
  class ILayoutSR extends ILayoutS 
  class IControlSR extends IControlS {
    override def setMovement(f:Boolean) = {movement = f; if(f){shotLoop.synchronized{shotLoop.notify()}}}
  }
  //Constructor/deconstructor
  override def construction() = {
    super.construction()
    shotLoop.start()
    shotTimer.start()
  }   
  override def deconstruction() = {
    super.deconstruction()
    work = false; shotLoop.synchronized{shotLoop.notify()}
  }   
  //Shot loop
  private val shotTimer:Thread = new Thread(){
    override def run() = {
      while(work){   
        Thread.sleep(50)
        shotLoop.synchronized{shotLoop.notify()}}  
    }
  }
  private val shotLoop:Thread = new Thread(){
    override def run() = {
      while(work){   
        this.synchronized{wait()} 
        if(work && movement){ 
          var refresh = false
          if(ILayoutIm != null){
            //Update 
            ITestViewDrawEx.objects.foreach((obj:(Component,Obj)) => {ITestViewDrawEx.objects(obj._1).visible = false})    
            ILayoutIm.centers.foreach((center:(Component, Dot)) => {
              try{          
                val vo = center._1; val ncp = center._2
                if(ITestCameraIm.contains(vo) || ITestViewDrawEx.objects.contains(vo)){  //If virtual object ready  
                  val ocn = ITestViewDrawEx.objects(vo).centre
                  val fcc = if((ocn.x != ncp.x.asInstanceOf[Int])||(ocn.y != ncp.y.asInstanceOf[Int])){true}else{false}    
                  val fpc = ITestCameraIm(vo).perimeterChange   
                  if(fcc && !fpc){  //Correcting perimetr 
                    val cp = ITestViewDrawEx.objects(vo).centre
                    val dx = ncp.x - cp.x; val dy = ncp.y - cp.y; var i = 0
                    ITestViewDrawEx.objects(vo).perimeter.foreach((p:Point) => {  //Calc absolute coordinates
                      ITestViewDrawEx.objects(vo).perimeter(i).x = (p.x + dx).asInstanceOf[Int]; ITestViewDrawEx.objects(vo).perimeter(i).y = (p.y + dy).asInstanceOf[Int]
                      i += 1}) 
                    refresh = true}
                  if(fcc){  //Update center  
                    ITestViewDrawEx.objects(vo).centre.x = ncp.x.asInstanceOf[Int]; ITestViewDrawEx.objects(vo).centre.y = ncp.y.asInstanceOf[Int]
                    refresh = true}
                  if(fpc || fcc){ //Update perimeter 
                    val sn = ITestCameraIm(vo).shape.length; var rn = ITestViewDrawEx.objects(vo).perimeter.length
                    while(sn != rn){  //Equalize
                      if(sn < rn){
                        ITestViewDrawEx.objects(vo).perimeter = ITestViewDrawEx.objects(vo).perimeter.drop(1)
                        rn -= 1}
                      else{
                        ITestViewDrawEx.objects(vo).perimeter :+= new Point(0,0)
                        rn += 1}}
                    var i = 0  
                    ITestCameraIm(vo).shape.foreach((p:Dot) => {  //Calc absolute coordinates
                      ITestViewDrawEx.objects(vo).perimeter(i).x = (p.x + ncp.x).asInstanceOf[Int]; ITestViewDrawEx.objects(vo).perimeter(i).y = (p.y + ncp.y).asInstanceOf[Int]
                      i += 1}) 
                    refresh = true}
                  if(fcc || fpc){ //Update describes rectangle
                    val cp = center._2 
                    val dr = ITestCameraIm(vo).describesRect
                    ITestViewDrawEx.objects(vo).describesRect.x = (dr.top.x + cp.x).asInstanceOf[Int]
                    ITestViewDrawEx.objects(vo).describesRect.y = (dr.top.y + cp.y).asInstanceOf[Int]
                    ITestViewDrawEx.objects(vo).describesRect.height = (dr.top.x.abs + dr.bottom.x.abs).asInstanceOf[Int]
                    ITestViewDrawEx.objects(vo).describesRect.width = (dr.top.y.abs + dr.bottom.y.abs).asInstanceOf[Int]}
                  ITestViewDrawEx.objects(vo).visible = true 
                //Update features
                val lsv = ITestCameraIm(vo).lineSpeedVector
                ITestViewDrawEx.objects(vo).lineSpeedVector.x = lsv.x.asInstanceOf[Int]
                ITestViewDrawEx.objects(vo).lineSpeedVector.y = lsv.y.asInstanceOf[Int]
                ITestViewDrawEx.objects(vo).angularVelocity = ITestCameraIm(vo).angularVelocity.asInstanceOf[Int]
                ITestViewDrawEx.objects(vo).mass = ITestCameraIm(vo).mass.asInstanceOf[Int]
                val clls = ITestCameraIm(vo).lastCollision
                if((clls != null) && (lastCollisionLink != clls)){
                  lastCollisionLink = clls; ITestCameraIm(vo).lastCollision = null
                  val p = new Point((clls.point.x + ncp.x).asInstanceOf[Int], (clls.point.y + ncp.y).asInstanceOf[Int])
                  val (x,y) = {
                    val dx = clls.segment.end.x - clls.segment.begin.x; val dy = clls.segment.end.y - clls.segment.begin.y
                    val d = sqrt((dx * dx)+(dy * dy)); val k = 40 / d
                    (clls.segment.begin.x + k * (clls.segment.end.x - clls.segment.begin.x), clls.segment.begin.y + k * (clls.segment.end.y - clls.segment.begin.y))}
                  val csb = new Point((clls.segment.begin.x + ncp.x).asInstanceOf[Int], (clls.segment.begin.y + ncp.y).asInstanceOf[Int]) 
                  val cse = new Point((x + ncp.x).asInstanceOf[Int], (y + ncp.y).asInstanceOf[Int])
                  val fs = new Point((clls.selfSpeed.x + p.x).asInstanceOf[Int], (clls.selfSpeed.y + p.y).asInstanceOf[Int])
                  val ss = new Point((clls.otherSpeed.x + p.x).asInstanceOf[Int], (clls.otherSpeed.y + p.y).asInstanceOf[Int])
                  ITestViewDrawEx.lastCollision = new Coll(p,csb,cse,fs,ss)}
                //Reset flag
                ITestCameraIm(vo).perimeterChange = false}}
              catch{
                case ex:NoSuchElementException => {/*do nothing*/}}})}
          //Refresh if need
          if((ITestViewDrawIm != null) && refresh){ITestViewDrawIm.refresh()}}}
    }
  }
  //Fields
  private var work = true
  private var movement = false
  private var lastCollisionLink:Collision = null  
  //Service code  
  construction()
} 