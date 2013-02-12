package matrix
import componentwork._,
       componentswing.{IViewDrawS,IViewDrawF,ObjTexture},
       javax.swing.Timer,
       java.awt.event.{ActionListener, ActionEvent},   
       java.awt.{Point,Rectangle},
       scala.collection.mutable.{Map => MMap},
       scala.math.sqrt 

       
class Camera(val name:MMap[String, Component]) extends Component {
  //Multiinterface "ICamera"
  val ICameraEx:ICameraFR = new ICameraFR;  var ICameraIm = MMap[Component, ICameraS]()
  interfaces += ("ICamera" -> (ICameraEx,"ICameraF","ICameraS", (c:Component, i:Interface) => {ICameraIm += (c -> i.asInstanceOf[ICameraS])}, (c:Component) => {ICameraIm -= c; false}, true))
  //Interface "IViewDraw" 
  val IViewDrawEx:IViewDrawSR = new IViewDrawSR; var IViewDrawIm:IViewDrawF = null   
  interfaces += ("IViewDraw" -> (IViewDrawEx,"IViewDrawS","IViewDrawF", (c:Component, i:Interface) => {IViewDrawIm = i.asInstanceOf[IViewDrawF]}, (c:Component) => {IViewDrawIm = null; false}, false))
  //Interface "ILayout" 
  val ILayoutEx:ILayoutSR = new ILayoutSR; var ILayoutIm:ILayoutF = null   
  interfaces += ("ILayout" -> (ILayoutEx,"ILayoutS","ILayoutF", (c:Component, i:Interface) => {ILayoutIm = i.asInstanceOf[ILayoutF]}, (c:Component) => {ILayoutIm = null; false}, false))
  //Interface "IControl" 
  val IControlEx:IControlSR = new IControlSR; var IControlIm:IControlF = null   
  interfaces += ("IControl" -> (IControlEx,"IControlS","IControlF", (c:Component, i:Interface) => {IControlIm = i.asInstanceOf[IControlF]}, (c:Component) => {IControlIm = null; false}, false))
  //Interfaces export realization
  class ICameraFR extends ICameraF {
    override def connection(c:Component) = {
      IViewDrawEx.objects += (c -> new ObjTexture(null, new Point(0,0), new Point(0,0)))  //New virtual object
    }   
    override def disconnection(c:Component) = {
      IViewDrawEx.objects -= c 
      if(IViewDrawIm != null){IViewDrawIm.refresh()}
    }  
  }
  class IViewDrawSR extends IViewDrawS {
    override def connection(c:Component) = {
    }  
  }
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
            ILayoutIm.centers.foreach((center:(Component, Dot)) => {
              try{
                val vo = center._1; val cp = center._2; val ocn = IViewDrawEx.objects(vo).center
                val cnc = if((ocn.x != cp.x.asInstanceOf[Int])||(ocn.y != cp.y.asInstanceOf[Int])){
                  ocn.x = cp.x.asInstanceOf[Int]; ocn.y = cp.y.asInstanceOf[Int]
                  true} 
                else{
                  false}  
                val(tcf,tx,ts) = ICameraIm(vo).getTexture
                if(tx == null){
                  IViewDrawEx.objects(vo).texture = if(IViewDrawEx.objects(vo).texture != null){refresh = true; null}else{null}}
                else{
                  if(tcf || cnc){
                    IViewDrawEx.objects(vo).texture = tx
                    val s = ts / 2
                    IViewDrawEx.objects(vo).coordinates.x = (cp.x - s).asInstanceOf[Int]
                    IViewDrawEx.objects(vo).coordinates.y = (cp.y - s).asInstanceOf[Int]
                    refresh = true}}}
              catch{
                case ex:NoSuchElementException => {/*do nothing*/}}
            })}
          //Refresh if need
          if((IViewDrawIm != null) && refresh){IViewDrawIm.refresh()}}}
    }
  }  
  //Fields
  private var work = true
  private var movement = false
  //Service code  
  construction()
} 