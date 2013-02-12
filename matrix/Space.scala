package matrix
import componentwork._,
       javax.swing.Timer,
       java.awt.event.{ActionListener, ActionEvent}, 
       scala.collection.mutable.{Map => MMap},
       scala.math.{sqrt,Pi,cos,sin,pow},
       java.lang.System
 
       
class Space(val name:MMap[String, Component]) extends Component {
  //Multiinterface "ISpace" 
  val ISpaceEx:ISpaceFR = new ISpaceFR;  var ISpaceIm = MMap[Component, ISpaceS]()
  interfaces += ("ISpace" -> (ISpaceEx,"ISpaceF","ISpaceS", (c:Component, i:Interface) => {ISpaceIm += (c -> i.asInstanceOf[ISpaceS])}, (c:Component) => {ISpaceIm -= c; false}, true))
  //Multiinterface "ILayout" 
  val ILayoutEx:ILayoutFR = new ILayoutFR;  var ILayoutIm = MMap[Component, ILayoutS]()
  interfaces += ("ILayout" -> (ILayoutEx,"ILayoutF","ILayoutS", (c:Component, i:Interface) => {ILayoutIm += (c -> i.asInstanceOf[ILayoutS])}, (c:Component) => {ILayoutIm -= c; false}, true))
  //Interface "IControl" 
  val IControlEx:IControlSR = new IControlSR; var IControlIm:IControlF = null   
  interfaces += ("IControl" -> (IControlEx,"IControlS","IControlF", (c:Component, i:Interface) => {IControlIm = i.asInstanceOf[IControlF]}, (c:Component) => {IControlIm = null; false}, false)) 
  //Interfaces export realization
  class ISpaceFR extends ISpaceF {
    override def connection(c:Component) = {
      val cp = new Dot(ISpaceIm(c).preferredPost.x, ISpaceIm(c).preferredPost.y)   
      if((((area.w != 0)&&(area.h != 0))&&((cp.x < 0)||(cp.x > area.w)||(cp.y < 0)||(cp.y > area.h)))||(cp.x > 3000)||(cp.y > 3000)||(cp.x < 0)||(cp.y < 0)){
        throw new MatrixException("Wrong preferred post")}
      //Correction central point
      val ci = ISpaceIm(c) 
      if(checkIntersection(c,ci,cp)){
        var f = true; var d:Double = 5; val td = new Dot(0,0); val cd = new Dot(cp.x,cp.x)  
        while (f){
          var aof = true; var a:Double = 0
          while(f && (a < (2 *Pi))){ 
            val cosa = cos(a); val sina = sin(a) 
            td.x = cosa - (d * sina); td.y = sina + (d * cosa)  
            cd.x = cp.x + td.x; cd.y = cp.y + td.y
            if((area.w != 0)&&(area.h != 0)){
              if((cd.x >= 0)&&(cd.x <= area.w)&&(cd.y >= 0)&&(cd.y <= area.h)){ 
                if(! checkIntersection(c,ci,cd)){f = false}  
                aof = false}}
            else{
              if(checkIntersection(c,ci,cd)){f = false}
              aof = false}
            a += Pi / 16}
          d += 5 
          if(aof || (d > 3000)){throw new MatrixException("Cen not locate object")}}
        cp.x = cd.x; cp.y = cd.y}
      //Add to list
      ILayoutEx.centers += (c -> cp)  
      ISpaceIm(c).center.x = cp.x; ISpaceIm(c).center.y = cp.y  
    } 
    override def disconnection(c:Component) = {
      ILayoutEx.centers -= c
    }
  }
  class ILayoutFR extends ILayoutF 
  class IControlSR extends IControlS {
    override def setMovement(f:Boolean) = {movement = f; if(f){movementLoop.synchronized{movementLoop.notify()}}}
    override def setArea(as:Area) = {area.w = as.w; area.h = as.h}
  }  
  //Constructor/deconstructor
  override def construction() = {
    super.construction()
    movementLoop.start()
    movementTimer.start()
  }   
  override def deconstruction() = {
    super.deconstruction()
    work = false; movementLoop.synchronized{movementLoop.notify()}
  }   
  //Movement loop
  private val movementTimer:Thread = new Thread(){
    override def run() = {
      while(work){   
        Thread.sleep(2)
        movementLoop.synchronized{movementLoop.notify()}}  
    }
  }
  private val movementLoop:Thread = new Thread(){
    override def run() = {
      while(work){   
        this.synchronized{wait()} 
        if(work && movement){  
          //Movement tick
          faced = faced.drop(faced.length) 
          ISpaceIm.foreach((ver:(Component, ISpaceS)) => { 
            val vc = ver._1; val vci = ver._2;
            //Call tick 
            val voc = vci.tick //Return: (polygon change, x-move, y-move)
            try{  
              //Move center 
              val cn = ILayoutEx.centers(vc)
              if( voc._2 != 0 || voc._3 != 0){  
                cn.x += voc._2
                cn.y += voc._3
                vci.center.x = cn.x
                vci.center.y = cn.y}
              //Check out of area
              if(((area.w != 0)&&(area.h != 0))&&((cn.x < 0)||(cn.x > area.w)||(cn.y < 0)||(cn.y > area.h))){
                destroyComponent(vc)}
              else{
                //Checking to interaction and connect if so                
                val vmr = ver._2.maxRadius; val vcn = ver._2.center
                ISpaceIm.foreach((item:(Component, ISpaceS)) => {
                  val oc = item._1 
                  if(vc != oc){ //If not same v. object
                    var cf = true; faced.foreach((item:(Component,Component)) => {if((vc == item._1)&&(oc == item._2)){cf = false}}) 
                    if(cf){ //If already faced 
                      val ocn = item._2.center; val omr = item._2.maxRadius
                      val dx = ocn.x - vcn.x; val dy = ocn.y - vcn.y; val d = sqrt((dx * dx)+(dy * dy)) //Distance between virtual object
                      if(d < (vmr + omr)){ //If distance between centers less a sum of max radius  
                        val(hf,af) = checkHit(vcn, ocn, ver._2.describesRect, item._2.describesRect, ver._2.shape, item._2.shape)
                        if(af){faced :+= (oc, vc)}
                        if(hf){
                          var cf = true; earlierFaced.foreach((item:(Component,Component)) => {if((((vc == item._1)&&(oc == item._2))||((oc == item._1)&&(vc == item._2)))){cf = false}}) //If already faced 
                          if(cf){
                            //Connect
                            var nc = true
                            while(nc){
                              try{
                                connect("IInteracting",vc,"ISynergy",oc); nc = false}                        
                              catch{
                                case ex:ComponentException => {
                                  try{
                                    connect("IInteracting",oc,"ISynergy",vc); nc = false}
                                  catch{
                                    case ex:ComponentException => {
                                      if(ex.number == 4){
                                        Thread.`yield`()}
                                      else{
                                        throw new MatrixException("Component exception: "+ ex.sting)}}}}}}}}}}}})}}
            catch{
             case ex:NoSuchElementException => {}}})
          earlierFaced = earlierFaced.drop(earlierFaced.length) 
          earlierFaced :::= faced}}           
    }
  }
  //Function
  private def checkIntersection (c:Component,ci:ISpaceS,cn:Dot):Boolean = { 
    var f = false
    ISpaceIm.foreach((item:(Component, ISpaceS)) => { 
      if((item._1 != c)&& ! f){
        val dx = item._2.center.x - cn.x; val dy = item._2.center.y - cn.y
        val dr = ci.describesRect; val shp = item._2.shape; val l = shp.size; var i = 0
        while((i < l)&& ! f){
          val p = shp(i) 
          if(((p.x + dx) > (dr.top.x - 2))&&((p.x + dx) < (dr.bottom.x + 2))&&((p.y + dy) > (dr.top.y - 2))&&((p.y + dy) < (dr.bottom.y + 2))){f = true} 
          i += 1} 
        if(! f){
          val dr = item._2.describesRect; val shp = ci.shape; val l = shp.size; var i = 0
          while((i < l)&& ! f){
            val p = shp(i) 
            if((p.x > (dr.top.x - dx - 2))&&(p.x < (dr.bottom.x - (dx + 2)))&&(p.y > (dr.top.y - dy - 2))&&(p.y  < (dr.bottom.y - (dy + 2)))){f = true} 
            i += 1}}}})
    f  
  }
  private def checkHit(fc:Dot, sc:Dot, fdr:Rect, sdr:Rect, fpr:List[Dot], spr:List[Dot]):(Boolean, Boolean) = { //Return: hit flag, add flag
    var hf = false; var af = false; val dx = fc.x - sc.x; val dy = fc.y - sc.y
    //Check describes rectangles intersection 
//    if(! hf){
//      var si = 4; var fi = 0
//      while(!hf && (si != 0)){
//        val(sbx,sby,sex,sey) = si match{
//          case 4 => ((sdr.top.x - dx), (sdr.top.y - dy), (sdr.bottom.x - dx), (sdr.top.y - dy))
//          case 3 => ((sdr.bottom.x - dx), (sdr.top.y - dy), (sdr.bottom.x - dx), (sdr.bottom.y - dy))
//          case 2 => ((sdr.bottom.x - dx), (sdr.bottom.y - dy), (sdr.top.x - dx), (sdr.bottom.y - dy))
//          case 1 => ((sdr.top.x - dx), (sdr.bottom.y - dy), (sdr.top.x - dx), (sdr.top.y - dy))}
//        fi = 4
//        while(!hf && (fi != 0)){
//          hf = fi match{
//            case 4 => checkIntersection(fdr.top.x, fdr.top.y, fdr.bottom.x, fdr.top.y, sbx, sby, sex, sey)
//            case 3 => checkIntersection(fdr.bottom.x, fdr.top.y, fdr.bottom.x, fdr.bottom.y, sbx, sby, sex, sey)
//            case 2 => checkIntersection(fdr.bottom.x,fdr.bottom.y, fdr.top.x, fdr.bottom.y, sbx, sby, sex, sey)
//            case 1 => checkIntersection(fdr.top.x, fdr.bottom.y, fdr.top.x, fdr.top.y, sbx, sby, sex, sey)}
//          fi -= 1}
//        si -= 1}}
    //Check hit points to describes rectangles 
    if(! hf){
      var i = 0
      while (!hf && (i < spr.size)){
        val p = spr(i); val x = p.x - dx; val y = p.y - dy //Correct coodinats to self system     
        if((x >= fdr.top.x) && (x <= fdr.bottom.x) && (y >= fdr.top.y) && (y <= fdr.bottom.y)){hf = true} //Check hit some point of other polygon(spr) in self discribes rect(fdr)
        i += 1}}          
    //Check intersection polygons
    if(hf){
      var si = 0; val ss = spr.size; var fi = 0; val fs = fpr.size; var ni = 0   
      while((si < ss) &&(ni < 2)){    
        val sbp = spr(si)
        val sep = if(si == (ss - 1)){spr(0)}else{spr(si + 1)}
        val sbpx = sbp.x - dx; val sbpy = sbp.y - dy 
        val sepx = sep.x - dx; val sepy = sep.y - dy 
        fi = 0
        while((fi < fs)&&(ni < 2)){      
          val fbp = fpr(fi)
          val fep = if(fi == (fs - 1)){fpr(0)}else{fpr(fi + 1)}
          if(checkIntersection(fbp.x, fbp.y, fep.x, fep.y, sbpx, sbpy, sepx, sepy)){ni += 1}
          fi += 1}
        si += 1}             
      if(ni < 2){hf = false}
      if(ni != 0){af = true}}
    (hf,af) 
  }
  private def checkIntersection(fbx:Double, fby:Double, fex:Double, fey:Double, sbx:Double, sby:Double, sex:Double, sey:Double):Boolean ={
    val d  = (fbx - fex)*(sey - sby) - (fby - fey)*(sex - sbx)
    val da = (fbx - sbx)*(sey - sby) - (fby - sby)*(sex - sbx)
    val db = (fbx - fex)*(fby - sby) - (fby - fey)*(fbx - sbx)  
    if(d.abs < 0.000001){
      false}  
    else{
      val ta = if(d == 0){0}else{da/d}; val tb = if(d == 0){0}else{db/d}   
      if((0 < ta) && (ta < 1) && (0 < tb) && (tb < 1)){true}else{ false}}
  }
  //Fields
  private var work = true
  private var movement = false
  private var faced = List[(Component,Component)]()
  private var earlierFaced = List[(Component,Component)]()
  private val area = new Area(0,0)
  //Service code  
  construction()
} 