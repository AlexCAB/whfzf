package test
import componentwork._,
       componentswing.{IWidgetS,IWidgetF,IViewDrawF,IViewDrawS,IFrameS,IFrameF,IViewSettingsF,IViewSettingsS},
       matrix.{IControlF,IControlS},
       javax.swing.JPanel,
       java.awt.{BorderLayout, Dimension, Graphics, Graphics2D, Color, Point},
       java.awt.event.{ComponentAdapter,ComponentEvent},
       java.awt.geom.Ellipse2D,
       java.awt.image.BufferedImage,
       scala.collection.mutable.{Map => MMap},
       scala.math.sqrt

       
class TestViewDraw (val name:MMap[String, Component], rootConName:String, rootConComponent:Component) extends JPanel with Component {
  //Interface "IWidget"(root)
  val IWidgetEx:IWidgetSR = new IWidgetSR; var IWidgetIm:IWidgetF = null   
  interfaces += ("IWidget" -> (IWidgetEx,"IWidgetS","IWidgetF", (c:Component, i:Interface) => {IWidgetIm = i.asInstanceOf[IWidgetF]}, (c:Component) => {IWidgetIm = null; true}, false))
  //Multiinterface "IViewSettings" 
  val IViewSettingsEx:IViewSettingsFR = new IViewSettingsFR;  var IViewSettingsIm = MMap[Component, IViewSettingsS]()
  interfaces += ("IViewSettings" -> (IViewSettingsEx,"IViewSettingsF","IViewSettingsS", (c:Component, i:Interface) => {IViewSettingsIm += (c -> i.asInstanceOf[IViewSettingsS])}, (c:Component) => {IViewSettingsIm -= c; false}, true))
  //Interface "ITestViewDraw" 
  val ITestViewDrawEx:ITestViewDrawFR = new ITestViewDrawFR; var ITestViewDrawIm:ITestViewDrawS = null   
  interfaces += ("ITestViewDraw" -> (ITestViewDrawEx,"ITestViewDrawF","ITestViewDrawS", (c:Component, i:Interface) => {ITestViewDrawIm = i.asInstanceOf[ITestViewDrawS]}, (c:Component) => {ITestViewDrawIm = null; false}, false))
  //Interfaces export realization
  class IWidgetSR  extends IWidgetS
  class IViewDrawFR extends IViewDrawF {
    override def refresh() = {thisComponent.asInstanceOf[TestViewDraw].repaint()}
  }  
  class ITestViewDrawFR extends ITestViewDrawF {
    override def refresh() = {thisComponent.asInstanceOf[TestViewDraw].repaint()}
  }
  class IViewSettingsFR  extends IViewSettingsF
  //Self-assembly
  setPreferredSize(new Dimension(700,400)); IViewSettingsEx.dimension.width = 700; IViewSettingsEx.dimension.height = 400
  setBackground(Color.white)
  IWidgetEx.component = thisComponent.asInstanceOf[TestViewDraw]
  IWidgetEx.position = BorderLayout.CENTER
  //Override methods
  override def paintComponent(g:Graphics) = {
    //Make buffer
    val d = getSize(); val (w,h) = (d.width, d.height)
    var f = false; if(buffer == null){f = true}else{ if((buffer.getWidth(),buffer.getHeight()) != (w,h)){f = true } }
    if(f){
      buffer = new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_ARGB)}
    val g2d:Graphics2D = buffer.createGraphics()
    //Clear
    g2d.setColor(Color.white);
    g2d.fillRect(0,0, d.width, d.height) 
    //Draw
    if(ITestViewDrawIm != null){
        ITestViewDrawIm.objects.foreach((obj:(Component,Obj)) => {
          if(obj._2.visible){
            //Draw center 
            g2d.setColor(Color.pink)
            val circle = new Ellipse2D.Double((obj._2.centre.x - 1), (obj._2.centre.y - 1), 3, 3)
            g2d.fill(circle)
            //Draw describes rect 
            val dr = obj._2.describesRect
            g2d.setColor(Color.green)
            g2d.drawLine(dr.x,dr.y,(dr.x + dr.height),dr.y)
            g2d.drawLine((dr.x + dr.height),dr.y,(dr.x + dr.height),(dr.y + dr.width))
            g2d.drawLine((dr.x + dr.height),(dr.y + dr.width),dr.x,(dr.y + dr.width))
            g2d.drawLine(dr.x,(dr.y + dr.width),dr.x,dr.y)}
            //Draw perimeter
            val s = obj._2.perimeter.size
            g2d.setColor(Color.black)
            s match{
              case 0 =>
              case 1 => val p0 = obj._2.perimeter(0); g2d.drawLine(p0.x,p0.y,p0.x,p0.y)
              case 2 => val p0 = obj._2.perimeter(0); val p1 = obj._2.perimeter(1); g2d.drawLine(p0.x,p0.y,p1.x,p1.y)
              case _ => 
                var i = 0
                while(i < (s - 1)){
                  val p0 = obj._2.perimeter(i); val p1 = obj._2.perimeter(i + 1); g2d.drawLine(p0.x,p0.y,p1.x,p1.y)
                  i += 1}
                val p0 = obj._2.perimeter(0); val p1 = obj._2.perimeter(i); g2d.drawLine(p0.x,p0.y,p1.x,p1.y)}
            //Draw name, coordinate and mass
            val cn = obj._1.toString(); var lcn = ""
            cn.foreach((cr:Char) => {if(lcn == ""){if(cr == '@'){lcn += cr}}else{lcn += cr}}) 
            lcn = lcn + " " + obj._2.centre.x + "x" + obj._2.centre.y 
            g2d.setColor(Color.gray)
            g2d.drawString(lcn, (obj._2.centre.x - 50), (obj._2.centre.y - 5)) 
            //Draw features 
            val ls = (sqrt((obj._2.lineSpeedVector.x * obj._2.lineSpeedVector.x)+(obj._2.lineSpeedVector.y * obj._2.lineSpeedVector.y))).asInstanceOf[Int]
            var lft = "LS="+ ls + ",AV=" + obj._2.angularVelocity + ",M=" + obj._2.mass
            g2d.setColor(Color.gray)
            g2d.drawString(lft, (obj._2.centre.x - 50), (obj._2.centre.y + 18)) 
            //Draw point coordinates
            g2d.setColor(Color.cyan)
            obj._2.perimeter.foreach((p:Point) => {
              val s:String = (p.x - obj._2.centre.x) + "x" + (p.y - obj._2.centre.y)
              g2d.drawString(s, (p.x + 2), p.y)})
            //Draw collision
            if(ITestViewDrawIm.lastCollision != null){
              val p = ITestViewDrawIm.lastCollision.point
              g2d.setColor(Color.red)
              val cpc = new Ellipse2D.Double((p.x - 2), (p.y - 2), 5, 5)
              g2d.fill(cpc)
              val csb = ITestViewDrawIm.lastCollision.segmentBegin
              val cse = ITestViewDrawIm.lastCollision.segmentEnd
              g2d.setColor(Color.blue)
              g2d.drawLine(csb.x, csb.y,cse.x,cse.y)
              val bpc = new Ellipse2D.Double((cse.x - 2),(cse.y - 2), 4, 4)
              g2d.fill(bpc)
              val fsp = ITestViewDrawIm.lastCollision.firstSpeed
              val ssp = ITestViewDrawIm.lastCollision.secondSpeed
              g2d.setColor(Color.red)
              g2d.drawLine(p.x, p.y,fsp.x,fsp.y)
              g2d.drawLine(p.x, p.y,ssp.x,ssp.y)
              g2d.setColor(Color.black)
              g2d.drawString((p.x + "x" + p.y), (p.x - 22), (p.y - 5))  
              val fx = p.x - fsp.x; val fy = p.y - fsp.y
              val lfsp = (sqrt((fx * fx)+(fy * fy))).asInstanceOf[Int].toString              
              val sx = p.x - ssp.x; val sy = p.y - ssp.y
              val lssp = (sqrt((sx * sx)+(sy * sy))).asInstanceOf[Int].toString
              g2d.drawString(lfsp, (fsp.x - 10), (fsp.y + 12)) 
              g2d.drawString(lssp, (ssp.x - 10), (ssp.y + 12)) 
            } 
        })}
    g2d.dispose()
    //Show
    super.paintComponent(g)
    g.drawImage(buffer, 0, 0, this)
  }
  //Listeners 
  addComponentListener(new ComponentAdapter(){
    override def componentResized(e:ComponentEvent) = {
      val d = getSize()
      val w = if(d.width < 700){700}else{d.width}
      val h = if(d.height < 400){400}else{d.height}
      IViewSettingsEx.dimension.width = w; IViewSettingsEx.dimension.height = h
      IViewSettingsIm.foreach((item:(Component,IViewSettingsS)) => {
        item._2.resize(w, h)})}
    })   
  //Fields
  private var buffer:BufferedImage = null
  private var movement = true
  //Service code  
  connectto("IWidget",rootConName,rootConComponent) //Connect root interface    
  construction()
} 