package componentswing
import componentwork._,      
       javax.swing.JPanel,
       java.awt.{Component => SC,BorderLayout,Dimension,Graphics,Graphics2D,Color,Point,Image,Toolkit},
       java.awt.event.{ComponentAdapter,ComponentEvent},
       java.awt.geom.Ellipse2D,
       java.awt.image.{BufferedImage, AreaAveragingScaleFilter,FilteredImageSource},
       scala.collection.mutable.{Map => MMap},
       scala.math.sqrt

       
class ViewDraw (val name:MMap[String, Component], rootConName:String, rootConComponent:Component) extends JPanel with Component {
  //Interface "IWidget"(root)
  val IWidgetEx:IWidgetSR = new IWidgetSR; var IWidgetIm:IWidgetF = null   
  interfaces += ("IWidget" -> (IWidgetEx,"IWidgetS","IWidgetF", (c:Component, i:Interface) => {IWidgetIm = i.asInstanceOf[IWidgetF]}, (c:Component) => {IWidgetIm = null; true}, false))
  //Interface "IViewDraw" 
  val IViewDrawEx:IViewDrawFR = new IViewDrawFR; var IViewDrawIm:IViewDrawS = null   
  interfaces += ("IViewDraw" -> (IViewDrawEx,"IViewDrawF","IViewDrawS", (c:Component, i:Interface) => {IViewDrawIm = i.asInstanceOf[IViewDrawS]}, (c:Component) => {IViewDrawIm = null; false}, false))
  //Multiinterface "IViewSettings" 
  val IViewSettingsEx:IViewSettingsFR = new IViewSettingsFR;  var IViewSettingsIm = MMap[Component, IViewSettingsS]()
  interfaces += ("IViewSettings" -> (IViewSettingsEx,"IViewSettingsF","IViewSettingsS", (c:Component, i:Interface) => {IViewSettingsIm += (c -> i.asInstanceOf[IViewSettingsS])}, (c:Component) => {IViewSettingsIm -= c; false}, true))
  //Interfaces export realization
  class IWidgetSR  extends IWidgetS 
  class IViewDrawFR extends IViewDrawF {
    override def refresh() = {thisComponent.asInstanceOf[ViewDraw].repaint()}
  }
  class IViewSettingsFR extends IViewSettingsF {
    override def setBackground(img:Image) = {
      backgroundImg = img
      val asf = new AreaAveragingScaleFilter(IViewSettingsEx.dimension.width, IViewSettingsEx.dimension.height)
      scalingBackgroundImg = toolKit.createImage(new FilteredImageSource(img.getSource(), asf)) 
      thisComponent.asInstanceOf[ViewDraw].repaint()
    }      
    override def disconnection(c:Component) = {
      backgroundImg = null; scalingBackgroundImg = null
    }  
    override def connection(c:Component) = {
      IViewSettingsIm(c).resize(IViewSettingsEx.dimension.width, IViewSettingsEx.dimension.height)
    }  
  }
  //Self-assembly
  setPreferredSize(new Dimension(700,400)); IViewSettingsEx.dimension.width = 700; IViewSettingsEx.dimension.height = 400
  setBackground(Color.white)
  IWidgetEx.component = thisComponent.asInstanceOf[ViewDraw]
  IWidgetEx.position = BorderLayout.CENTER 
  //Override methods
  override def paintComponent(g:Graphics) = {
    //Make buffer
    val d = getSize(); val (w,h) = (d.width, d.height)
    var f = false; if(buffer == null){f = true}else{if((buffer.getWidth(),buffer.getHeight()) != (w,h)){f = true }}
    if(f){
      buffer = new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_ARGB)}
    val g2d:Graphics2D = buffer.createGraphics()
    //Draw background  
    if(backgroundImg == null){
      g2d.setColor(Color.white) 
      g2d.fillRect(0,0, d.width, d.height)} 
    else{
      g2d.drawImage(scalingBackgroundImg, 0, 0,Color.white,null)}
    //Draw textures
    if(IViewDrawIm != null){
      IViewDrawIm.objects.foreach((obj:(Component,ObjTexture)) => {  
        val ot = obj._2
        if(ot.texture != null){
          g2d.drawImage(ot.texture, ot.coordinates.x, ot.coordinates.y, tanspatentColor, null)}})}
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
      IViewSettingsIm.foreach((item:(Component,IViewSettingsS)) => {item._2.resize(w, h)})
      if(backgroundImg != null){
        val asf = new AreaAveragingScaleFilter(w, h)
        scalingBackgroundImg = toolKit.createImage(new FilteredImageSource(backgroundImg.getSource(), asf))} 
    }
  })   
  //Fields
  private var buffer:BufferedImage = null
  private var movement = true
  private var scalingBackgroundImg:Image = null
  private var backgroundImg:Image = null  
  private val toolKit = Toolkit.getDefaultToolkit() 
  private val tanspatentColor = new Color(255, 255, 255, 0)
  //Service code  
  connectto("IWidget",rootConName,rootConComponent) //Connect root interface    
  construction()
} 
      
      
      
      
      
      
      
      
      

