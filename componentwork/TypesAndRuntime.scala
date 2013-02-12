/****************************************************************** Description *******************************************************************************
Calling outside the component:
  connect(<First connected interface name>,<First component(handle)>,<Second connected interface name>,<Second component(handle)>)  //Connect two components, with different interface names
  connect(<Interface name(same)>,<First component(handle)>,<Second component(handle)>)                                              //Connect two components
  disconnect(<Disconnectable interface name(in disconnectable component)>,<Disconnectable component(handle)>)                       //Disconnect interface from other component(inerfaseName - name in disconnectable component)
  disconnect(<Disconnectable interface name(in disconnectable component)>,<Disconnectable component(multiconnection, handle)>,
             <Disconnect component(not multiconnection, handle)>)                                                                   //Disconnect interface from other component(used to the multiconnection)

Calling in the component:
  connectto(<First connected interface name>,<Second connected interface name>,<Second component(handle)>)  //Connect self to other components, with interface different names
  connectto(<Interface name(same)>,<Second component(handle)>)                                              //Connect self to other components
  disconnectoff(<Disconnectable interface name(self)>)                                                      //Disconnect self interface
  disconnectoff(<Disconnectable interface name(self)>,<Disconnect component(not multiconnection, handle)> ) //Disconnect self multiinterface  

Component template with root interface:
  class <prototype name> (val name:NamedComponents, rootConName:String, rootConComponent:Component) extends Component {
    //Interface <root interface name>(root)
    val <root interface name>Ex:<export type name>R = new <export type name>R; var <root interface name>Im:<import type name> = null   
    interfaces += ("<root interface name>" -> (<root interface name>Ex,"<export type name>","<import type name>", (c:Component, i:Interface) => {<root interface name>Im = i.asInstanceOf[<import type name>]}, 
                  (c:Component) => {<root interface name>Im = null; true}, false))    
    //Constructor/deconstructor 
    override def construction() = {
      super.construction()
      <code>  
    } 
    override def deconstruction() = { 
      super.deconstruction()
      <code> 
    } 
    //Interfaces export realization
    class <export type name>R extends <export type name> {
      override def connection(component:Component) = {
        <code> 
      }
      override def disconnection{
        <code> 
      } 
      <rest methods and fields>
    }
    //Internal methods and fields
      <definitions> 
    //Service code  (!!only in top level)
    connectto("<root interface name>",rootConName,rootConComponent) //Connect root interface
    construction()
 } 
   
Component template with multiinterface:
  class <prototype name>(val name:NamedComponents) extends Component {
    //Multiinterface <interface name>
    val <interface name>Ex:<export type name>R = new <export type name>R;  var <interface name>Im = MMap[Component, <import type name>]()
    interfaces += ("<interface name>" -> (<interface name>Ex,"<export type name>","<import type name>", (c:Component, i:Interface) => {<interface name>Im += (c -> i.asInstanceOf[<import type name>])}, 
                  (c:Component) => {<interface name>Im -= c; false}, true))
    //Constructor/deconstructor 
      <definitions>  
    //Interfaces export realization
      <definitions> 
    //Internal methods and fields
      <definitions> 
 } 
   
Component template:  
  class <prototype name>(val name:NamedComponents) extends Component {
    //Interface <interface name>
    val <interface name>Ex:<export type name>R = new <export type name>R; var <interface name>Im:<import type name> = null   
    interfaces += ("<interface name>" -> (<interface name>Ex,"<export type name>","<import type name>", (c:Component, i:Interface) => {<interface name>Im = i.asInstanceOf[<import type name>]}, 
                  (c:Component) => {<interface name>Im = null; true}, false))
    //Constructor/deconstructor 
      <definitions>  
    //Interfaces export realization
      <definitions> 
    //Internal methods and fields
      <definitions> 
    //Service code  (!!only in top level)
    construction()
  } 
   
Interface template: 
  class <Interface name>First extends Interface {
    <methods and fields>
  } 
  class <Interface name>Second extends Interface {
    <methods and fields>
    //Service code  (!!only in top level)
    construction()
  } 
      
Lists ontents:
  var interfaces  //MMap(Supported intefase name -> (Interfase object reference, Export section type  name, Import section type name, Connecn to function, Disconnecn to function, Is multinterface))
  var connections //MMap(Connected component -> List(Name self interface, Name connected interface, Connect interfase object reference))  
Exceptions numbers:
  0 - internal error
  1 - Component not exists
  2 - Not a component
  3 - Component don't have interface
  4 - Interface  already connected
  5 - Interfaces have a different types
  6 - Interface not connected
  7 - Interface is multiinterface, need specify jack handle 
*************************************************************************************************************************************************************/
package componentwork
import scala.collection.mutable.{Map => MMap}

//Class-parent to all class-components
trait Component {
  //Lists
  protected final var interfaces = MMap[String, (Interface, String, String, (Component,Interface) => Unit, (Component) => Boolean, Boolean)]() //Supported intefase list 
  /*private*/ final var connections =  MMap[Component,List[(String, String, Interface)]]()  //Connections list
  private final var existence = true //Component exist 
  protected final val thisComponent = this
  //Methods 
  protected final def connect(plugInerfName:String, plug:Component, jackInerfName:String, jack:Component) = { //Connect two components, with different interface names
    connectComponents(plugInerfName, plug, jackInerfName, jack)
  }  
  protected final def connect(inerfaseName:String, plug:Component, jack:Component) = { //Connect two components
    connectComponents(inerfaseName, plug, inerfaseName, jack)
  }  
  protected final def connectto(plugInerfName:String, jackInerfName:String, jack:Component) = { //Connect self to other components, with interface different names
    connectComponents(plugInerfName, this, jackInerfName, jack)
  }  
  protected final def connectto(inerfaseName:String, jack:Component) = { //Connect self to other components
    connectComponents(inerfaseName, this, inerfaseName, jack)
  }  
  protected final def disconnect(inerfaseName:String, plug:Component) = { //Disconnect interface from other component(inerfaseName - name in disconnectable component)  
    disconnectComponent(inerfaseName, plug, (getConComponent(plug, inerfaseName)), null)
  }  
  protected final def disconnect(inerfaseName:String, plug:Component,jack:Component) = { //Disconnect interface from other component(used to the multiconnection)   
    disconnectComponent(inerfaseName, plug, jack, null)
  }  
  protected final def disconnectoff(inerfaseName:String) = { //Disconnect interface
    disconnectComponent(inerfaseName, this, (getConComponent(this, inerfaseName)), null)
  }
  protected final def disconnectoff(inerfaseName:String, jack:Component) = { //Disconnect self multiinterface  
    disconnectComponent(inerfaseName, this,jack, null)
  }   
  protected final def selfdestruction() = { 
    destroyComponent(this)
  }
  protected final def destroyComponent(component:Component):Boolean = { 
    if(!(component.existence) || (component == null)){throw new ComponentException("Component not exists: " + component,1)}
    component.deconstruction()
    disconnectAll(component)
    component.existence = false                         
    return true                                            
  }
  //Can be override  
  protected def construction() = {}
  protected def deconstruction() = {} 
  //Functions
  private def connectComponents(plugInerfName:String, plug:Component, jackInerfName:String, jack:Component) = {  
    //Checking
    if(!(plug.isInstanceOf[Component])){throw new ComponentException("Plag not a component: " + plug,2)}
    if(!(jack.isInstanceOf[Component])){throw new ComponentException("Jack not a component: " + jack,2)}   
    if(!(plug.existence)){throw new ComponentException("Plag not exists: " + plug,1)}
    if(!(jack.existence)){throw new ComponentException("Jack not exists: " + jack,1)}
    if(!(plug.interfaces.contains(plugInerfName))){throw new ComponentException("Plag component: " + plug + " don't have interface: " + plugInerfName,3)}
    if(!(jack.interfaces.contains(jackInerfName))){throw new ComponentException("Jack component: " + jack + " don't have interface: " + plugInerfName,3)}
    if(plug.checkConnect(jack, plugInerfName)){throw new ComponentException("Plag interface: " + plugInerfName + " of: " + plug + " already connected to: " + jackInerfName+ " of: " + jack,4)}
    if(jack.checkConnect(plug, jackInerfName)){throw new ComponentException("Jack interface: " + jackInerfName + " of: " + jack + " already connected to: " + plugInerfName + " of: " + plug,4)}    
    val jackInterfDisc = jack.interfaces.get(jackInerfName).get; val plugInterfDisc = plug.interfaces.get(plugInerfName).get //get -> (Interface, String, String, (Component,Interface) => Unit, (Component) => Boolean)    
    if((plugInterfDisc._2 != jackInterfDisc._3) || (plugInterfDisc._3 != jackInterfDisc._2)){
      throw new ComponentException("Interface: " + plugInerfName + "(" + plugInterfDisc._2 + "|" + plugInterfDisc._3 + ") and " + jackInerfName + "(" + jackInterfDisc._2 + "|" + jackInterfDisc._3 + ") have a different types",5)}
    //Connect
    plugInterfDisc._4(jack,jackInterfDisc._1); jackInterfDisc._4(plug,plugInterfDisc._1)
    plug.addConnect(jack, plugInerfName, jackInerfName, jackInterfDisc._1); jack.addConnect(plug, jackInerfName, plugInerfName, plugInterfDisc._1)
    plugInterfDisc._1.connection(jack); jackInterfDisc._1.connection(plug)
  }   
  private def disconnectComponent(plugInerfName:String, plug:Component, jack:Component, nodesr:Component) = {   
    //Checking
    if(!(plug.isInstanceOf[Component])){throw new ComponentException("Plag not a component: " + plug,2)}
    if(!(jack.isInstanceOf[Component])){throw new ComponentException("Jack not a component: " + jack,2)}
    if(!(plug.existence)){throw new ComponentException("Plag not exists: " + plug,1)}
    if(!(jack.existence)){throw new ComponentException("Jack not exists: " + jack,1)}
    if(!(plug.interfaces.contains(plugInerfName))){throw new ComponentException("Plag component: " + plug + " don't have interface: " + plugInerfName,3)}
    val jackConnect = plug.getConnect(jack, plugInerfName)  //:(String, Interface)
    if(jackConnect == (null,null)){throw new ComponentException("Interface: " + plugInerfName + " of plag: " + jack + " not connected",6)}
    val plugConnect = jack.getConnect(plug, jackConnect._1)  //:(String, Interface)
    if(plugConnect == (null,null)){throw new ComponentException("Connection list is a damaged 4",0)}
    //Disconnect
    plugConnect._2.disconnection(jack);  jackConnect._2.disconnection(plug)
    plug.delConnect(jack,plugInerfName); jack.delConnect(plug,jackConnect._1) 
    val pd = plug.interfaces(plugConnect._1)._5(jack); val jd = jack.interfaces(jackConnect._1)._5(plug) 
    if((pd == true) && (plug != nodesr)){destroyComponent(plug)}; if((jd == true) && (plug != nodesr)){destroyComponent(jack)}  
  }   
  private def getConComponent(plug:Component, plugInterName:String):Component = { //Return: jack 
    //Check
    if(!(plug.isInstanceOf[Component])){throw new ComponentException("Plag not a component: " + plug,2)}
    if(!(plug.existence)){throw new ComponentException("Plag not exists: " + plug,1)}
    if(!(plug.interfaces.contains(plugInterName))){throw new ComponentException("Plag component: " + plug + " don't have interface: " + plugInterName,5)} 
    if(plug.interfaces(plugInterName)._6){throw new ComponentException("Plag interface: " + plugInterName + " of: " + plug + " is multiinterface, need specify jack handle",7)}
    //Search jack
    var c:Component = null
    plug.connections.foreach((mitem:(Component, List[(String, String, Interface)])) => {
      val l = mitem._2.length
      if(l > 1){         
        mitem._2.foreach( (litem:(String, String, Interface)) => {
          if (litem._1 == plugInterName){
            if(c != null){throw new ComponentException("Connection list is a damaged 1",0)}
            c = mitem._1}})}
      else{ 
        if(l == 0){throw new ComponentException("Connection list is a damaged 2",0)}
        if(mitem._2(0)._1 == plugInterName){
          if(c != null){throw new ComponentException("Connection list is a damaged 3",0)}
           c = mitem._1}}})
    if(c == null){throw new ComponentException("Plag interfase: " + plugInterName + " of: " + plug + " not connected",6)} 
    return c
  }
  private def disconnectAll(plug:Component) = { //Disconnecrion all self interfaces 
    while(plug.connections.size != 0){
      var el = plug.connections.head
      while((el._2).length > 1){
        disconnectComponent((el._2(0))._1, plug, (el._1),plug)
        el = plug.connections.head}
     disconnectComponent((el._2(0))._1, plug, (el._1),plug)} 
  }
  //Subfunctions
  private def addConnect(jack:Component, plugInterName:String, jackInterName:String, jackInterface:Interface) = {
    if (connections.contains(jack)){
      connections(jack) +:= (plugInterName, jackInterName,jackInterface)}
    else{
      connections += (jack -> List[(String, String, Interface)]((plugInterName, jackInterName, jackInterface)))}
  }
  private def delConnect(jack:Component, plugInterName:String) = {
    if (connections(jack).length > 1){
      var i = 0; while( connections(jack)(i)._1 != plugInterName){ i += 1 }
      val (l1, l2) = connections(jack).splitAt(i)
      connections(jack) =  l1 ::: (l2 drop 1)}
    else{
      connections -= jack}
  }
  private def getConnect(jack:Component, plugInterName:String):(String, Interface) = { //Return: jackInterName & jackInretface, if not found, return (null,null)
    if (!(connections.contains(jack))){return (null,null)}
    val l = connections(jack).length
    if(l > 1){
      var i = 0; var f = true; while(f && (i < l)){f = connections(jack)(i)._1 != plugInterName; if(f){i += 1}}
      if (f){return (null,null)}
      return (connections(jack)(i)._2, connections(jack)(i)._3)} 
    else{
      return (connections(jack)(0)._2, connections(jack)(0)._3)}
  }
  private def checkConnect(jack:Component, plugInterName:String):Boolean = { //Return true when connect exist
    if(connections.contains(jack)){
      val l = connections(jack).length
      if(l > 1){
        var i = 0; var f = true; while(f && (i < l)){f = connections(jack)(i)._1 != plugInterName; if(f){i += 1}}
        return !f}
      else{
        if(connections(jack)(0)._1 == plugInterName){return true}}}
    else{
      if(interfaces(plugInterName)._6 == false){  //If not multiinterface
        var f = false
        connections.foreach((mitem:(Component, List[(String, String, Interface)])) => {
          if(plugInterName == mitem._2(0)._1){
            if(f == true){throw new ComponentException("Connection list is a damaged 5",0)}
              f = true}})
        return f}}
    return false
  }
 }

//Class-parent to all class-interfaces
class Interface {
  //Methods, can be override
  def connection (component:Component) = {}
  def disconnection (component:Component) = {}
}

//Exception type
class ComponentException(val sting:String, val number:Int) extends Exception(sting) {}  
