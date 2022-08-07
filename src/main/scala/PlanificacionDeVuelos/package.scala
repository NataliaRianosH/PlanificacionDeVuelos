import PlanificacionDeVuelos.itinerario

package object PlanificacionDeVuelos {
  import Datos._
  type Itinerario= List[Vuelo]
  /*
  * FUNCIONES AUXILIARES
  * */
  def horaAminuto(h: Int, m:Int): Int = {
    (h * 60) + m
  }
  def tiempo(HS: Int, MS: Int, HL:Int, ML:Int): Int ={
    val salida= horaAminuto(HS, MS)
    val llegada =horaAminuto(HL, ML)
    if(salida<llegada){
      llegada-salida
    }else{
      1440-(salida-llegada)
    }
  }
  def tiempoDeViaje(iti: Itinerario): Int ={
    if(iti.length==0){
      0
    }else if(iti.length==1){
      tiempo(iti(0).HS, iti(0).MS, iti(0).HL, iti(0).ML)
    }else{
      tiempo(iti.head.HS, iti.head.MS, iti.last.HL, iti.last.ML)
    }
  }
  def encontrarTiempoMenor(iti: List[Itinerario]): Int ={
    val a= for{
      i <- 0 until iti.length
    } yield tiempoDeViaje(iti(i));
    a.min
  }
  def salidaMasTarde(iti: List[Itinerario]): Int ={
    val salidas= for{
      i <- 0 until iti.length
    } yield horaAminuto(iti(i).head.HS,iti(i).head.MS);
    salidas.max
  }
  def seleccionarAgregar(nuevoIti: List[Itinerario],i: List[Itinerario], paraFiltrar: Itinerario=>Int, paraComparar: List[Itinerario]=>Int): List[Itinerario] ={
    val agregar = i.filter(x=> paraFiltrar(x)==paraComparar(i))
    if(agregar.length==0){
      List(Nil)
    }else if(agregar.length==1){
      agregar
    }else if(agregar.length ==2){
      if(nuevoIti.length==0){
        agregar(0)::List(agregar(1))
      }else if(nuevoIti.length==1){
        List(agregar(0))
      }else{
        List(Nil)
      }
    }else {
      if(nuevoIti.length==0){
        agregar(0)::(agregar(1))::List(agregar(2))
      }else if(nuevoIti.length==1){
        agregar(0)::List(agregar(1))
      }else if(nuevoIti.length==2){
        List(agregar(0))
      }else {
        List(Nil)
      }
    }
  }

  /**
   * 1 ENCONTRANDO RUTAS
   * dados dos aeropuertos devuelve la lista de itinerarios posibles para viajar entre esos aeropuertos
   * */
  def itinerario(a1: String, a2: String):List[Itinerario]={
    def genDestDirecto(aeropuerto: String, vuelos: List[Vuelo]):Itinerario=vuelos.filter((p:Vuelo)=>p.Org.equals(aeropuerto))
    def genItinerario(airport: String, vuelos: List[Vuelo]):List[Itinerario]={
      if(vuelos.isEmpty) List(List())
      else{
        val itinerarioDirecto = genDestDirecto(airport, vuelos).map(f=>List(f))
        val itinerariosConEscala = for {
          vueloP <- itinerarioDirecto.map(f => f.head)
          vueloD<- genItinerario(vueloP.Dst, vuelos.filterNot((p:Vuelo)=> p.Org.equals(airport)||p.Dst.equals(airport)))
        }yield vueloP::vueloD
        itinerarioDirecto++itinerariosConEscala
      }
    }
    genItinerario(a1, vuelos).filter((p:List[Vuelo])=>p.last.Dst.equals(a2))
  }

  /**
   *2. MINIMIZACIÓN DE TIEMPO TOTAL DE VIAJE
   * dados dos aeropuertos, encuentra al menos 3 itinerarios que correspondadn a los menores tiempos de viaje
   * teniendo en cuenta el tiempo de espera en tierra
   * */
  def itinerariosTiempo(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    val iti=itinerario(salida.Cod, llegada.Cod)
    def calcularTiemposMenores(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] ={
      val agregar = seleccionarAgregar(nuevoIti, itinerario, tiempoDeViaje,encontrarTiempoMenor)
      if(nuevoIti.length<3){
        calcularTiemposMenores(nuevoIti++agregar, itinerario.filterNot(x=> tiempoDeViaje(x)==encontrarTiempoMenor(itinerario)))
      }else{
        nuevoIti
      }
    }
    if(iti.length <= 3){
      iti
    }else{
      calcularTiemposMenores(Nil, iti)
    }
  }

  /**
   *3.MINIMIZACIÓN DE CAMBIOS DE AVIÓN
   * dados dos aeropuertos, encuentra al menos 3 itinerarios que hacen el menor numero de cambios de avión
   * sin tener en cuenta el tiempo total de viaje
   * */
  def itinerariosCambios(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    val iti = itinerario(salida.Cod, llegada.Cod)
    def encontrarElmenor( itinerario: List[Itinerario]): Int ={
      val masCortos= for{
        i <- 0 until itinerario.length
      } yield itinerario(i).length;
      masCortos.min
    }
    def getItinerariosCambios(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] = {
      /**
       * selecciona de un itinerario los que se pueden agregar al nuevo itinerario
       * */
      def tamaño(itinerario: Itinerario): Int ={
        itinerario.length
      }
      val agregar = seleccionarAgregar(nuevoIti, itinerario, tamaño, encontrarElmenor)
      if(nuevoIti.length<3){
        getItinerariosCambios(nuevoIti++agregar, itinerario.filterNot(x=> x.length==encontrarElmenor(itinerario)))
      }else{
        nuevoIti
      }
    }
    if(iti.length <=3){
      iti
    }else{
      getItinerariosCambios(Nil,iti)
    }
  }
  /**
   * 4. MINIMIZACIÓN DE LA DISTANCIA RECORRIDA
   * dados dos aeropuertos  encuentra al menos 3 itinerarios que
   * que minimicen el tiempo de vuelo, sin tener en cuenta el tiempo total de viaje
   * */
  def itinerariosDistacia(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    def calculeTiempoDeVuelo(vuelo: Vuelo): Int ={
      tiempo(vuelo.HS, vuelo.MS, vuelo.HL, vuelo.ML)
    }
    def tiempoEnElAire(iti: Itinerario): Int ={
      val tVuelo = for{
        i<- 0 until iti.length
      }yield calculeTiempoDeVuelo(iti(i))
      tVuelo.sum
    }
    def tiempoEnElAireMenor(iti: List[Itinerario]): Int ={
      val a= for{
        i <- 0 until iti.length
      } yield tiempoEnElAire(iti(i));
      a.min
    }
    def calcularTiemposMenores(nuevoIti: List[Itinerario],  itinerario: List[Itinerario]): List[Itinerario] ={
      val agregar = seleccionarAgregar(nuevoIti, itinerario, tiempoEnElAire, tiempoEnElAireMenor)
      if(nuevoIti.length < 3){
        calcularTiemposMenores(nuevoIti++agregar, itinerario.filterNot(x=> tiempoEnElAire(x)==tiempoEnElAireMenor(itinerario)))
      }else{
        nuevoIti
      }
    }
    val iti=itinerario(salida.Cod, llegada.Cod)
    if(iti.length <= 3){
      iti
    }else{
      calcularTiemposMenores(Nil, iti)
    }
  }
  /**
   *5. OPTIMIZACIÓN DE LA HORA DE SALIDA
   * dados dos aeropuertos , determine el itinerario tal que la hora de salida a1,
   * sea la hora más tarde posible para salir del aeropuero y llegar a tiempo a la cita
   * */
  def itinerariosSalida(salida: Aeropuerto, llegada: Aeropuerto , h: Int, m: Int): Itinerario ={
    val cita= horaAminuto(h,m)
    def llegadaDeItinerario(iti: Itinerario)= horaAminuto(iti.last.HL, iti.last.ML)
    //filtrar los que llegan antes de la hora de la cita
    val iti= itinerario(salida.Cod, llegada.Cod).filter(x=> llegadaDeItinerario(x) <= cita)
    if(iti.length==0){
      //no hay itinerarios que lleguen antes de la hora de la cita
      Nil
    }else{
      iti.filter(x=> horaAminuto(x.head.HS, x.head.MS)==salidaMasTarde(iti)).head
    }
  }
}
