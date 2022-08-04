package object PlanificacionDeVuelos {
  import Datos._
  type Itinerario= List[Vuelo]
  /**
   * ENCONTRANDO RUTAS
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
    def tiempo(HS: Int, MS: Int, HL:Int, ML:Int): Int ={
      val salida= (HS * 60) + MS
      val llegada =(HL * 60) + ML
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
    def calcularTiemposMenores(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] ={
      val agregar = itinerario.filter(x=> tiempoDeViaje(x)==encontrarTiempoMenor(itinerario))
      if(nuevoIti.length==2){
        agregar++nuevoIti
      }else{
        calcularTiemposMenores(nuevoIti++agregar, itinerario.filterNot(x=> tiempoDeViaje(x)==encontrarTiempoMenor(itinerario)))
      }
    }
    if(iti.length <= 1){
      //los unicos que hay
      iti
    }else{
      calcularTiemposMenores(Nil, iti)
    }
  }
  /**
   *MINIMIZACIÓN DE CAMBIOS DE AVIÓN
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
    def getItinerariosCambios(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] ={
      val agregar= itinerario.filter(x=> x.length == encontrarElmenor(itinerario))
      if(nuevoIti.length==2){
        agregar++nuevoIti
      }else{
        getItinerariosCambios(nuevoIti++agregar, itinerario.filterNot(x=> x.length==encontrarElmenor(itinerario)))
      }
    }
    //mirar la cantidad de vuelos que tiene un itinerario
    if(iti.length<=3){
      iti
    }else{
      getItinerariosCambios(Nil,iti)
    }
  }


  /**
   *MINIMIZACIÓN DE LA DISTANCIA RECORRIDA
   * dados dos aeropuertos  encuentra al menos 3 itinerarios que minimicen itinerarios
   * que minimicen el tiempo de vuelo, sin tener en cuenta el tiempo total de viaje
   * */

  def itinerariosDistacia(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    def tiempo(HS: Int, MS: Int, HL:Int, ML:Int): Int ={
      val salida= (HS * 60) + MS
      val llegada =(HL * 60) + ML
      if(salida<llegada){
        llegada-salida
      }else{
        1440-(salida-llegada)
      }
    }
    def calculeTiempoDeVuelo(vuelo: Vuelo): Int ={
      tiempo(vuelo.HS, vuelo.MS, vuelo.HL, vuelo.ML)
    }

    def tiempoEnElAire(iti: Itinerario): Int ={
      val tVuelo = for{
        i<- 0 until iti.length
      }yield calculeTiempoDeVuelo(iti(i))
      tVuelo.sum
    }

    def encontrarTiempoMenor(iti: List[Itinerario]): Int ={
      val a= for{
        i <- 0 until iti.length
      } yield tiempoEnElAire(iti(i));
      a.min
    }
    def calcularTiemposMenores(nuevoIit: List[Itinerario],  itinerario: List[Itinerario]): List[Itinerario] ={
      val agregar = itinerario.filter(x=> tiempoEnElAire(x)==encontrarTiempoMenor(itinerario))

      if(nuevoIit.length==2){
        agregar++nuevoIit
      }else{
        calcularTiemposMenores(nuevoIit++agregar, itinerario.filterNot(x=> tiempoEnElAire(x)==encontrarTiempoMenor(itinerario)))
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
   *OPTIMIZACIÓN DE LA HORA DE SALIDA
   * dados dos aeropuertos , determine el itinerario tal que la hora de salida a1,
   * sea la hora más tarde posible para salir del aeropuero y llegar a tiempo a la cita
   * */
  def itinerariosSalida(salida: Aeropuerto, llegada: Aeropuerto , h: Int, m: Int): Itinerario ={

    def horaAminuto(h: Int, m:Int): Int = {
      (h * 60) + m
    }
    val cita= horaAminuto(h,m)
    def llegadaDeItinerario(iti: Itinerario)= horaAminuto(iti.last.HL, iti.last.ML)
    //filtrar los que llegan antes de la hora de la cita
    val iti= itinerario(salida.Cod, llegada.Cod).filter(x=> llegadaDeItinerario(x) >= cita)
    if(iti.length==0){
      //no hay itinerarios que lleguen antes de la hora de salida
      Nil
    }else{
      //el itinerario que sale más tarde es el que demora menos
      itinerariosTiempo(salida, llegada).head
    }

  }
}
