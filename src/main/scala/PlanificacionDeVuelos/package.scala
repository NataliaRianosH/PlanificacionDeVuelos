package object PlanificacionDeVuelos {
  import Datos._
  type Itinerario= List[Vuelo]
  /**
   * ENCONTRANDO RUTAS
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
   *MINIMIZACIÓN DE TIEMPO TOTAL DE VIAJE
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
    getItinerariosCambios(Nil,iti)
  }

  /**
   *MINIMIZACIÓN DE LA DISTANCIA RECORRIDA
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
    calcularTiemposMenores(Nil, iti)
  }

  /**
   *OPTIMIZACIÓN DE LA HORA DE SALIDA
   * */
  def itinerariosSalida(salida: Aeropuerto, llegada: Aeropuerto , h: Int, m: Int): Itinerario ={
    def horaAminuto(h: Int, m:Int): Int = {
      (h * 60) + m
    }
    def tiempoDeEspera(itinerario: Itinerario): Int = {
      val salir = horaAminuto(h,m)
      val sale = horaAminuto(itinerario.head.HS, itinerario.head.MS)
      sale-salir
    }
    def esperaMenor(itinerario: List[Itinerario]): Int ={
      val espera = for{
        i <- itinerario.indices
      } yield tiempoDeEspera( itinerario(i));
      espera.min
    }
    //encontrar la hora más cercana
    val iti= itinerario(salida.Cod, llegada.Cod).filter(x=>  horaAminuto(h,m) <= horaAminuto(x.head.HS, x.head.MS))
    // escribir las horas de salida en minutos
    iti.filter(x=> tiempoDeEspera(x) == esperaMenor(iti)).head
  }


}
