package object VersionConcurrente {
  import Datos._
  import PlanificacionDeVuelos._
  import scala.collection.parallel.CollectionConverters._

  def encontrarTiempoMenorPar(iti: List[Itinerario]): Int ={
    val a= for{
      i <- (0 until iti.length).par
    } yield tiempoDeViaje(iti(i));
    a.min
  }

  def itinerarioPar(a1: String, a2: String):List[Itinerario]={
    def genDestDirecto(aeropuerto: String, vuelos: List[Vuelo]):Itinerario=vuelos.filter((p:Vuelo)=>p.Org.equals(aeropuerto))
    def genItinerario(airport: String, vuelos: List[Vuelo]):List[Itinerario]={
      if(vuelos.isEmpty) List(List())
      else{
        val itinerarioDirecto = genDestDirecto(airport, vuelos).par.map(f=>List(f)).toList
        val itinerariosConEscala = for {
          vueloP <- itinerarioDirecto.par.map(f => f.head)

          vueloD<- genItinerario(vueloP.Dst, vuelos.par.filterNot((p:Vuelo)=> p.Org.equals(airport)||p.Dst.equals(airport)).toList)
        }yield vueloP::vueloD
        itinerarioDirecto++itinerariosConEscala
      }
    }
    genItinerario(a1, vuelos).par.filter((p:List[Vuelo])=>p.last.Dst.equals(a2)).toList
  }

  def itinerariosTiempoPar(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    val iti=itinerarioPar(salida.Cod, llegada.Cod)
    def calcularTiemposMenores(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] ={
      def seleccionarAgregar(nuevoIti: List[Itinerario],i: List[Itinerario]): List[Itinerario] ={
        val agregar = i.par.filter(x=> tiempoDeViaje(x)==encontrarTiempoMenorPar(i)).toList
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
      val agregar = seleccionarAgregar(nuevoIti, itinerario)
      if(nuevoIti.length<3){
        calcularTiemposMenores(nuevoIti++agregar, itinerario.filterNot(x=> tiempoDeViaje(x)==encontrarTiempoMenorPar(itinerario))).par.toList
      }else{
        nuevoIti
      }
    }
    if(iti.length <= 3){
      //los unicos que hay
      iti
    }else{
      calcularTiemposMenores(Nil, iti).par.toList
    }
  }

  def itinerariosCambiosPar(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    val iti = itinerarioPar(salida.Cod, llegada.Cod)

    def encontrarElmenor( itinerario: List[Itinerario]): Int ={
      val masCortos= for{
        i <- 0 until itinerario.length
      } yield itinerario(i).length;
      masCortos.min
    }
    def getItinerariosCambios(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] = {
      /*
    * selecciona de un itinerario los que se pueden agregar al nuevo itinerario
    * */
      def seleccionarAgregar(nuevoIti: List[Itinerario],i: List[Itinerario]): List[Itinerario] ={
        val agregar = i.par.filter(x => x.length == encontrarElmenor(i)).toList
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
      val agregar = seleccionarAgregar(nuevoIti, itinerario)
      if(nuevoIti.length<3){
        getItinerariosCambios(nuevoIti++agregar, itinerario.par.filterNot(x=> x.length==encontrarElmenor(itinerario)).toList)
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


  def itinerariosDistaciaPar(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
    def calculeTiempoDeVuelo(vuelo: Vuelo): Int ={
      tiempo(vuelo.HS, vuelo.MS, vuelo.HL, vuelo.ML)
    }
    def tiempoEnElAire(iti: Itinerario): Int ={
      val tVuelo = for{
        i<- (0 until iti.length).par
      }yield calculeTiempoDeVuelo(iti(i))
      tVuelo.sum
    }
    def tiempoEnElAireMenor(iti: List[Itinerario]): Int ={
      val a= for{
        i <- (0 until iti.length).par
      } yield tiempoEnElAire(iti(i));
      a.min
    }


    def calcularTiemposMenores(nuevoIti: List[Itinerario],  itinerario: List[Itinerario]): List[Itinerario] ={
      val agregar = itinerario.par.filter(x=> tiempoEnElAire(x)==tiempoEnElAireMenor(itinerario))
      //revisar este condicional
      if(agregar.length >= 3){
        if(nuevoIti.length==0){
          agregar
        }else if(nuevoIti.length==1){
          agregar(0)++agregar(1)++nuevoIti
        }else if(nuevoIti.length==2){
          agregar(0)++nuevoIti
        }else if(nuevoIti.length==3){
          nuevoIti
        }
      }
      if(nuevoIti.length < 3){
        calcularTiemposMenores(nuevoIti++agregar, itinerario.par.filterNot(x=> tiempoEnElAire(x)==tiempoEnElAireMenor(itinerario)).toList)
      }else{
        nuevoIti
      }
    }
    val iti=itinerarioPar(salida.Cod, llegada.Cod)
    if(iti.length <= 3){
      iti
    }else{
      calcularTiemposMenores(Nil, iti)
    }
  }

  def itinerariosSalidaPar(salida: Aeropuerto, llegada: Aeropuerto , h: Int, m: Int): Itinerario ={
    val cita= horaAminuto(h,m)
    def llegadaDeItinerario(iti: Itinerario)= horaAminuto(iti.last.HL, iti.last.ML)
    //filtrar los que llegan antes de la hora de la cita
    val iti= itinerarioPar(salida.Cod, llegada.Cod).par.filter(x=> llegadaDeItinerario(x) <= cita).toList
    if(iti.length==0){
      //no hay itinerarios que lleguen antes de la hora de la cita
      Nil
    }else{
      iti.par.filter(x=> horaAminuto(x.head.HS, x.head.MS)==salidaMasTarde(iti)).head
    }
  }
}



