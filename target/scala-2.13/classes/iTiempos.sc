import Datos.{Aeropuerto, aeropuertos}
import PlanificacionDeVuelos.{Itinerario, encontrarTiempoMenor, itinerario, tiempoDeViaje}

/*def itinerariosTiempo(salida: Aeropuerto, llegada: Aeropuerto): List[Itinerario] ={
  val iti=itinerario(salida.Cod, llegada.Cod)
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
}*/

def calcularTiemposMenores(nuevoIti: List[Itinerario], itinerario: List[Itinerario]): List[Itinerario] ={
  val agregar = itinerario.filter(x=> tiempoDeViaje(x)==encontrarTiempoMenor(itinerario))

  if(nuevoIti.length<3){
    calcularTiemposMenores(nuevoIti++agregar, itinerario.filterNot(x=> tiempoDeViaje(x)==encontrarTiempoMenor(itinerario)))
  }else{
    nuevoIti
  }/*
  if(nuevoIti.length==2){
    agregar++nuevoIti
  }else{
    calcularTiemposMenores(nuevoIti++agregar, itinerario.filterNot(x=> tiempoDeViaje(x)==encontrarTiempoMenor(itinerario)))
  }*/
}



//primea prueba
val salida = aeropuertos(0)
val llegada= aeropuertos(5)

val iti = itinerario(salida.Cod, llegada.Cod)
calcularTiemposMenores(Nil, iti)
itinerario(salida.Cod, llegada.Cod).length
itinerario(salida.Cod, llegada.Cod).tail.tail.tail

//itinerariosTiempo(salida, llegada)
//itinerariosTiempo(salida, llegada).length
