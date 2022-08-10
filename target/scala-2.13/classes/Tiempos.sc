import VersionConcurrente._
import datos2._
import PlanificacionDeVuelos._
import org.scalameter.{Key, Warmer, config}

val standardConfig=config(
  Key.exec.minWarmupRuns := 20 ,
  Key.exec.maxWarmupRuns := 40 ,
  Key.exec.benchRuns := 25 ,
  Key.verbose := false
) withWarmer Warmer . Default ( )

val salida = aeropuertos2(0)
val llegada= aeropuertos2(1)

val iti = itinerario(salida.Cod, llegada.Cod)

val Seq = standardConfig measure {
  itinerario(salida.Cod,llegada.Cod)

  itinerario(salida.Cod, llegada.Cod).length

  itinerariosTiempo(salida, llegada)
  itinerariosTiempo(salida, llegada).length

  itinerariosCambios(salida, llegada)
  itinerariosCambios(salida, llegada).length

  itinerariosDistacia(salida, llegada)
  itinerariosDistacia(salida, llegada).length

}
val Par = standardConfig measure {
  val iti = itinerarioPar(salida.Cod, llegada.Cod)

  itinerarioPar(salida.Cod, llegada.Cod).length

  itinerariosTiempoPar(salida, llegada)
  itinerariosTiempoPar(salida, llegada).length

  itinerariosCambiosPar(salida, llegada)
  itinerariosCambiosPar(salida, llegada).length

  itinerariosDistaciaPar(salida, llegada)
  itinerariosDistaciaPar(salida, llegada).length
}

val funcionItinerarioSec = standardConfig measure {
  itinerario(salida.Cod, llegada.Cod).length
}

val funcionItinerarioPar = standardConfig measure {
  itinerarioPar(salida.Cod, llegada.Cod).length
}




