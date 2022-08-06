import PlanificacionDeVuelos._
import Datos._

//primea prueba
val salida = aeropuertos(0)
val llegada= aeropuertos(6)
val iti = itinerario(salida.Cod, llegada.Cod)

itinerario(salida.Cod, llegada.Cod).length

itinerariosTiempo(salida, llegada)
itinerariosTiempo(salida, llegada).length

itinerariosCambios(salida, llegada)
itinerariosCambios(salida, llegada).length

itinerariosDistacia(salida, llegada)
itinerariosDistacia(salida, llegada).length

val cita=(17,30)
val h=cita._1
val m=cita._2

itinerariosSalida(salida, llegada, h,m)







