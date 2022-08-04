import PlanificacionDeVuelos._
import Datos._

//primea prueba
val salida = aeropuertos(12)
val llegada= aeropuertos(13)

itinerario(salida.Cod, llegada.Cod)
itinerariosTiempo(salida, llegada)
itinerariosTiempo(salida, llegada).length
itinerariosDistacia(aeropuertos(12), aeropuertos(13))
itinerario(salida.Cod, llegada.Cod)
itinerariosSalida(aeropuertos(12), aeropuertos(13),4, 30)


// segunda prueba
val salida2 = aeropuertos(0)
val llegada2= aeropuertos(1)
itinerario(salida2.Cod, llegada.Cod)
itinerariosTiempo(salida, llegada)
itinerariosTiempo(salida, llegada).length
itinerariosDistacia(salida2, llegada2)
itinerariosSalida(salida2, llegada2, 4, 30)