import Datos.Vuelo

package object Datos {
  case class Aeropuerto(Cod:String,X:Int,Y:Int,GMT:Int)
  //codigo del aeropuerto, coordenada  y Y, franja horaria
  case class Vuelo(Aln:String,  Num:Int  , Org:String,              HS:Int,MS:Int,      Dst:String,               HL:Int,ML:Int, Esc:Int)
  // aerolinea,  numDelVuelo, CodDelAeropuertoDeOrigen, HoraMinutosSalina, codigoAeropuertoDeLlegada, horaLlegada,  numDeEscalas
  val aeropuertos=List(
    Aeropuerto("A", 195, 275, -800),
    Aeropuerto("B", 470, 280, -600),
    Aeropuerto("C", 430, 240, -300),
    Aeropuerto("D", 590, 302, -200),
    Aeropuerto("E", 560, 180, -300),
    Aeropuerto("F", 770, 981, -600),
    Aeropuerto("G", 940, 460, -500),
    Aeropuerto("H", 150, 784, -800),
    Aeropuerto("I", 540, 282, -600),
    Aeropuerto("J", 510, 491, -900),
    Aeropuerto("K", 540, 173, -500),
    Aeropuerto("L", 920, 402, -200),
    Aeropuerto("M", 110, 270, -600),
    Aeropuerto("N", 590, 600, -300),

  )
  val vuelos = List(
    Vuelo("a1", 372, "A", 6, 0, "B", 8, 0, 1),
    Vuelo("a2", 245, "B", 9, 0, "C", 9, 30, 1),
    Vuelo("a3", 835, "C", 10, 0, "D", 13, 0, 1),
    Vuelo("a4", 356, "D", 14, 0, "F", 17, 10, 1),
    Vuelo("a5", 752, "E", 17, 40, "F", 18, 0, 1),
    Vuelo("a6", 853, "F", 20, 0, "H", 22, 40, 1),
    Vuelo("a7", 138, "G", 9, 0, "F", 12, 0, 1),
    Vuelo("a8", 568, "A", 5, 0, "C", 7, 30, 1),
    Vuelo("a9", 568, "B", 12, 0, "D", 13, 30, 1),
    Vuelo("a10", 332, "C", 9, 40, "H", 12, 0, 1),
    Vuelo("a11", 368, "D", 16, 0, "H", 17, 0, 1),
    Vuelo("a12", 482, "A", 15, 0, "B", 16, 0, 1),
    Vuelo("a13", 531, "C", 11, 0, "D", 13, 0, 1),

  )
}
