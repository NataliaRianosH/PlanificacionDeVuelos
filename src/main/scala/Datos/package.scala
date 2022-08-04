package object Datos {
  case class Aeropuerto(Cod:String,X:Int,Y:Int,GMT:Int)
  //codigo del aeropuerto, coordenada  y Y, franja horaria
  case class Vuelo(Aln:String,  Num:Int  , Org:String,              HS:Int,MS:Int,      Dst:String,               HL:Int,ML:Int, Esc:Int)
  // aerolinea,  numDelVuelo, CodDelAeropuertoDeOrigen, HoraMinutosSalina, codigoAeropuertoDeLlegada, horaLlegada,  numDeEscalas
  val aeropuertos=List(
    Aeropuerto("A", 195, 275, -800),
    Aeropuerto("B", 470, 280, -600),
    Aeropuerto("C", 430, 240, -700),
    Aeropuerto("D", 590, 100, -600),
    Aeropuerto("E", 540, 180, -600),
    Aeropuerto("F", 540, 180, -600),
    Aeropuerto("G", 540, 180, -600),
    Aeropuerto("H", 540, 180, -600),
    Aeropuerto("I", 540, 180, -600),
    Aeropuerto("J", 540, 180, -600),
    Aeropuerto("K", 540, 180, -600),
    Aeropuerto("L", 540, 180, -600),
    Aeropuerto("M", 540, 180, -600),
    Aeropuerto("N", 540, 180, -600),

  )
  val vuelos = List(
    Vuelo("a1", 373, "B", 8, 10, "C", 9, 10, 1),
    Vuelo("a2", 373, "C", 16, 0, "D", 17, 0, 1),
    Vuelo("a3", 373, "A", 6, 0, "B", 8, 0, 1),
    Vuelo("a4", 256, "A", 5, 10, "E", 6, 10, 0),
    Vuelo("a5", 256, "E", 6, 20, "F", 7, 20, 0),
    Vuelo("a6", 403, "B", 13, 0, "D", 14, 0, 0),
    Vuelo("a7", 403, "M", 1, 30, "N", 6, 30, 0),
    Vuelo("a8", 403, "M", 4, 0, "A", 5, 0, 0),
    Vuelo("a9", 403, "C", 10, 0, "N", 11, 0, 0),
    Vuelo("a10", 403, "N", 13, 0, "D", 14, 0, 0),
    Vuelo("a11", 403, "M", 5, 0, "E", 6, 0, 0),
    Vuelo("a11", 403, "F", 8, 0, "H", 9, 30, 0),
    Vuelo("a11", 403, "H", 10, 0, "L", 12, 0, 0),
    Vuelo("a11", 403, "L", 12, 10, "N", 13, 0, 0),
  )
}
