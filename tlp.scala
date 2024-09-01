
case class Obra(autor: String, año: Int, descripcion: String, tipologia: String, precio: Double)

// Lista de inventario del museo
val inventario: List[Obra] = List(
  Obra("Autor A", 1995, "Descripción 1", "Terror", 5000),
  Obra("Autor B", 1980, "Descripción 2", "Ficcion", 15000),
  Obra("Autor C", 2005, "Descripción 3", "Pintura", 7000),
  Obra("Autor D", 1975, "Descripción 4", "Terror", 2000),
  Obra("Autor E", 1999, "Descripción 5", "Audio", 3000)
)

// obras por tipología
def segmentarPorTipologia(obras: List[Obra]): Map[String, List[Obra]] = {
  obras.groupBy(_.tipologia)
}

// Encontrar la obra más costosa con más de 20 años de antigüedad
def obraMasCostosaAntigua(obras: List[Obra]): Option[Obra] = {
  val obrasAntiguas = obras.filter(obra => (2024 - obra.año) > 20)
  obrasAntiguas.maxByOption(_.precio)
}

// Calcular el patrimonio actual del museo
def calcularPatrimonio(obras: List[Obra]): Double = {
  obras.map(_.precio).sum
}

// Ordenar la lista por año (mayor a menor) de manera recursiva
def ordenarPorAño(obras: List[Obra]): List[Obra] = {
  if (obras.isEmpty) obras
  else {
    val maxAñoObra = obras.maxBy(_.año)
    maxAñoObra :: ordenarPorAño(obras.filterNot(_ == maxAñoObra))
  }
}

// Ordenar la lista por precio
def ordenarPorPrecio(obras: List[Obra]): List[Obra] = {
  obras.sortBy(_.precio)
}

// Aplicar un descuento del 50% 
def aplicarDescuento(obras: List[Obra], categoria: String, dia: String): List[Obra] = {
  if (dia == "viernes") {
    obras.map { obra =>
      if (obra.tipologia == categoria) obra.copy(precio = obra.precio * 0.5) else obra
    }
  } else obras
}



