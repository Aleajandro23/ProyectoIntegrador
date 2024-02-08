/*
package ec.edu.utpl.computacion.pfr.pi

import com.github.tototoshi.csv._
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData
import java.io.File
import com.github.tototoshi.csv._
import org.nspl._

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}
/*
object App {
  @main
  def pintegra() =
    //D:\Informacion\Downloads\__MACOSX\._dsPartidosYGoles.csv
    //"D://Informacion//Downloads//__MACOSX//._dsPartidosYGoles.csv"
    val pathDataFile: String = "D:\\Informacion\\Documents\\rubencis\\pi\\dsPartidosYGoles.csv"
    val reader = CSVReader.open(new File(pathDataFile))
    //val contentFile: List[List[String]] = reader.all()
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()
    reader.close()
    println(contentFile.take(2))
    println(s"Filas: ${contentFile.length}  y Columnas: ${contentFile(1).keys.size}")
    datosGrafica(contentFile)
    //charting(contentFile)
/*
    charting(contentFile)

  def charting(data: List[Map[String, String]]): Unit = {
    val listNroShirt: List[Double] = data.filter(row => row("squads_position_name") == "forward" && row("squads_shirt_number") != "0")
      .map(row => row("squads_shirt_number").toDouble)

    val hisForwardShirtNumber = xyplot(HistogramData(listNroShirt, 10) -> bar())(
      par // viene implicitamente
        .xlab("Numero Camiseta")
        .ylab("freq")
        .main("Fowrard Shirt number")
    )
    pngToFile(new File("D:\\Informacion\\Documents\\rubencis\\pi\\imagen1.png"), hisForwardShirtNumber.build, 1000)
    renderToByteArray(hisForwardShirtNumber.build, width = 2000)


  }
  */
    datosGrafica(contentFile)
  def datosGrafica(data: List[Map[String, String]]): Unit = {
    val dataGoles = data
      .map(row => (
        row("tournaments tournament_name"),
        row("matches match_id"),
        row("matches_home_team_score"),
        row("matches_away_team_score")
      ))
      .distinct
      .map(t4 => (t4._1, t4._3.toInt + t4._4.toInt))
      .groupBy(_._1)
      .map(t2 => (t2._1, t2._2.map(_._2).sum))
      .toList
      .sortBy(_._1)

    dataGoles.foreach(println)

    dataGoles

  }

  def charting(dataGoles: List[(String, Int)]): Unit = {

    //val tournaments = dataGoles.map(_._1)
    val goles = dataGoles.map(_._2.toDouble)


    val histTournamentGoals = xyplot(HistogramData(goles, 10) -> bar())(
      par
        .xlab("Nombre del Torneo")
        .ylab("Total de Goles")
        .main("Total de Goles por Torneo")

    )

    // Guarda el histograma como una imagen PNG
    pngToFile(new File("D:\\Informacion\\Documents\\rubencis\\pi\\grafica.png"), histTournamentGoals.build, 1000)

  }

}

 */


object App {
  @main
  def principal(): Unit = {
    val pathDataFile: String = "D://Informacion//Documents//rubencis//pi//dsPartidosYGoles.csv"
    val reader = CSVReader.open(new File(pathDataFile))
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()

    val fue: List[(String, Int)] = goal1(contentFile)
    charting(fue)

    val pathDataFile2: String = "D://Informacion//Documents//rubencis//pi//dsAlineacionesXTorneo.csv"
    val reader2 = CSVReader.open(new File(pathDataFile2))
    val contentFile2: List[(String, Int)] = reader2.allWithHeaders()

    reader2.close()
    charting(contentFile2)
  }

  def goal1(data: List[Map[String, String]]): List[(String, Int)] = {
    val selectedColumns = data
      .map(row => (
        row("tournaments_year"),
        row("matches_match_id"),
        row("matches_home_team_score"),
        row("matches_away_team_score")
      ))
      .distinct
      .map(t4 => (t4._1, t4._3.toInt + t4._4.toInt))
      .groupBy(_._1)
      .map(t2 => (t2._1, t2._2.map(_._2).sum))
      .toList
      .sortBy(_._1)

    selectedColumns.foreach(println)
    selectedColumns
  }



  def charting(data: List[(String, Int)]): Unit = {
    val listX:List[Double]=data.map(x=>x._1.toDouble)
    val listY:List[Double]=data.map(x=>x._2.toDouble)
    val grafica = xyplot(HistogramData(listX, 10) -> bar())(
      par
        .xlab("años mundialistas")
        .ylab("goles")
        .main("Goles en mundiales")

    )
    pngToFile(new File("D://Informacion//Documents//rubencis//pi//gm.png"), grafica.build,1000)
  }



  def charting2(data: List[Map[String, String]]): Unit =
    val listNroShirt: List[Double] = data
       .filter(row => row("squads_position_name") == "forward" && row("squads_shirt_number") != "0")
       .map(row => row("squads_shirt_number").toDouble)
     val histForwardShirtNumber = xyplot(HistogramData(listNroShirt, 10) -> bar())(
       par // viene implicitamente
         .xlab("Shirt number")
         .ylab("freq.")
         .main("Forward shirt number")
     )

     pngToFile(new File("D://Informacion//Documents//rubencis//pi//fhsn.png"), histForwardShirtNumber.build, 1000)
     renderToByteArray(histForwardShirtNumber.build, width = 2000)




}

 */