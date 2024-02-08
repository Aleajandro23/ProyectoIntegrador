package ec.edu.utpl.computacion.pfr.pi

import breeze.plot.{Figure, plot}
import doobie.*
import doobie.implicits.*
import doobie.Transactor
import cats.effect.IO
import cats.*
import cats.effect.*
import cats.implicits.*
import cats.effect.unsafe.implicits.global
import com.github.tototoshi.csv.*
import doobie.implicits.toSqlInterpolator
import java.io.File
import org.nspl.*
import org.nspl.data.HistogramData
import org.saddle.{Index, Series, Vec}
import org.nspl.awtrenderer.*
import org.nspl.data.*
import saddle._
import org.saddle.*


implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}
object pi {
  @main
  def main() = {
    // Archivo del dataset dsAlienacionesXTorneo--------------------------------------------------------------
    val rutaAlineacionesTorneo = "D://Informacion//Documents//rubencis//pi//dsAlineacionesXTorneo.csv"
    val leerAlineacionesTorneo = CSVReader.open(new File(rutaAlineacionesTorneo))
    val alineacionesTorneo: List[Map[String, String]] = // de tipo lista que contiene Mapa de clave valor String
      leerAlineacionesTorneo.allWithHeaders() // Llamo al método allWithHeaders() de la función leerAlineacionesTorneo para obtener una lista de mapas que representan las alineaciones de un torneo.
    leerAlineacionesTorneo.close() // aqui cierro con el metodo Close de la funcion leer

    // Archivo del dataset dsPartidosYGoles-------------------------------------------------------
    val rutaPartyGoles = "D://Informacion//Documents//rubencis//pi//dsPartidosYGoles.csv"
    val leerPartidosGoles = CSVReader.open(new File(rutaPartyGoles))
    val partyGoles: List[Map[String, String]] = // partidos y goles lista de mapa con clave valor string
      leerPartidosGoles.allWithHeaders() // llamo al metodo allWhit de la funcion leerPArtidos
    leerPartidosGoles.close()

    // Conexion a la BD-----------------------------------------------------------------------------
    val xa = Transactor.fromDriverManager[IO](
      driver = "com.mysql.cj.jdbc.Driver",
      url = "jdbc:mysql://localhost:3306/db_pticum", // le pongo mi bd
      user = "root",
      password = "ruben2023",
      logHandler = None
    )

    // *-*-*------------------------------Metodos las consultas desde CSV ********************************************************

    //golesJugadores(partyGoles)
    //golesDelPArtido(partyGoles)
    //camisetaMidCampista(alineacionesTorneo)

    


    // ------*-*-*----------------------Métodos para las consultas desde la BD*-*-*-*------------------------------------------------------------

    //graficaGolesAnio(golesPorAnio().transact(xa).unsafeRunSync())
    //graficagolFavoryContra(golFavoryContra().transact(xa).unsafeRunSync())
    graficanroPartidosyGoles(nroPartidosyGoles().transact(xa).unsafeRunSync())


  }

  // Metodo para el reemplazo del valor NA por 0 para columnas de tipo entero o real
  def defaultValue(text: String): Double = {
    if (text.equals("NA")) {
      0
    } else {
      text.toDouble
    }
  }

  // ------------------------------CONSULTAS CON Gráficas de los DATASET CSV------------------------------
  // Gráfico de barras de goles por jugador
  def golesJugadores(data: List[Map[String, String]]) = {
    val golesJugador = data
      .filter(_("tournaments_tournament_name").contains("Men"))
      .map(row => (
        row("goals_player_id"),
        defaultValue(row("goals_minute_regulation")).toInt
      ))
      .groupBy(_._1)
      .map {
        case (jugadorId, goles) =>
          (jugadorId, goles.map(_._2).sum.toDouble)
      }
      .toList
      .sortBy(_._2)(Ordering[Double].reverse)
      .take(30) // Se toman solo los 30 primeros resultados

    graficaGolesJugadores(golesJugador)
  }

  def graficaGolesJugadores(data: List[(String, Double)]): Unit = {

    val indices = Index(data.map(_._1).toArray)
    val values = Vec(data.map(_._2).toArray)

    val series = Series(indices, values)

    val barPlot = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(0.5))
    )(
      par
        .xLabelRotation(-90)
        .xNumTicks(0)
        .xlab("ID Jugador")
        .ylab("Goles")
        .main("Goles por Jugador")
    )

    pngToFile(
      new File("D:\\Informacion\\Documents\\rubencis\\pi\\goleesJugadores.png\\"),
      barPlot.build,
      5000
    )
  }
  
  // Histograma de goles marcados por el equipo local
  def golesDelPArtido(data: List[Map[String, String]]) = {
    val golesDelPArtidoList =
      data
        .map(_("matches_home_team_score"))
        .filterNot(_.equals("NA"))
        .map(_.toDouble)

    val histograma = xyplot(HistogramData(golesDelPArtidoList, 20) -> line())(
      par
        .xlab("Goles")
        .ylab("freq.")
        .main("Goles marcados por equipos locales")
    )

    pngToFile(new File("D:\\Informacion\\Documents\\rubencis\\pi\\golesDelPArtido.png\\"),
      histograma.build, width = 1000)
  }


  // numero de camiseta de los mediocampistas
  def camisetaMidCampista(data: List[Map[String, String]]) = {
    val listNroShirt: List[Double] = data
      .filter(row => row("squads_position_name") == "midfielder")
      .map(row => row("squads_shirt_number").toDouble)

    val forwardNumber = xyplot(density(listNroShirt.toVec.toSeq) -> line())(
      par
        .xlab("Numero Camiseta")
        .ylab("freq.")
        .main("Posicion") //grafica nombre
    )

    pngToFile(new File("D://Informacion//Documents//rubencis//pi//camisetaMidCampista.png"),
      forwardNumber.build, width = 1000)

  }

  // grafica en barras de goles en Mundiales
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

    //selectedColumns.foreach(println)
    selectedColumns
  }

  def charting(data: List[(String, Int)]): Unit = {
    val listX: List[Double] = data.map(x => x._1.toDouble)
    val listY: List[Double] = data.map(x => x._2.toDouble)

    val grafica = xyplot(listX -> listY -> bar())(
      par
        .xlab("años mundialistas")
        .ylab("goles")
        .main("Goles en mundiales")
    )

    pngToFile(new File("D:\\Informacion\\Documents\\rubencis\\pi\\gmund.png\\"), grafica.build, 1000)
  }



  // -----------------------------Gráficas a partir de la BASE DE D -------------------------------------------------

  // Gráfico de barras
  // obtener el total de goles dependiendo del año
  def golesPorAnio(): ConnectionIO[List[(String, Double)]] = {
    sql"""
      SELECT
        tournaments.year,
        SUM(dspartidosygoles.matches_home_team_score + dspartidosygoles.matches_away_team_score) AS total_goles
      FROM dspartidosygoles
      INNER JOIN tournaments ON dspartidosygoles.matches_tournament_id = tournaments.id
      GROUP BY tournaments.year
    """
      .query[(String, Double)]
      .to[List]
  }


  def graficaGolesAnio(data: List[(String, Double)]) = {
    val indices = Index(data.map(_._1).toArray)
    val values = Vec(data.map(_._2).toArray)

    val series = Series(indices, values)

    val barPlot = saddle.barplotHorizontal(series,
      xLabFontSize = Option(RelFontSize(0.5))
    )(
      par
        .xLabelRotation(-90)
        .xNumTicks(0)
        .xlab("Year")
        .ylab("Goles Totales")
        .main("Goles por Anio")
    )

    pngToFile(
      new File("D:\\Informacion\\Documents\\rubencis\\pi\\golesAnio.png\\"),
      barPlot.build,
      5000
    )
  }


  // Gráfica de dispersión de Goles a favor y goles en contra por equipo
  def golFavoryContra(): ConnectionIO[List[(Int, Int)]] = {
    sql"""
    SELECT
      SUM(CASE WHEN teams.id = matches.home_team_id THEN matches.home_team_score ELSE matches.away_team_score END) AS goles_a_favor,
      SUM(CASE WHEN teams.id = matches.away_team_id THEN matches.home_team_score ELSE matches.away_team_score END) AS goles_en_contra
    FROM teams
    INNER JOIN matches ON teams.id IN (matches.home_team_id, matches.away_team_id)
    GROUP BY teams.id;
  """
      .query[(Int, Int)]
      .to[List]
  }

  def graficagolFavoryContra(data: List[(Int, Int)]): Unit = {
    val f = Figure()
    val p = f.subplot(0)
    p += plot(data.map(_._1), data.map(_._2), '+')
    p.xlabel = "Goles a Favor"
    p.ylabel = "Goles en Contra"
    f.saveas("D:\\Informacion\\Documents\\rubencis\\pi\\golFavoryContra.png\\")
  }

  // Gráfica de dispersión de Número de partidos y número de goles en cada torneo:
  def nroPartidosyGoles(): ConnectionIO[List[(Int, Int)]] = {
    sql"""
    SELECT
      COUNT(matches.id) AS partidos,
      SUM(matches.home_team_score + matches.away_team_score) AS goles
    FROM matches
    INNER JOIN tournaments ON matches.tournament_id = tournaments.id
    GROUP BY tournaments.id;
  """
      .query[(Int, Int)]
      .to[List]
  }

  def graficanroPartidosyGoles(data: List[(Int, Int)]): Unit = {
    val f = Figure()
    val p = f.subplot(0)
    p += plot(data.map(t => t._1), data.map(t => t._2), '+')
    p.xlabel = "Partidos"
    p.ylabel = "Goles"
    f.saveas("D://Informacion//Documents//rubencis//pi//partidosyGoles.png")
  }

}