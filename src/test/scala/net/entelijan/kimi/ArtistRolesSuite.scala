package net.entelijan.kimi

import net.entelijan.kimi.Model._
import org.scalatest.FunSuite

class ArtistRolesSuite extends FunSuite {
  
  import ArtistRolesUtil._

  case class PTV(roles: String, names: String, namesRev: String, should: List[Artist])

  val ptvs = List(
    PTV("femD/masE", "Gerda Klein/Emil Tumpl", "Klein Gerda/Tumpl Emil",
      List(Artist(G_Feminin, AR_Director$, "Gerda Klein", "Klein Gerda"),
        Artist(G_Masculin, AR_Editor$, "Emil Tumpl", "Tumpl Emil"))),
    PTV("femW/masA", " Gerda Klein / Emil Tumpl ", "Klein Gerda/Tumpl Emil",
      List(Artist(G_Feminin, AR_Author$, "Gerda Klein", "Klein Gerda"),
        Artist(G_Masculin, AR1_Artist, "Emil Tumpl", "Tumpl Emil"))),
    PTV("femB", " Gerda Klein ", "Klein Gerda",
      List(Artist(G_Feminin, AR_Architect$, "Gerda Klein", "Klein Gerda"))))

  ptvs.foreach { tv =>
    test(s"""Parse ${tv}""") {
      val trs = parse(tv.roles, tv.names, tv.namesRev)
      assert(trs === tv.should)
    }

  }

  case class FTV(artists: List[Artist], lang: Lang, should: List[String])

  val ftvs = List(
    FTV(
      List(Artist(G_Feminin, AR1_Artist, "A B", "B A")),
      Ger,
      List("Künstlerin: B A")),
    FTV(
      List(Artist(G_Feminin, AR1_Artist, "A B", "B A"), Artist(G_Feminin, AR1_Artist, "X Y", "Y X")),
      Ger,
      List("Künstlerinnen: B A, Y X")),
    FTV(
      List(Artist(G_Masculin, AR1_Artist, "A B", "B A"), Artist(G_Feminin, AR1_Artist, "X Y", "Y X")),
      Ger,
      List("Künstler: B A, Y X")),
    FTV(
      List(Artist(G_Masculin, AR1_Artist, "A B", "B A"), Artist(G_Feminin, AR1_Artist, "X Y", "Y X")),
      Eng,
      List("Artists: B A, Y X")),
    FTV(
      List(Artist(G_Masculin, AR1_Artist, "A B", "B A"), Artist(G_Feminin, AR_Director$, "X Y", "Y X")),
      Ger,
      List("Künstler: B A", "Regie: Y X")),
    FTV(
      List(Artist(G_Masculin, AR1_Artist, "A B", "B A"), Artist(G_Feminin, AR_Director$, "X Y", "Y X")),
      Eng,
      List("Artist: B A", "Director: Y X")),
    FTV(
      List(Artist(G_Feminin, AR_Author$, "A B", "B A")),
      Eng,
      List("Author: B A")))

  ftvs.foreach { tv =>
    test(s"""Format ${tv}""") {
      val trs = format(tv.artists, tv.lang)
      assert(trs === tv.should)
    }

  }

}