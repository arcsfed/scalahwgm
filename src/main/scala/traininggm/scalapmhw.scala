package traininggm
import java.util.Date
import scala.io.Source
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object scalapmhw {

  case class movie(movieId: Int, title: String, year: Option[Int], genres: List[String])
  case class rating(userId: Int, movieId: Int, rate: Double, ts: Date)
  case class tag(userId: Int, movieId: Int, tag: String, ts: Date)

  def parseMovie(line: String): movie = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1).toList match {
    case movieId :: titleWYear :: genres :: Nil => //println(titleWYear)
      movie(
        movieId.toInt,
        if (titleWYear.contains("(")) titleWYear.substring(0, titleWYear.lastIndexOf("(")).init else titleWYear,
        Try(titleWYear.substring(titleWYear.lastIndexOf("("), titleWYear.lastIndexOf(")")).drop(1).toInt).toOption,
        genres.split('|').toList
      )
  }

  def parseRating(line:String): rating = line.split(",").toList match {
    case userId :: movieId :: rate :: ts :: Nil =>
      rating(
        userId.toInt,
        movieId.toInt,
        rate.toDouble,
        new Date(ts.toLong * 1000)
      )
  }

  def parseTag(line:String): tag = line.split(",").toList match {
    case userId :: movieId :: rate :: ts :: Nil =>
      tag(
        userId.toInt,
        movieId.toInt,
        tag.toString,
        new Date(ts.toLong * 1000)
      )
  }

  def main(args: Array[String]): Unit = {
    val movieLines = Source.fromFile("data/movies.csv").getLines().toList.drop(1)
    val ratingLines = Source.fromFile("data/ratings.csv").getLines().toList.drop(1)
    val tagLines = Source.fromFile("data/tags.csv").getLines().toList.drop(1)

    val movies = movieLines.map(parseMovie)
    //println(movies.head)
    val ratings = ratingLines.map(parseRating)
    //println(ratings.head)
    val tags = tagLines.map(parseTag)

    //Get distinct years
    val distinctYears = movies.flatMap(_.year).toSet
    val distinctGenres = movies.flatMap(_.genres).toSet
    val totalFilmCount = movies.map(m => m.movieId).size
    val totalYearCount = movies.map(m => m.year).toSet.size
    val groupedRatingsbyUserId = ratings.groupBy(r => r.userId)
    val ratingCountforEachUser = groupedRatingsbyUserId.map { case (k,v) =>
      (k,v.map(_.movieId))}
    val groupedTagsbyUserId = tags.groupBy(r => r.userId)
    val tagCountforEachUser = groupedTagsbyUserId.map { case (k,v) =>
      (k,v.map(_.movieId))}
    val mostRatingUser = tags.map(_.userId).groupBy(i => i).map { case (k, v) => (k, v.length) }.toList.sortBy(_._2).reverse.take(1).head
    val filteredRatingsbyTop = ratings.filter(_.userId == mostRatingUser._1).map(f => f.movieId)

    //Function gives genre count for year
    def getGenreCountofYear (x: Int) = {
      val yearMovies = movies.filter(_.year.contains(x))
      val yearMovieGenres = yearMovies.flatMap(_.genres)
      val countGenres = yearMovieGenres.groupBy(i => i)
      val grouped = countGenres.map { case (k, v) => (k, v.length) }.toList
      println(s"Year Of films: $x")
      grouped.sortBy(_._2).reverse.foreach(println)

    }


    def getRatingsPerYear(x: Int) = {
      val movieMap = movies.map(m => (m.movieId, m)).toMap
      val groupedRatings = ratings.filter(_.rate == 5.0).groupBy(r => r.movieId)
      val averageRatings = groupedRatings.map { case (k, v) =>
        (k, v.map(_.rate).sum)
      }.toList
      val arWYear = averageRatings.map(ar => (movieMap(ar._1).year, ar._2))
      val groupedarWYears = arWYear.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).sum/5.0) }.toList
      groupedarWYears.sortBy(_._2).reverse.take(x).foreach(println)
    }

    def getLeastGenre = {
      val genres = movies.flatMap(_.genres)
      val countGenres = genres.groupBy(i => i)
      val listGenres = countGenres.map { case (k, v) => (k, v.length) }.toList
      val leastGenre = listGenres.sortBy(_._2).drop(1).take(1)
      println(s"Total Film Count: $totalFilmCount in $totalYearCount years")
      println(s"Least filmed genre:")
      leastGenre.foreach(println)
    }

    def getUserTagsRatings(year:Int) = {
      movies.filter(_.year.contains(year)).flatMap(_.genres).toSet.map(x => getAuxUTR(x))
      def getAuxUTR(str: String) = {
        val filteredMovieIds = movies.filter(_.year.contains(year)).filter(_.genres.contains(str)).map( m => m.movieId)
        val filteredRCfEU = ratingCountforEachUser.filter(_._2.exists(filteredMovieIds.contains)).map( f => f._1).toList
        val filteredTCfEU = tagCountforEachUser.filter(_._2.exists(filteredMovieIds.contains)).map( f => f._1).toList
        if (filteredTCfEU.isEmpty) {
        println(s"No tags found for $str at year $year") } else
        { val ratio = (filteredTCfEU.intersect(filteredRCfEU).length.toDouble / filteredRCfEU.length.toDouble) * 100
          println(s"For Year $year and genre $str ratio is $ratio") }
      }
    }


    def getAUserRatings (year: Int) = {
      movies.filter(_.year.contains(year)).flatMap(_.genres).toSet.map(x => getAuxAUR(x))
      def getAuxAUR(str: String) = {
        println(s"for genre $str")
        val filteredMovieIds = movies.filter(_.year.contains(year)).filter(_.genres.contains(str)).map( m => m.movieId)
        val filteredRatingsbyUser = filteredRatingsbyTop.intersect(filteredMovieIds)
        val movieUserList = ratings.filter( i => filteredRatingsbyUser.contains(i.movieId)).filter(_.userId == mostRatingUser._1).map(i => i.rate).sum

      }

    }


    def getRateTagRatio (str: String) = {
      println(s"For genre $str")
      val zeroDate = new Date(0.toLong * 1000)
      val ratedMoviesTs = ratings.map(r => (r.movieId,r.ts)).toMap
      val taggedMoviesTs = tags.map(t => (t.movieId,t.ts)).toMap
      val genreMovieIds = movies.filter(_.genres.contains(str)).map( m => if ((ratedMoviesTs.contains(m.movieId) && taggedMoviesTs.contains(m.movieId))) (m.movieId, ratedMoviesTs(m.movieId), taggedMoviesTs(m.movieId)) else (0,zeroDate,zeroDate)).filter(i => i._1 != 0)
      val ratioResult = genreMovieIds.map(g => if (g._2.after(g._3)) "tag comes first" else "rate comes first" ).groupBy(l => l).map(t => (t._1, t._2.length))
      ratioResult.foreach(println)

      }





    //Answer-1
    //distinctYears.map(x => getGenreCountofYear(x))
    //Answer-2 //You can give parameters for how much years you want
    //getRatingsPerYear(1)
    //Answer-3
    //getLeastGenre
    //Answer-4
    //distinctYears.map(x => getUserTagsRatings(x))
    //Answer-5
    //distinctYears.map(x => getAUserRatings(x))
    //Answer-6
    //distinctGenres.map(x => getRateTagRatio(x))





  }



}
