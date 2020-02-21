package io.github.jkobejs

import scala.io.Source
import cats.implicits._
import java.io.PrintWriter

object Main extends App {
  val fileNames = List(
    "a_example.txt",
    "b_read_on.txt",
    "c_incunabula.txt",
    "d_tough_choices.txt",
    "e_so_many_books.txt",
    "f_libraries_of_the_world.txt"
  )

  fileNames.foreach { filename =>
    val iterator = Source.fromResource(filename).getLines()

    Game.parse(iterator) match {
      case Right(game) =>
        var step = 0
        val selectedLibraries =
          scala.collection.mutable.ListBuffer.empty[Library]
        val remainingLibraries = scala.collection.mutable.Map
          .from(game.libraries.map(lib => (lib.id, lib)))
        val scanningDays = game.init.scanningDays
        val remainingBooks = scala.collection.mutable.Map
          .from(game.books.map(book => (book.id, book)))

        while (step < game.init.scanningDays) {
          val remainingDays = scanningDays - step
          val librariesSortedByPotential =
            remainingLibraries.values.toList
              .map(library =>
                library.copy(books =
                  library.books.filter(book => remainingBooks.contains(book.id))
                )
              )
              .sortBy { library =>
                val totalPotential = library.books
                  .map(_.score)
                  .sortWith(_ > _)
                  .take(
                    (remainingDays - library.signupProcess) * library.bookScannRate
                  )
                  .sum
                val score = totalPotential.toDouble / library.signupProcess
                score
              }

          val lastLibOpt = librariesSortedByPotential.lastOption

          lastLibOpt match {
            case Some(lastLib) =>
              lastLib.books.foreach(book => remainingBooks.remove(book.id))

              selectedLibraries.addOne(
                lastLib.copy(books = lastLib.books
                  .sortBy(_.score)
                  .takeRight(
                    (remainingDays - lastLib.signupProcess) * lastLib.bookScannRate
                  )
                )
              )

              remainingLibraries.remove(lastLib.id)
              step += lastLib.signupProcess
            case None => step = game.init.scanningDays
          }
        }

        val solution = Solution(selectedLibraries.toList.filter(_.books.nonEmpty))

        Solution.serialize(solution, filename)

      case Left(error) => println(error)
    }
  }
}

case class Game(
    init: Init,
    books: List[Book],
    libraries: List[Library]
)

case class Solution(
    libraries: List[Library]
)

object Solution {
  def serialize(solution: Solution, filename: String) = {
    new PrintWriter(s"1-solution-$filename") {
      write(s"${solution.libraries.length.toString}\n")
      solution.libraries.map(lib => write(Library.serialize(lib)))
      close
    }
  }
}

object Game {
  def parse(lines: Iterator[String]): Either[String, Game] = {
    for {
      init <- parseInit(lines)
      books <- parseBooks(lines, init)
      libraries <- parseLibraries(lines, init, books)
    } yield Game(init, books, libraries)
  }

  def parseInit(lines: Iterator[String]): Either[String, Init] = {
    if (lines.hasNext) {
      Init.parse(lines.next())
    } else
      Left(s"Init lines missing")
  }
  def parseBooks(
      lines: Iterator[String],
      init: Init
  ): Either[String, List[Book]] = {
    if (lines.hasNext) {
      Book.parse(lines.next(), init)
    } else
      Left(s"Books line is missing")
  }
  def parseLibraries(
      lines: Iterator[String],
      init: Init,
      books: List[Book]
  ): Either[String, List[Library]] = {
    val booksMap = books.map(book => (book.id, book)).toMap
    (0 until init.libraries).toList.traverse(libraryId =>
      for {
        libraryLine <- lines
          .nextOption()
          .toRight(s"First library line for library $libraryId is missing")
        booksLine <- lines
          .nextOption()
          .toRight(s"Second library line for library $libraryId is missing")
        library <- Library.parse(libraryId, libraryLine, booksLine, booksMap)
      } yield library
    )
  }
}

case class Init(
    books: Int,
    libraries: Int,
    scanningDays: Int
)

object Init {
  def parse(line: String): Either[String, Init] = {
    val initArray = line.split(" ")
    if (initArray.size != 3) {
      Left(s"Invalid init line: $line")
    } else {
      (for {
        books <- initArray(0).toIntOption
        libraries <- initArray(1).toIntOption
        scanningDays <- initArray(2).toIntOption
      } yield Init(
        books = books,
        libraries = libraries,
        scanningDays = scanningDays
      )).toRight(s"Invalid init line: $line")
    }
  }
}

case class Book(id: Int, score: Int)

object Book {
  def parse(line: String, init: Init): Either[String, List[Book]] = {
    val booksArray = line.split(" ")
    if (booksArray.size != init.books) {
      Left(s"Invalid books line: $line")
    } else {
      Right(
        booksArray
          .map(_.toIntOption)
          .collect {
            case Some(score) => score
          }
          .zipWithIndex
          .map { case (score, index) => Book(id = index, score = score) }
          .toList
      )
    }
  }
}
case class Library(
    id: Int,
    books: List[Book],
    signupProcess: Int,
    bookScannRate: Int
)

object Library {
  def parse(
      id: Int,
      libraryLine: String,
      booksLine: String,
      books: Map[Int, Book]
  ): Either[String, Library] = {
    val initArray = libraryLine.split(" ")
    if (initArray.size != 3) {
      Left(s"Invalid init line: $libraryLine")
    } else {
      (for {
        numberOfBooks <- initArray(0).toIntOption
        signupProcess <- initArray(1).toIntOption
        bookScannRate <- initArray(2).toIntOption
        libraryBooks = booksLine
          .split(" ")
          .map(_.toIntOption)
          .collect { case Some(id) => books.get(id) }
          .collect { case Some(book) => book }
        _ <- if (libraryBooks.size != numberOfBooks) None else Some(())
      } yield Library(
        id = id,
        books = libraryBooks.toList,
        signupProcess = signupProcess,
        bookScannRate = bookScannRate
      )).toRight(s"Invalid library init line: $libraryLine")
    }
  }

  def serialize(library: Library): String = {
    s"${library.id} ${library.books.length}\n" +
      library.books.map(_.id).mkString(" ").trim +
      "\n"
  }
}
