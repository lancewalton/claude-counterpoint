package counterpoint

enum NoteName:
  case C, D, E, F, G, A, B

enum Accidental:
  case Natural, Sharp, Flat

case class Note(name: NoteName, accidental: Accidental, octave: Int):
  override def toString: String = 
    val accStr = accidental match
      case Accidental.Natural => ""
      case Accidental.Sharp => "#"
      case Accidental.Flat => "b"
    s"${name}$accStr$octave"

object Notes:
  import NoteName.*
  import Accidental.*
  
  // C Major scale notes from G2 to G5
  val G2 = Note(G, Natural, 2)
  val A2 = Note(A, Natural, 2)
  val B2 = Note(B, Natural, 2)
  val C3 = Note(C, Natural, 3)
  val D3 = Note(D, Natural, 3)
  val E3 = Note(E, Natural, 3)
  val F3 = Note(F, Natural, 3)
  val G3 = Note(G, Natural, 3)
  val A3 = Note(A, Natural, 3)
  val B3 = Note(B, Natural, 3)
  val C4 = Note(C, Natural, 4)
  val D4 = Note(D, Natural, 4)
  val E4 = Note(E, Natural, 4)
  val F4 = Note(F, Natural, 4)
  val G4 = Note(G, Natural, 4)
  val A4 = Note(A, Natural, 4)
  val B4 = Note(B, Natural, 4)
  val C5 = Note(C, Natural, 5)
  val D5 = Note(D, Natural, 5)
  val E5 = Note(E, Natural, 5)
  val F5 = Note(F, Natural, 5)
  val G5 = Note(G, Natural, 5)
  
  // C Major scale
  val cMajorScale: List[Note] = List(
    G2, A2, B2, 
    C3, D3, E3, F3, G3, A3, B3,
    C4, D4, E4, F4, G4, A4, B4,
    C5, D5, E5, F5, G5
  )

@main def main(): Unit = 
  println("C Major Scale from G2 to G5:")
  Notes.cMajorScale.foreach(println)
