package counterpoint

enum NoteName:
  case C, D, E, F, G, A, B

case class Note(name: NoteName, octave: Int):
  override def toString: String = s"$name$octave"
  
  def midiNumber: Int = 
    val noteValues = Map(
      NoteName.C -> 0,
      NoteName.D -> 2,
      NoteName.E -> 4,
      NoteName.F -> 5,
      NoteName.G -> 7,
      NoteName.A -> 9,
      NoteName.B -> 11
    )
    (octave + 1) * 12 + noteValues(name)

  def isWithinOctave(other: Note): Boolean =
    val diff = math.abs(this.midiNumber - other.midiNumber)
    diff <= 12

object Note:
  import NoteName.*
  
  val G2 = Note(G, 2)
  val A2 = Note(A, 2)
  val B2 = Note(B, 2)
  val C3 = Note(C, 3)
  val D3 = Note(D, 3)
  val E3 = Note(E, 3)
  val F3 = Note(F, 3)
  val G3 = Note(G, 3)
  val A3 = Note(A, 3)
  val B3 = Note(B, 3)
  val C4 = Note(C, 4)
  val D4 = Note(D, 4)
  val E4 = Note(E, 4)
  val F4 = Note(F, 4)
  val G4 = Note(G, 4)
  val A4 = Note(A, 4)
  val B4 = Note(B, 4)
  val C5 = Note(C, 5)
  val D5 = Note(D, 5)
  val E5 = Note(E, 5)
  val F5 = Note(F, 5)
  val G5 = Note(G, 5)
  
  val cMajorScale: List[Note] = List(
    G2, A2, B2, 
    C3, D3, E3, F3, G3, A3, B3,
    C4, D4, E4, F4, G4, A4, B4,
    C5, D5, E5, F5, G5
  )
  
  val allNotes: List[Note] = cMajorScale