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
    
  def intervalSize(other: Note): Int =
    // For compound intervals, we need to work with the actual semitone difference
    val isAscending = this.midiNumber < other.midiNumber
    val semitones = math.abs(this.midiNumber - other.midiNumber)
    
    // Calculate octave displacement
    val octaves = semitones / 12
    val remainingSemitones = semitones % 12
    
    // Calculate the simple interval (no octaves)
    val simpleInterval =
      // Special case for G to C
      if (this.name == NoteName.G && other.name == NoteName.C && isAscending) then
        4  // G up to C is a fourth
      // Special case for C to G  
      else if (this.name == NoteName.C && other.name == NoteName.G && !isAscending) then
        5  // C down to G is a fifth
      else
        remainingSemitones match
          case 0 => if semitones == 0 then 1 else 8  // unison or octave
          case 1 | 2 => 2  // second
          case 3 | 4 => 3  // third
          case 5 => 4  // fourth
          case 7 => 5  // fifth
          case 8 | 9 => 6  // sixth
          case 10 | 11 => 7  // seventh
          case 6 => if isAscending then 4 else 5  // tritone - aug 4th or dim 5th
          case _ => throw IllegalArgumentException(s"Invalid semitone difference: $remainingSemitones")
    
    // Calculate compound interval
    if simpleInterval == 8 then
      // For octaves: unison(1) + 7*octaves
      (octaves * 7) + 1
    else if simpleInterval == 1 && octaves > 0 then
      // Compound unison is an octave
      octaves * 7 + 1
    else
      // For compound intervals, add 7 for each octave
      simpleInterval + (octaves * 7)

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