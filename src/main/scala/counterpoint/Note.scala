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
    val simpleInterval = remainingSemitones match
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
      
  /**
   * Calculate the interval between this note and another note
   */
  def interval(other: Note): Interval = Interval(this, other)
  
  /**
   * Returns a note that is one semitone lower
   */
  def lower: Note =
    Note.fromMidiNumber(midiNumber - 1)
    
  /**
   * Returns a note that is one semitone higher
   */
  def higher: Note =
    Note.fromMidiNumber(midiNumber + 1)

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
  
  /**
   * Mapping from MIDI note numbers to white keys (C major scale notes)
   */
  private val midiToWhiteKey = Map(
    0 -> (C, -1), 2 -> (D, -1), 4 -> (E, -1), 5 -> (F, -1), 7 -> (G, -1), 9 -> (A, -1), 11 -> (B, -1),
    12 -> (C, 0), 14 -> (D, 0), 16 -> (E, 0), 17 -> (F, 0), 19 -> (G, 0), 21 -> (A, 0), 23 -> (B, 0),
    24 -> (C, 1), 26 -> (D, 1), 28 -> (E, 1), 29 -> (F, 1), 31 -> (G, 1), 33 -> (A, 1), 35 -> (B, 1),
    36 -> (C, 2), 38 -> (D, 2), 40 -> (E, 2), 41 -> (F, 2), 43 -> (G, 2), 45 -> (A, 2), 47 -> (B, 2),
    48 -> (C, 3), 50 -> (D, 3), 52 -> (E, 3), 53 -> (F, 3), 55 -> (G, 3), 57 -> (A, 3), 59 -> (B, 3),
    60 -> (C, 4), 62 -> (D, 4), 64 -> (E, 4), 65 -> (F, 4), 67 -> (G, 4), 69 -> (A, 4), 71 -> (B, 4),
    72 -> (C, 5), 74 -> (D, 5), 76 -> (E, 5), 77 -> (F, 5), 79 -> (G, 5), 81 -> (A, 5), 83 -> (B, 5),
    84 -> (C, 6), 86 -> (D, 6), 88 -> (E, 6), 89 -> (F, 6), 91 -> (G, 6), 93 -> (A, 6), 95 -> (B, 6),
    96 -> (C, 7), 98 -> (D, 7), 100 -> (E, 7), 101 -> (F, 7), 103 -> (G, 7), 105 -> (A, 7), 107 -> (B, 7),
    108 -> (C, 8), 110 -> (D, 8), 112 -> (E, 8), 113 -> (F, 8), 115 -> (G, 8), 117 -> (A, 8), 119 -> (B, 8),
    120 -> (C, 9), 122 -> (D, 9), 124 -> (E, 9), 125 -> (F, 9), 127 -> (G, 9)
  )
  
  /**
   * Mapping from MIDI note numbers to black keys (accidentals)
   * Used for enharmonic alternatives
   */
  private val midiToBlackKey = Map(
    1 -> (C, -1, true), 3 -> (D, -1, true), 6 -> (F, -1, true), 8 -> (G, -1, true), 10 -> (A, -1, true),
    13 -> (C, 0, true), 15 -> (D, 0, true), 18 -> (F, 0, true), 20 -> (G, 0, true), 22 -> (A, 0, true),
    25 -> (C, 1, true), 27 -> (D, 1, true), 30 -> (F, 1, true), 32 -> (G, 1, true), 34 -> (A, 1, true),
    37 -> (C, 2, true), 39 -> (D, 2, true), 42 -> (F, 2, true), 44 -> (G, 2, true), 46 -> (A, 2, true),
    49 -> (C, 3, true), 51 -> (D, 3, true), 54 -> (F, 3, true), 56 -> (G, 3, true), 58 -> (A, 3, true),
    61 -> (C, 4, true), 63 -> (D, 4, true), 66 -> (F, 4, true), 68 -> (G, 4, true), 70 -> (A, 4, true),
    73 -> (C, 5, true), 75 -> (D, 5, true), 78 -> (F, 5, true), 80 -> (G, 5, true), 82 -> (A, 5, true),
    85 -> (C, 6, true), 87 -> (D, 6, true), 90 -> (F, 6, true), 92 -> (G, 6, true), 94 -> (A, 6, true),
    97 -> (C, 7, true), 99 -> (D, 7, true), 102 -> (F, 7, true), 104 -> (G, 7, true), 106 -> (A, 7, true),
    109 -> (C, 8, true), 111 -> (D, 8, true), 114 -> (F, 8, true), 116 -> (G, 8, true), 118 -> (A, 8, true),
    121 -> (C, 9, true), 123 -> (D, 9, true), 126 -> (F, 9, true)
  )
  
  /**
   * Create a Note from a MIDI number, preferring white keys in C major
   */
  def fromMidiNumber(midiNumber: Int): Note = {
    // First check if it's a white key
    if (midiToWhiteKey.contains(midiNumber)) {
      val (name, octave) = midiToWhiteKey(midiNumber)
      Note(name, octave)
    } else if (midiToBlackKey.contains(midiNumber)) {
      // For simplicity, we'll just pick the sharp version
      // (in real music theory, we'd choose based on context)
      val (name, octave, _) = midiToBlackKey(midiNumber)
      
      // Take the note below and sharpen it
      // This is a simplified approach - in real music theory, the choice would depend on the scale/key
      val noteBelowMidi = midiNumber - 1
      val (nameBelow, octaveBelow) = midiToWhiteKey(noteBelowMidi)
      
      // Return the white key that's one semitone lower
      Note(nameBelow, octaveBelow)
    } else {
      throw IllegalArgumentException(s"Invalid MIDI number: $midiNumber")
    }
  }