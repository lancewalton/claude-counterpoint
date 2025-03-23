package counterpoint

enum IntervalQuality:
  case Perfect, Major, Minor, Augmented, Diminished

enum IntervalName:
  case Unison, Second, Third, Fourth, Fifth, Sixth, Seventh, Octave, Ninth, Tenth, Eleventh, Twelfth, Thirteenth, DoubleOctave

  def simpleIntervalName: IntervalName =
    this match
      case Ninth => Second
      case Tenth => Third
      case Eleventh => Fourth
      case Twelfth => Fifth
      case Thirteenth => Sixth
      case DoubleOctave => Octave
      case _ => this

case class Interval(number: Int, name: IntervalName, quality: IntervalQuality, semitones: Int):
  def isCompound: Boolean = number > 8

  def isTritone: Boolean = semitones == 6

  def isConsonant: Boolean =
    // For compound intervals, check the simple version
    val (simpleName, simpleQuality) = getSimpleForm
    (simpleName, simpleQuality) match
      case (IntervalName.Unison, IntervalQuality.Perfect) => true
      case (IntervalName.Third, IntervalQuality.Major) => true
      case (IntervalName.Third, IntervalQuality.Minor) => true
      case (IntervalName.Fifth, IntervalQuality.Perfect) => true
      case (IntervalName.Sixth, IntervalQuality.Major) => true
      case (IntervalName.Sixth, IntervalQuality.Minor) => true
      case (IntervalName.Octave, IntervalQuality.Perfect) => true
      case _ => false

  def isPerfect: Boolean =
    // For compound intervals, check if the simple form is perfect
    val (simpleName, simpleQuality) = getSimpleForm
    simpleQuality == IntervalQuality.Perfect && 
    (simpleName == IntervalName.Unison || simpleName == IntervalName.Fourth || 
     simpleName == IntervalName.Fifth || simpleName == IntervalName.Octave)
     
  def isPerfectFifth: Boolean =
    val (simpleName, simpleQuality) = getSimpleForm
    simpleName == IntervalName.Fifth && simpleQuality == IntervalQuality.Perfect
    
  def isPerfectOctave: Boolean =
    val (simpleName, simpleQuality) = getSimpleForm
    simpleName == IntervalName.Octave && simpleQuality == IntervalQuality.Perfect
    
  def getSimpleForm: (IntervalName, IntervalQuality) =
    if isCompound then
      (name.simpleIntervalName, quality)
    else
      (name, quality)
      
  def toSimpleInterval: Interval =
    if isCompound then
      // Get the simple interval properties
      val simpleNumber = (number - 1) % 7 + 1
      val simpleName = name.simpleIntervalName
      val simpleSemitones = semitones % 12
      
      // Create a new interval with the simple properties but preserve the quality
      Interval(simpleNumber, simpleName, quality, simpleSemitones)
    else
      this

object Interval:
  def apply(from: Note, to: Note): Interval =
    val isAscending = to.midiNumber >= from.midiNumber
    val semitones = math.abs(to.midiNumber - from.midiNumber)
    
    // First, determine the interval number based on the number of note names traversed
    val fromValue = noteNameToValue(from.name)
    val toValue = noteNameToValue(to.name)
    
    val octaveDiff = to.octave - from.octave
    val rawDiff = if isAscending then toValue - fromValue else fromValue - toValue
    val adjustedDiff = if rawDiff < 0 then rawDiff + 7 else rawDiff
    
    val number = if isAscending then
      adjustedDiff + 1 + (octaveDiff * 7)
    else
      // For descending intervals, we need to adjust differently
      -rawDiff + 1 + (-octaveDiff * 7)
    
    // Now, determine the interval name and quality based on the semitones and number
    val (name, quality) = numberAndSemitonesToNameAndQuality(number, semitones)
    
    Interval(number, name, quality, semitones)
  
  def between(note1: Note, note2: Note): Interval =
    if (note1.midiNumber <= note2.midiNumber)
      note1.interval(note2)
    else
      note2.interval(note1)
      
  def size(note1: Note, note2: Note): Int =
    if (note1.midiNumber <= note2.midiNumber)
      note1.intervalSize(note2)
    else
      note2.intervalSize(note1)
  
  def semitones(note1: Note, note2: Note): Int =
    math.abs(note1.midiNumber - note2.midiNumber) % 12
    
  def simpleSize(note1: Note, note2: Note): Int =
    val interval = size(note1, note2)
    if interval == 8 || interval % 7 == 1 then
      if interval == 1 then 1 else 8  // Handle unison vs octave
    else
      // Compound intervals reduce to their simple form
      (interval - 1) % 7 + 1
  
  def direction(from: Note, to: Note): Int =
    if (to.midiNumber > from.midiNumber) 1
    else if (to.midiNumber < from.midiNumber) -1
    else 0
  
  def isSkip(note1: Note, note2: Note): Boolean =
    val interval = size(note1, note2)
    interval == 3 || interval == 4  // Only actual thirds and fourths are skips
  
  def isLeap(note1: Note, note2: Note): Boolean =
    val intervalSize = size(note1, note2)
    val simpleInterval = simpleSize(note1, note2)
    
    // Both compound intervals and intervals of a fifth or larger are leaps
    intervalSize > 8 || simpleInterval >= 5  // Any interval of a fifth or larger is a leap
  
  def isMelodicStep(from: Note, to: Note): Boolean =
    val semitones = math.abs(to.midiNumber - from.midiNumber)
    semitones == 1 || semitones == 2  // minor or major second
  
  def isNoteInsideSpan(middleNote: Note, fromNote: Note, toNote: Note): Boolean =
    // Check if middleNote's MIDI number is between fromNote and toNote
    if fromNote.midiNumber <= toNote.midiNumber then
      // Ascending interval
      fromNote.midiNumber < middleNote.midiNumber && middleNote.midiNumber < toNote.midiNumber
    else
      // Descending interval
      toNote.midiNumber < middleNote.midiNumber && middleNote.midiNumber < fromNote.midiNumber
    
  private def noteNameToValue(name: NoteName): Int =
    name match
      case NoteName.C => 0
      case NoteName.D => 1
      case NoteName.E => 2
      case NoteName.F => 3
      case NoteName.G => 4
      case NoteName.A => 5
      case NoteName.B => 6
      
  private def numberAndSemitonesToNameAndQuality(number: Int, semitones: Int): (IntervalName, IntervalQuality) =
    // Determine the simple interval (1-8)
    val simpleNumber = if number > 8 then (number - 1) % 7 + 1 else number
    
    // Determine the expected semitones for a perfect or major interval
    val expectedSemitones = simpleNumber match
      case 1 => 0  // unison
      case 2 => 2  // major second
      case 3 => 4  // major third
      case 4 => 5  // perfect fourth
      case 5 => 7  // perfect fifth
      case 6 => 9  // major sixth
      case 7 => 11 // major seventh
      case 8 => 12 // perfect octave
    
    // Determine quality based on the difference between actual and expected semitones
    val simpleSemitones = semitones % 12
    
    // Special handling for octaves and unisons
    val quality = if ((simpleNumber == 8 || simpleNumber == 1) && simpleSemitones == 0) then
      IntervalQuality.Perfect
    else
      val diff = simpleSemitones - expectedSemitones
      (simpleNumber, diff) match
        case (1 | 4 | 5 | 8, 0) => IntervalQuality.Perfect
        case (1 | 4 | 5 | 8, 1) => IntervalQuality.Augmented
        case (1 | 4 | 5 | 8, -1) => IntervalQuality.Diminished
        case (2 | 3 | 6 | 7, 0) => IntervalQuality.Major
        case (2 | 3 | 6 | 7, -1) => IntervalQuality.Minor
        case (2 | 3 | 6 | 7, 1) => IntervalQuality.Augmented
        case (2 | 3 | 6 | 7, -2) => IntervalQuality.Diminished
        case _ => 
          // Default to a reasonable quality based on interval type for extreme cases
          if simpleNumber == 1 || simpleNumber == 4 || simpleNumber == 5 || simpleNumber == 8 then
            IntervalQuality.Perfect
          else
            IntervalQuality.Major
    
    // Determine the interval name based on the number
    // For intervals beyond our enum, use the highest applicable one
    // which is usually enough for our counterpoint rules
    val name = number match
      case 1 => IntervalName.Unison
      case 2 => IntervalName.Second
      case 3 => IntervalName.Third
      case 4 => IntervalName.Fourth
      case 5 => IntervalName.Fifth
      case 6 => IntervalName.Sixth
      case 7 => IntervalName.Seventh
      case 8 => IntervalName.Octave
      case 9 => IntervalName.Ninth
      case 10 => IntervalName.Tenth
      case 11 => IntervalName.Eleventh
      case 12 => IntervalName.Twelfth
      case 13 => IntervalName.Thirteenth
      case n if n >= 14 && n < 22 => IntervalName.DoubleOctave  // Handle up to triple octave - 1
      case _ => IntervalName.DoubleOctave  // For any larger interval, use double octave
                                         // This is a simplification but works for our rules
    
    (name, quality)