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
    (name, quality) match
      case (IntervalName.Unison, IntervalQuality.Perfect) => true
      case (IntervalName.Third, IntervalQuality.Major) => true
      case (IntervalName.Third, IntervalQuality.Minor) => true
      case (IntervalName.Fifth, IntervalQuality.Perfect) => true
      case (IntervalName.Sixth, IntervalQuality.Major) => true
      case (IntervalName.Sixth, IntervalQuality.Minor) => true
      case (IntervalName.Octave, IntervalQuality.Perfect) => true
      case _ => false

  def isPerfect: Boolean =
    quality == IntervalQuality.Perfect && 
    (name == IntervalName.Unison || name == IntervalName.Fourth || 
     name == IntervalName.Fifth || name == IntervalName.Octave ||
     name == IntervalName.Eleventh || name == IntervalName.Twelfth || 
     name == IntervalName.DoubleOctave)

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
    val quality = (simpleNumber, simpleSemitones - expectedSemitones) match
      case (1 | 4 | 5 | 8, 0) => IntervalQuality.Perfect
      case (1 | 4 | 5 | 8, 1) => IntervalQuality.Augmented
      case (1 | 4 | 5 | 8, -1) => IntervalQuality.Diminished
      case (2 | 3 | 6 | 7, 0) => IntervalQuality.Major
      case (2 | 3 | 6 | 7, -1) => IntervalQuality.Minor
      case (2 | 3 | 6 | 7, 1) => IntervalQuality.Augmented
      case (2 | 3 | 6 | 7, -2) => IntervalQuality.Diminished
      case _ => throw IllegalArgumentException(s"Cannot determine quality for interval: number=$simpleNumber, semitones=$simpleSemitones")
    
    // Determine the interval name based on the number
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
      case 14 => IntervalName.DoubleOctave
      case _ => throw IllegalArgumentException(s"Interval number out of supported range: $number")
    
    (name, quality)