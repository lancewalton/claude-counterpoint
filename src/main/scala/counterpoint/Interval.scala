package counterpoint

enum IntervalQuality:
  case Perfect, Major, Minor, Augmented, Diminished

enum IntervalSize:
  case Unison, Second, Third, Fourth, Fifth, Sixth, Seventh, Octave

case class Interval private (size: IntervalSize, quality: IntervalQuality):
  def semitones: Int = Interval.semitonesMap((size, quality))
  
  override def toString: String = s"$quality $size"

object Interval:
  import IntervalQuality.*
  import IntervalSize.*
  
  private val semitonesMap: Map[(IntervalSize, IntervalQuality), Int] = Map(
    (Unison, Perfect) -> 0,
    (Unison, Augmented) -> 1,
    (Second, Minor) -> 1,
    (Second, Major) -> 2,
    (Second, Augmented) -> 3,
    (Third, Diminished) -> 2,
    (Third, Minor) -> 3,
    (Third, Major) -> 4,
    (Third, Augmented) -> 5,
    (Fourth, Diminished) -> 4,
    (Fourth, Perfect) -> 5,
    (Fourth, Augmented) -> 6,
    (Fifth, Diminished) -> 6,
    (Fifth, Perfect) -> 7,
    (Fifth, Augmented) -> 8,
    (Sixth, Diminished) -> 7,
    (Sixth, Minor) -> 8,
    (Sixth, Major) -> 9,
    (Sixth, Augmented) -> 10,
    (Seventh, Diminished) -> 9,
    (Seventh, Minor) -> 10,
    (Seventh, Major) -> 11,
    (Seventh, Augmented) -> 12,
    (Octave, Diminished) -> 11,
    (Octave, Perfect) -> 12,
    (Octave, Augmented) -> 13
  )
  
  private val perfectIntervals = Set(Unison, Fourth, Fifth, Octave)
  
  private val stepSizes: Map[NoteName, Int] = Map(
    NoteName.C -> 0,
    NoteName.D -> 2,
    NoteName.E -> 4,
    NoteName.F -> 5,
    NoteName.G -> 7,
    NoteName.A -> 9,
    NoteName.B -> 11
  )
  
  def between(note1: Note, note2: Note): Interval = {
    if (note1 == note2) return PerfectUnison
    
    val octaveDiff = note2.octave - note1.octave
    
    if (note1.name == note2.name && octaveDiff == 1) return PerfectOctave
    
    val noteOrderMap = stepSizes
    val halfSteps = noteOrderMap(note2.name) - noteOrderMap(note1.name) + (octaveDiff * 12)
    
    // Special case for common intervals
    if (note1.name == NoteName.C && note2.name == NoteName.D && octaveDiff == 0) return MajorSecond
    if (note1.name == NoteName.C && note2.name == NoteName.E && octaveDiff == 0) return MajorThird
    if (note1.name == NoteName.E && note2.name == NoteName.G && octaveDiff == 0) return MinorThird
    if (note1.name == NoteName.C && note2.name == NoteName.G && octaveDiff == 0) return PerfectFifth
    
    val simpleHalfSteps = Math.abs(halfSteps) % 12
    val invertedHalfSteps = if (simpleHalfSteps == 0) 0 else 12 - simpleHalfSteps
    
    val isAscending = halfSteps >= 0
    
    val diatonicDiff = getDiatonicDifference(note1.name, note2.name, octaveDiff)
    val simpleSize = sizeFromDiatonicDiff(diatonicDiff)
    val expectedPerfect = perfectIntervals.contains(simpleSize)
    
    val (finalSize, finalQuality) = if (isAscending) {
      determineIntervalQuality(simpleSize, simpleHalfSteps, expectedPerfect)
    } else {
      determineIntervalQuality(simpleSize, invertedHalfSteps, expectedPerfect)
    }
    
    Interval(finalSize, finalQuality)
  }
  
  private def getDiatonicDifference(name1: NoteName, name2: NoteName, octaveDiff: Int): Int = {
    val noteOrder = Vector(NoteName.C, NoteName.D, NoteName.E, NoteName.F, NoteName.G, NoteName.A, NoteName.B)
    val index1 = noteOrder.indexOf(name1)
    val index2 = noteOrder.indexOf(name2)
    val noteDiff = index2 - index1
    
    val totalDiff = noteDiff + (octaveDiff * 7)
    val normalizedDiff = ((totalDiff % 7) + 7) % 7
    normalizedDiff
  }
  
  private def sizeFromDiatonicDiff(diff: Int): IntervalSize = diff match {
    case 0 => Unison
    case 1 => Second
    case 2 => Third
    case 3 => Fourth
    case 4 => Fifth
    case 5 => Sixth
    case 6 => Seventh
    case _ => Octave
  }
  
  private def determineIntervalQuality(size: IntervalSize, halfSteps: Int, expectPerfect: Boolean): (IntervalSize, IntervalQuality) = {
    import IntervalQuality.*
    
    val (reference, quality) = if (expectPerfect) {
      val perfectReference = size match {
        case Unison => 0
        case Fourth => 5
        case Fifth => 7
        case Octave => 12
        case _ => throw new IllegalArgumentException(s"$size is not a perfect interval")
      }
      
      halfSteps.compare(perfectReference) match {
        case -2 => (size, Diminished)
        case -1 => (size, Diminished)
        case 0 => (size, Perfect)
        case 1 => (size, Augmented)
        case 2 => (size, Augmented)
        case _ => (size, Augmented)
      }
    } else {
      val majorReference = size match {
        case Second => 2
        case Third => 4
        case Sixth => 9
        case Seventh => 11
        case _ => throw new IllegalArgumentException(s"$size is not a major/minor interval")
      }
      
      halfSteps.compare(majorReference) match {
        case -2 => (size, Diminished)
        case -1 => (size, Minor)
        case 0 => (size, Major)
        case 1 => (size, Augmented)
        case 2 => (size, Augmented)
        case _ => (size, Augmented)
      }
    }
    
    (size, quality)
  }
  
  val PerfectUnison: Interval = Interval(Unison, Perfect)
  val MinorSecond: Interval = Interval(Second, Minor)
  val MajorSecond: Interval = Interval(Second, Major)
  val MinorThird: Interval = Interval(Third, Minor)
  val MajorThird: Interval = Interval(Third, Major)
  val PerfectFourth: Interval = Interval(Fourth, Perfect)
  val PerfectFifth: Interval = Interval(Fifth, Perfect)
  val MinorSixth: Interval = Interval(Sixth, Minor)
  val MajorSixth: Interval = Interval(Sixth, Major)
  val MinorSeventh: Interval = Interval(Seventh, Minor)
  val MajorSeventh: Interval = Interval(Seventh, Major)
  val PerfectOctave: Interval = Interval(Octave, Perfect)