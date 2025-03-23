package counterpoint

case class CounterpointRules():
  /**
   * Rule to check if adding the candidate notes would create parallel perfect fifths.
   * Parallel perfect fifths occur when both voices move in the same direction and the
   * interval between them is a perfect fifth both before and after the movement.
   */
  def noParallelPerfectFifthsRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val lastMovementDirection = getDirection(lowerVoiceLastNote, lowerVoiceCandidateNote)
    val upperMovementDirection = getDirection(upperVoiceLastNote, upperVoiceCandidateNote)
    
    // If both voices move in the same direction
    if lastMovementDirection == upperMovementDirection && lastMovementDirection != 0 then
      // Check if both intervals are perfect fifths
      val lastInterval = getInterval(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = getInterval(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      !(isPerfectFifth(lastInterval) && isPerfectFifth(candidateInterval))
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true
      
  /**
   * Helper method to determine the direction of movement from one note to another.
   * Returns 1 for ascending, -1 for descending, 0 for no movement.
   */
  private def getDirection(from: Note, to: Note): Int =
    if (to.midiNumber > from.midiNumber) 1
    else if (to.midiNumber < from.midiNumber) -1
    else 0
    
  /**
   * Helper method to get the interval between two notes.
   * Always calculates from the lower note to the higher note.
   */
  private def getInterval(note1: Note, note2: Note): Interval =
    if (note1.midiNumber <= note2.midiNumber)
      note1.interval(note2)
    else
      note2.interval(note1)
      
  /**
   * Helper method to check if an interval is a perfect fifth.
   */
  private def isPerfectFifth(interval: Interval): Boolean =
    val simpleInterval = getSimpleInterval(interval)
    simpleInterval.name == IntervalName.Fifth && simpleInterval.quality == IntervalQuality.Perfect
    
  /**
   * Helper method to reduce a compound interval to its simple form.
   */
  private def getSimpleInterval(interval: Interval): Interval =
    if interval.isCompound then
      // Get the simple interval properties
      val simpleNumber = (interval.number - 1) % 7 + 1
      val simpleName = interval.name.simpleIntervalName
      val simpleSemitones = interval.semitones % 12
      
      // Create a new interval with the simple properties but preserve the quality
      Interval(simpleNumber, simpleName, interval.quality, simpleSemitones)
    else
      interval