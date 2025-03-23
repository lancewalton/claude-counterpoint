package counterpoint

case class CounterpointRules():
  /**
   * Maximum allowed distance between adjacent voices
   */
  val MaxAdjacentVoiceDistance = 12 // One octave
  
  /**
   * List of consonant intervals in counterpoint
   * Perfect unisons, perfect fifths, perfect octaves, 
   * major and minor thirds, and major and minor sixths
   */
  val ConsonantIntervals = Set(
    (IntervalName.Unison, IntervalQuality.Perfect),
    (IntervalName.Third, IntervalQuality.Major),
    (IntervalName.Third, IntervalQuality.Minor),
    (IntervalName.Fifth, IntervalQuality.Perfect),
    (IntervalName.Sixth, IntervalQuality.Major),
    (IntervalName.Sixth, IntervalQuality.Minor),
    (IntervalName.Octave, IntervalQuality.Perfect)
  )
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
   * Rule to check if adding the candidate notes would create parallel octaves.
   * Parallel octaves occur when both voices move in the same direction and the
   * interval between them is an octave both before and after the movement.
   */
  def noParallelOctavesRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val lastMovementDirection = getDirection(lowerVoiceLastNote, lowerVoiceCandidateNote)
    val upperMovementDirection = getDirection(upperVoiceLastNote, upperVoiceCandidateNote)
    
    // If both voices move in the same direction
    if lastMovementDirection == upperMovementDirection && lastMovementDirection != 0 then
      // Check if both intervals are octaves
      val lastInterval = getInterval(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = getInterval(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      !(isPerfectOctave(lastInterval) && isPerfectOctave(candidateInterval))
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
   * Rule to prohibit direct movement to a perfect consonance (fifth or octave).
   * Direct (or similar) motion to a perfect consonance happens when both voices move in
   * the same direction and end on a perfect consonance (perfect fifth or octave).
   */
  def noDirectMotionToPerfectConsonanceRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val lowerDirection = getDirection(lowerVoiceLastNote, lowerVoiceCandidateNote)
    val upperDirection = getDirection(upperVoiceLastNote, upperVoiceCandidateNote)
    
    // If both voices move in the same direction
    if lowerDirection == upperDirection && lowerDirection != 0 then
      // Check if the resulting interval is a perfect consonance (fifth or octave)
      val resultingInterval = getInterval(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      val simpleInterval = getSimpleInterval(resultingInterval)
      
      !(isPerfectFifth(resultingInterval) || isPerfectOctave(resultingInterval))
    else
      // If voices move in different directions or one of them doesn't move,
      // it's not direct motion, so this rule doesn't apply
      true

  /**
   * Helper method to check if an interval is a perfect fifth.
   */
  private def isPerfectFifth(interval: Interval): Boolean =
    val simpleInterval = getSimpleInterval(interval)
    simpleInterval.name == IntervalName.Fifth && simpleInterval.quality == IntervalQuality.Perfect
    
  /**
   * Helper method to check if an interval is a perfect octave.
   */
  private def isPerfectOctave(interval: Interval): Boolean =
    val simpleInterval = getSimpleInterval(interval)
    simpleInterval.name == IntervalName.Octave && simpleInterval.quality == IntervalQuality.Perfect
    
  /**
   * Rule to prevent voice crossing.
   * The upper voice should always be higher in pitch than the lower voice.
   */
  def noVoiceCrossingRule(
    lowerVoiceNote: Note,
    upperVoiceNote: Note
  ): Boolean =
    upperVoiceNote.midiNumber > lowerVoiceNote.midiNumber
    
  /**
   * Rule to maintain proper voice spacing.
   * Adjacent voices should not be too far apart.
   */
  def properVoiceSpacingRule(
    lowerVoiceNote: Note,
    upperVoiceNote: Note
  ): Boolean =
    val semitones = math.abs(upperVoiceNote.midiNumber - lowerVoiceNote.midiNumber)
    semitones <= MaxAdjacentVoiceDistance
    
  /**
   * Rule to ensure consonance between voices.
   * In strict counterpoint, intervals between voices should be consonant.
   */
  def consonantIntervalRule(
    lowerVoiceNote: Note,
    upperVoiceNote: Note
  ): Boolean =
    val interval = getInterval(lowerVoiceNote, upperVoiceNote)
    val simpleInterval = getSimpleInterval(interval)
    
    // Check if the simple interval is in our list of consonant intervals
    ConsonantIntervals.contains((simpleInterval.name, simpleInterval.quality))
    
  /**
   * Rule for proper resolution of dissonances.
   * Dissonances should resolve properly, typically by step in the direction set up by the dissonance.
   */
  def dissonanceResolutionRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val lastInterval = getInterval(lowerVoiceLastNote, upperVoiceLastNote)
    
    // If the last interval was consonant, this rule doesn't apply
    if isConsonant(lastInterval) then
      true
    else
      // For dissonant intervals, verify proper resolution
      // Typically, the upper voice should resolve down by step
      val upperDirection = getDirection(upperVoiceLastNote, upperVoiceCandidateNote)
      val isStepResolution = isMelodicStep(upperVoiceLastNote, upperVoiceCandidateNote)
      
      // In first species, we generally don't allow dissonances,
      // but for other species, this would check if the resolution is proper
      upperDirection == -1 && isStepResolution && 
      isConsonant(getInterval(lowerVoiceCandidateNote, upperVoiceCandidateNote))
    
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
      
  /**
   * Helper method to check if an interval is consonant in counterpoint.
   */
  private def isConsonant(interval: Interval): Boolean =
    val simpleInterval = getSimpleInterval(interval)
    ConsonantIntervals.contains((simpleInterval.name, simpleInterval.quality))
    
  /**
   * Helper method to check if a melodic movement is a step.
   * A step is a melodic interval of a second (major or minor).
   */
  private def isMelodicStep(from: Note, to: Note): Boolean =
    val semitones = math.abs(to.midiNumber - from.midiNumber)
    semitones == 1 || semitones == 2  // minor or major second