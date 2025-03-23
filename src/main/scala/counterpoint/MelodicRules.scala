package counterpoint

case class MelodicRules():
  def getIntervalSize(note1: Note, note2: Note): Int =
    if (note1.midiNumber <= note2.midiNumber)
      note1.intervalSize(note2)
    else
      note2.intervalSize(note1)
      
  def getSemitones(note1: Note, note2: Note): Int =
    math.abs(note1.midiNumber - note2.midiNumber) % 12
    
  def getDirection(from: Note, to: Note): Int =
    if (to.midiNumber > from.midiNumber) 1
    else if (to.midiNumber < from.midiNumber) -1
    else 0
    
  // Gets the simple interval size (reduced to within an octave)
  def getSimpleIntervalSize(note1: Note, note2: Note): Int =
    val interval = getIntervalSize(note1, note2)
    if interval == 8 || interval % 7 == 1 then
      if interval == 1 then 1 else 8  // Handle unison vs octave
    else
      // Compound intervals reduce to their simple form
      (interval - 1) % 7 + 1
    
  def isSkip(note1: Note, note2: Note): Boolean =
    val simpleInterval = getSimpleIntervalSize(note1, note2)
    simpleInterval == 3 || simpleInterval == 4
    
  def isLeap(note1: Note, note2: Note): Boolean =
    val simpleInterval = getSimpleIntervalSize(note1, note2)
    simpleInterval >= 5 && simpleInterval < 8  // Don't count octaves as leaps
  
  def isWithinOctaveRule(lastNote: Note, candidate: Note): Boolean =
    lastNote.isWithinOctave(candidate)
    
  def notASeventhRule(lastNote: Note, candidate: Note): Boolean =
    val simpleInterval = getSimpleIntervalSize(lastNote, candidate)
    simpleInterval != 7
    
  def notATritoneRule(lastNote: Note, candidate: Note): Boolean =
    getSemitones(lastNote, candidate) != 6
    
  def noSeventhInSameDirectionRule(
    secondLastNote: Note,
    lastNote: Note,
    candidate: Note
  ): Boolean =
    // Check the direction of both movements
    val lastMovementDirection = getDirection(secondLastNote, lastNote)
    val candidateMovementDirection = getDirection(lastNote, candidate)
    
    // If both movements are in the same direction
    if lastMovementDirection == candidateMovementDirection && lastMovementDirection != 0 then
      // Check that the interval from secondLastNote to candidate is not a seventh
      val simpleInterval = getSimpleIntervalSize(secondLastNote, candidate)
      simpleInterval != 7
    else
      // Rule doesn't apply if directions are different
      true
      
  def leapMustBePrecededByNoteInSpanRule(
    lastNote: Note,
    candidate: Note,
    secondLastNote: Option[Note]
  ): Boolean =
    // Check if the interval is a leap
    if isLeap(lastNote, candidate) then
      secondLastNote match
        case Some(secondLast) =>
          // If we have a second last note, check if it's inside the span
          val isInsideSpan = isNoteInsideSpan(secondLast, lastNote, candidate)
          isInsideSpan
        case None =>
          // If this is the first interval, no preceding note to check
          false
    else
      // Not a leap, so rule doesn't apply
      true
      
  def afterLeapNoteInSpanRule(
    secondLastNote: Note,
    lastNote: Note,
    candidate: Note
  ): Boolean =
    // Check if there's a leap between the second last and last notes
    if isLeap(secondLastNote, lastNote) then
      // If there was a leap, check if the candidate is within the span
      isNoteInsideSpan(candidate, secondLastNote, lastNote)
    else
      // Not a leap, so the rule doesn't apply
      true
      
  private def isNoteInsideSpan(middleNote: Note, fromNote: Note, toNote: Note): Boolean =
    // Check if middleNote's MIDI number is between fromNote and toNote
    if fromNote.midiNumber <= toNote.midiNumber then
      // Ascending interval
      fromNote.midiNumber < middleNote.midiNumber && middleNote.midiNumber < toNote.midiNumber
    else
      // Descending interval
      toNote.midiNumber < middleNote.midiNumber && middleNote.midiNumber < fromNote.midiNumber
    
  def afterTwoSkipsChangeDirectionRule(
    thirdLastNote: Note, 
    secondLastNote: Note, 
    lastNote: Note, 
    candidate: Note
  ): Boolean =
    val lastIsSkip = isSkip(secondLastNote, lastNote)
    val secondLastIsSkip = isSkip(thirdLastNote, secondLastNote)
    
    val secondLastDirection = getDirection(thirdLastNote, secondLastNote)
    val lastDirection = getDirection(secondLastNote, lastNote)
    val candidateDirection = getDirection(lastNote, candidate)
    
    // Check if both skips were in the same direction
    val skipsInSameDirection = secondLastDirection == lastDirection && secondLastDirection != 0
    
    // Only enforce rule if we have two skips in the same direction
    if lastIsSkip && secondLastIsSkip && skipsInSameDirection then
      // Must change direction (candidate direction must be different from the skip directions)
      candidateDirection != lastDirection || candidateDirection == 0  // Allow same note
    else
      true  // Rule doesn't apply