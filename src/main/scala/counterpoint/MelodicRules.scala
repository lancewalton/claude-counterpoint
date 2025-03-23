package counterpoint

case class MelodicRules():
  
  def isWithinOctaveRule(lastNote: Note, candidate: Note): Boolean =
    lastNote.isWithinOctave(candidate)
    
  def notASeventhRule(lastNote: Note, candidate: Note): Boolean =
    val simpleInterval = Interval.simpleSize(lastNote, candidate)
    simpleInterval != 7  // Reject all kinds of sevenths (simple and compound)
    
  def notATritoneRule(lastNote: Note, candidate: Note): Boolean =
    Interval.semitones(lastNote, candidate) != 6
    
  def notConsecutiveRepeatedNotesRule(lastNote: Note, candidate: Note): Boolean =
    lastNote != candidate  // The candidate note cannot be the same as the last note
    
  def noSeventhInSameDirectionRule(
    secondLastNote: Note,
    lastNote: Note,
    candidate: Note
  ): Boolean =
    // Check the direction of both movements
    val lastMovementDirection = Interval.direction(secondLastNote, lastNote)
    val candidateMovementDirection = Interval.direction(lastNote, candidate)
    
    // If both movements are in the same direction
    if lastMovementDirection == candidateMovementDirection && lastMovementDirection != 0 then
      // Check that the interval from secondLastNote to candidate is not a seventh
      val simpleInterval = Interval.simpleSize(secondLastNote, candidate)
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
    if Interval.isLeap(lastNote, candidate) then
      secondLastNote match
        case Some(secondLast) =>
          // If we have a second last note, check if it's inside the span
          val isInsideSpan = Interval.isNoteInsideSpan(secondLast, lastNote, candidate)
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
    if Interval.isLeap(secondLastNote, lastNote) then
      // If there was a leap, check if the candidate is within the span
      Interval.isNoteInsideSpan(candidate, secondLastNote, lastNote)
    else
      // Not a leap, so the rule doesn't apply
      true
    
  def afterTwoSkipsChangeDirectionRule(
    thirdLastNote: Note, 
    secondLastNote: Note, 
    lastNote: Note, 
    candidate: Note
  ): Boolean =
    val lastIsSkip = Interval.isSkip(secondLastNote, lastNote)
    val secondLastIsSkip = Interval.isSkip(thirdLastNote, secondLastNote)
    
    val secondLastDirection = Interval.direction(thirdLastNote, secondLastNote)
    val lastDirection = Interval.direction(secondLastNote, lastNote)
    val candidateDirection = Interval.direction(lastNote, candidate)
    
    // Check if both skips were in the same direction
    val skipsInSameDirection = secondLastDirection == lastDirection && secondLastDirection != 0
    
    // Only enforce rule if we have two skips in the same direction
    if lastIsSkip && secondLastIsSkip && skipsInSameDirection then
      // Must change direction (candidate direction must be different from the skip directions)
      candidateDirection != lastDirection || candidateDirection == 0  // Allow same note
    else
      true  // Rule doesn't apply