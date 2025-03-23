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
    
  def isSkip(note1: Note, note2: Note): Boolean =
    val interval = getIntervalSize(note1, note2)
    interval == 3 || interval == 4
  
  def isWithinOctaveRule(lastNote: Note, candidate: Note): Boolean =
    lastNote.isWithinOctave(candidate)
    
  def notASeventhRule(lastNote: Note, candidate: Note): Boolean =
    getIntervalSize(lastNote, candidate) != 7
    
  def notATritoneRule(lastNote: Note, candidate: Note): Boolean =
    getSemitones(lastNote, candidate) != 6
    
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