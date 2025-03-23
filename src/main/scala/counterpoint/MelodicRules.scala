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
    
  def afterTwoSkipsChangeDirectionRule(melody: Melody, candidate: Note): Boolean =
    if melody.toList.size < 2 then
      true
    else
      val notesList = melody.toList
      val lastNote = notesList.last
      val secondLastNote = notesList(notesList.size - 2)
      
      val lastIsSkip = isSkip(secondLastNote, lastNote)
      val secondLastIsSkip = 
        if notesList.size >= 3 then
          val thirdLastNote = notesList(notesList.size - 3)
          isSkip(thirdLastNote, secondLastNote)
        else
          false
          
      val lastDirection = getDirection(secondLastNote, lastNote)
      val candidateDirection = getDirection(lastNote, candidate)
      
      !(lastIsSkip && secondLastIsSkip && lastDirection == candidateDirection)