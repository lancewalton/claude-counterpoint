package counterpoint

object IntervalOps:
  def getDirection(from: Note, to: Note): Int =
    if (to.midiNumber > from.midiNumber) 1
    else if (to.midiNumber < from.midiNumber) -1
    else 0
    
  def getInterval(note1: Note, note2: Note): Interval =
    if (note1.midiNumber <= note2.midiNumber)
      note1.interval(note2)
    else
      note2.interval(note1)
  
  def getIntervalSize(note1: Note, note2: Note): Int =
    if (note1.midiNumber <= note2.midiNumber)
      note1.intervalSize(note2)
    else
      note2.intervalSize(note1)
      
  def getSemitones(note1: Note, note2: Note): Int =
    math.abs(note1.midiNumber - note2.midiNumber) % 12
    
  def getSimpleIntervalSize(note1: Note, note2: Note): Int =
    val interval = getIntervalSize(note1, note2)
    if interval == 8 || interval % 7 == 1 then
      if interval == 1 then 1 else 8  // Handle unison vs octave
    else
      // Compound intervals reduce to their simple form
      (interval - 1) % 7 + 1
      
  def getSimpleInterval(interval: Interval): Interval =
    if interval.isCompound then
      // Get the simple interval properties
      val simpleNumber = (interval.number - 1) % 7 + 1
      val simpleName = interval.name.simpleIntervalName
      val simpleSemitones = interval.semitones % 12
      
      // Create a new interval with the simple properties but preserve the quality
      Interval(simpleNumber, simpleName, interval.quality, simpleSemitones)
    else
      interval
      
  def isSkip(note1: Note, note2: Note): Boolean =
    val interval = getIntervalSize(note1, note2)
    interval == 3 || interval == 4  // Only actual thirds and fourths are skips, not compound versions
    
  def isLeap(note1: Note, note2: Note): Boolean =
    val simpleInterval = getSimpleIntervalSize(note1, note2)
    simpleInterval >= 5  // Any interval of a fifth or larger is a leap (including octave)
    
  def isMelodicStep(from: Note, to: Note): Boolean =
    val semitones = math.abs(to.midiNumber - from.midiNumber)
    semitones == 1 || semitones == 2  // minor or major second
    
  def isPerfectFifth(interval: Interval): Boolean =
    val simpleInterval = getSimpleInterval(interval)
    simpleInterval.name == IntervalName.Fifth && simpleInterval.quality == IntervalQuality.Perfect
    
  def isPerfectOctave(interval: Interval): Boolean =
    val simpleInterval = getSimpleInterval(interval)
    simpleInterval.name == IntervalName.Octave && simpleInterval.quality == IntervalQuality.Perfect
    
  def isNoteInsideSpan(middleNote: Note, fromNote: Note, toNote: Note): Boolean =
    // Check if middleNote's MIDI number is between fromNote and toNote
    if fromNote.midiNumber <= toNote.midiNumber then
      // Ascending interval
      fromNote.midiNumber < middleNote.midiNumber && middleNote.midiNumber < toNote.midiNumber
    else
      // Descending interval
      toNote.midiNumber < middleNote.midiNumber && middleNote.midiNumber < fromNote.midiNumber