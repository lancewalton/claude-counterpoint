package counterpoint

/**
 * DEPRECATED: This class is kept for backward compatibility with tests.
 * All functionality has been moved to the Interval companion object.
 * Use methods from Interval instead.
 */
@deprecated("Use methods from Interval companion object instead", "2.0.0")
object IntervalOps:
  def getDirection(from: Note, to: Note): Int =
    Interval.direction(from, to)
    
  def getInterval(note1: Note, note2: Note): Interval =
    Interval.between(note1, note2)
  
  def getIntervalSize(note1: Note, note2: Note): Int =
    Interval.size(note1, note2)
      
  def getSemitones(note1: Note, note2: Note): Int =
    Interval.semitones(note1, note2)
    
  def getSimpleIntervalSize(note1: Note, note2: Note): Int =
    Interval.simpleSize(note1, note2)
      
  def getSimpleInterval(interval: Interval): Interval =
    interval.toSimpleInterval
      
  def isSkip(note1: Note, note2: Note): Boolean =
    Interval.isSkip(note1, note2)
    
  def isLeap(note1: Note, note2: Note): Boolean =
    Interval.isLeap(note1, note2)
    
  def isMelodicStep(from: Note, to: Note): Boolean =
    Interval.isMelodicStep(from, to)
    
  def isPerfectFifth(interval: Interval): Boolean =
    interval.isPerfectFifth
    
  def isPerfectOctave(interval: Interval): Boolean =
    interval.isPerfectOctave
    
  def isNoteInsideSpan(middleNote: Note, fromNote: Note, toNote: Note): Boolean =
    Interval.isNoteInsideSpan(middleNote, fromNote, toNote)