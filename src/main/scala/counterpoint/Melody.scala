package counterpoint

case class Melody private (private val notes: List[Note]):
  def add(note: Note): Melody = new Melody(note :: notes)
  
  def toList: List[Note] = notes.reverse
  
  def isWithinOctaveRule(lastNote: Note, candidate: Note): Boolean =
    lastNote.isWithinOctave(candidate)
    
  def notASeventhRule(lastNote: Note, candidate: Note): Boolean =
    lastNote.intervalSize(candidate) != 7
  
  def validNextNotes: List[Note] =
    if notes.isEmpty then
      Note.allNotes
    else
      val lastNote = notes.head
      Note.allNotes
        .filter(note => isWithinOctaveRule(lastNote, note))
        .filter(note => notASeventhRule(lastNote, note))
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = new Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = {
    val reversed = notes.reverse
    new Melody(reversed)
  }
  
  def apply(notes: Note*): Melody = apply(notes.toList)