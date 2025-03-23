package counterpoint

case class Melody private (private val notes: List[Note], val rules: MelodicRules = MelodicRules()):
  def add(note: Note): Melody = new Melody(note :: notes, rules)
  
  def toList: List[Note] = notes.reverse
  
  def validNextNotes: List[Note] =
    if notes.isEmpty then
      Note.allNotes
    else
      val lastNote = notes.head
      Note.allNotes
        .filter(note => rules.isWithinOctaveRule(lastNote, note))
        .filter(note => rules.notASeventhRule(lastNote, note))
        .filter(note => rules.notATritoneRule(lastNote, note))
        .filter(note => rules.afterTwoSkipsChangeDirectionRule(this, note))
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = new Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = {
    val reversed = notes.reverse
    new Melody(reversed)
  }
  
  def apply(notes: Note*): Melody = apply(notes.toList)