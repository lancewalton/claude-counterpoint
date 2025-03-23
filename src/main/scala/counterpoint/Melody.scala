package counterpoint

case class Melody private (private val notes: List[Note], val rules: MelodicRules = MelodicRules()):
  def add(note: Note): Melody = new Melody(note :: notes, rules)
  
  def toList: List[Note] = notes.reverse
  
  private def getLastThreeNotes: (Option[Note], Option[Note], Option[Note]) =
    val allNotes = toList
    val size = allNotes.size
    
    val lastNote = if size >= 1 then Some(allNotes(size - 1)) else None
    val secondLastNote = if size >= 2 then Some(allNotes(size - 2)) else None
    val thirdLastNote = if size >= 3 then Some(allNotes(size - 3)) else None
    
    (thirdLastNote, secondLastNote, lastNote)
  
  def validNextNotes: List[Note] =
    if notes.isEmpty then
      Note.allNotes
    else
      val lastNote = notes.head
      val (thirdLastOpt, secondLastOpt, _) = getLastThreeNotes
      
      Note.allNotes
        .filter(note => rules.isWithinOctaveRule(lastNote, note))
        .filter(note => rules.notASeventhRule(lastNote, note))
        .filter(note => rules.notATritoneRule(lastNote, note))
        .filter(note => 
          secondLastOpt match
            case None => true
            case Some(secondLastNote) => 
              rules.afterTwoSkipsChangeDirectionRule(
                thirdLastOpt, 
                secondLastNote, 
                lastNote, 
                note
              )
        )
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = new Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = {
    val reversed = notes.reverse
    new Melody(reversed)
  }
  
  def apply(notes: Note*): Melody = apply(notes.toList)