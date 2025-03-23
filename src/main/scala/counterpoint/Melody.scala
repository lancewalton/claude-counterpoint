package counterpoint

case class Melody private (private val notes: List[Note], val rules: MelodicRules = MelodicRules()):
  def add(note: Note): Melody = new Melody(note :: notes, rules)
  
  def toList: List[Note] = notes.reverse
  
  private def meetsWithinOctaveRule(candidateNote: Note): Boolean =
    val lastNote = notes.head
    rules.isWithinOctaveRule(lastNote, candidateNote)
    
  private def meetsNoSeventhRule(candidateNote: Note): Boolean =
    val lastNote = notes.head
    rules.notASeventhRule(lastNote, candidateNote)
    
  private def meetsNoTritoneRule(candidateNote: Note): Boolean =
    val lastNote = notes.head
    rules.notATritoneRule(lastNote, candidateNote)
    
  private def meetsConsecutiveSkipsRule(candidateNote: Note): Boolean =
    toList match
      case thirdLast :: secondLast :: last :: _ =>
        // Three or more notes, check the rule
        rules.afterTwoSkipsChangeDirectionRule(
          thirdLast,
          secondLast,
          last,
          candidateNote
        )
      case _ => true  // Not enough notes to trigger the rule
      
  private def meetsConsecutiveSkipsSpanRule(candidateNote: Note): Boolean =
    toList match
      case thirdLast :: secondLast :: last :: _ =>
        // Three or more notes, check the rule
        rules.consecutiveSkipsNotSpanningSeventhRule(
          thirdLast,
          secondLast,
          last,
          candidateNote
        )
      case _ => true  // Not enough notes to trigger the rule
  
  def validNextNotes: List[Note] =
    if notes.isEmpty then
      Note.allNotes
    else
      Note.allNotes
        .filter(meetsWithinOctaveRule)
        .filter(meetsNoSeventhRule)
        .filter(meetsNoTritoneRule)
        .filter(meetsConsecutiveSkipsRule)
        .filter(meetsConsecutiveSkipsSpanRule)
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = new Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = {
    val reversed = notes.reverse
    new Melody(reversed)
  }
  
  def apply(notes: Note*): Melody = apply(notes.toList)