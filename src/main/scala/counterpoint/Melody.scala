package counterpoint

case class Melody private (private val notesList: List[Note], val rules: MelodicRules = MelodicRules()):
  def add(note: Note): Melody = new Melody(note :: notesList, rules)
  def addNote(note: Note): Melody = add(note)
  
  def toList: List[Note] = notesList.reverse
  
  def notes: List[Note] = toList
  
  def isEmpty: Boolean = notesList.isEmpty
  
  def size: Int = notesList.size
  
  def lastNote: Option[Note] = notesList.headOption
  
  private def meetsWithinOctaveRule(candidateNote: Note): Boolean =
    val lastNote = notesList.head
    rules.isWithinOctaveRule(lastNote, candidateNote)
    
  private def meetsNoSeventhRule(candidateNote: Note): Boolean =
    val lastNote = notesList.head
    rules.notASeventhRule(lastNote, candidateNote)
    
  private def meetsNoTritoneRule(candidateNote: Note): Boolean =
    val lastNote = notesList.head
    rules.notATritoneRule(lastNote, candidateNote)
    
  private def meetsNotConsecutiveRepeatedNotesRule(candidateNote: Note): Boolean =
    val lastNote = notesList.head
    rules.notConsecutiveRepeatedNotesRule(lastNote, candidateNote)
    
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
      
  private def meetsNoSeventhInSameDirectionRule(candidateNote: Note): Boolean =
    toList match
      case secondLast :: last :: _ =>
        // Two or more notes, check the rule
        rules.noSeventhInSameDirectionRule(
          secondLast,
          last,
          candidateNote
        )
      case _ => true  // Not enough notes to trigger the rule
      
  private def meetsLeapPrecedenceRule(candidateNote: Note): Boolean =
    toList match
      case secondLast :: last :: _ =>
        // Two or more notes, check the rule with the second-last note
        rules.leapMustBePrecededByNoteInSpanRule(
          last,
          candidateNote,
          Some(secondLast)
        )
      case last :: Nil =>
        // Only one note, no preceding note to check against
        rules.leapMustBePrecededByNoteInSpanRule(
          last,
          candidateNote,
          None
        )
      case _ => true  // Empty melody, no notes to check against
  
  private def meetsAfterLeapNoteInSpanRule(candidateNote: Note): Boolean =
    toList match
      case secondLast :: last :: _ =>
        // Two or more notes, check the rule
        rules.afterLeapNoteInSpanRule(
          secondLast,
          last,
          candidateNote
        )
      case _ => true  // Not enough notes to trigger the rule
  
  def validNextNotes: List[Note] =
    if notesList.isEmpty then
      Note.allNotes
    else
      Note.allNotes
        .filter(meetsWithinOctaveRule)
        .filter(meetsNoSeventhRule)
        .filter(meetsNoTritoneRule)
        .filter(meetsNotConsecutiveRepeatedNotesRule)  
        .filter(meetsConsecutiveSkipsRule)
        .filter(meetsNoSeventhInSameDirectionRule)
        .filter(meetsLeapPrecedenceRule)
        .filter(meetsAfterLeapNoteInSpanRule)
        
  def validNextNotes(fromNote: Note): List[Note] =
    if notesList.isEmpty then
      Note.allNotes.filter(note => rules.isWithinOctaveRule(fromNote, note))
    else
      // Create a temporary melody with the fromNote as the last note
      val tempMelody = Melody.empty.add(fromNote)
      
      // Use the standard rules for the next note
      Note.allNotes
        .filter(note => rules.isWithinOctaveRule(fromNote, note))
        .filter(note => rules.notASeventhRule(fromNote, note))
        .filter(note => rules.notATritoneRule(fromNote, note))
        .filter(note => rules.notConsecutiveRepeatedNotesRule(fromNote, note))
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = new Melody(List.empty)
  
  def apply(): Melody = empty
  
  def apply(notes: List[Note]): Melody = {
    val reversed = notes.reverse
    new Melody(reversed)
  }
  
  def apply(notes: Note*): Melody = apply(notes.toList)