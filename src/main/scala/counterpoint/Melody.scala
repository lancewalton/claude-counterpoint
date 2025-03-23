package counterpoint

case class Melody private (private val notes: List[Note]):
  def add(note: Note): Melody = new Melody(notes :+ note)
  
  def toList: List[Note] = notes
  
  def getValidNextNotes: Set[Note] = {
    if (notes.isEmpty) {
      Set(Note.C4, Note.D4, Note.E4, Note.F4, Note.G4, Note.A4, Note.B4)
    } else {
      val lastNote = notes.last
      import NoteName.*
      val allNotes = Note.cMajorScale.toSet
      
      allNotes.filter(candidate => {
        val interval = Math.abs(Note.cMajorScale.indexOf(candidate) - Note.cMajorScale.indexOf(lastNote))
        
        interval match {
          case 0 => false
          case 1 => true
          case 2 => true
          case _ => false
        }
      })
    }
  }
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = new Melody(notes)