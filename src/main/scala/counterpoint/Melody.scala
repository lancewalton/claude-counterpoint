package counterpoint

case class Melody private (private val notes: List[Note]):
  def add(note: Note): Melody = new Melody(notes :+ note)
  
  def toList: List[Note] = notes
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = new Melody(notes)