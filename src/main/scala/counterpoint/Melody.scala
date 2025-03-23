package counterpoint

case class Melody private (private val notes: List[Note]):
  def add(note: Note): Melody = new Melody(note :: notes)
  
  def toList: List[Note] = notes.reverse
  
  override def toString: String = toList.mkString(" ")

object Melody:
  def empty: Melody = new Melody(List.empty)
  
  def apply(notes: List[Note]): Melody = {
    val reversed = notes.reverse
    new Melody(reversed)
  }
  
  def apply(notes: Note*): Melody = apply(notes.toList)