package counterpoint

/**
 * Represents a musical melody as a sequence of notes.
 *
 * @param notes The list of notes that make up the melody (private)
 */
case class Melody private (private val notes: List[Note]):
  /**
   * Creates a new melody by adding a note to the end of this melody.
   *
   * @param note The note to add to the melody
   * @return A new melody with the note added at the end
   */
  def add(note: Note): Melody = new Melody(notes :+ note)
  
  /**
   * Returns the list of notes in the order they were added
   * 
   * @return A list of notes in the order they were added
   */
  def toList: List[Note] = notes
  
  /**
   * String representation of the melody
   */
  override def toString: String = toList.mkString(" ")

object Melody:
  /**
   * Creates an empty melody with no notes
   */
  def empty: Melody = Melody(List.empty)
  
  /**
   * Creates a melody from a list of notes
   * The order of notes in the list is preserved
   */
  def apply(notes: List[Note]): Melody = new Melody(notes)