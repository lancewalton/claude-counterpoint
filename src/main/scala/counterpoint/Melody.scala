package counterpoint

/**
 * Represents a musical melody as a sequence of notes.
 *
 * @param notes The list of notes that make up the melody
 */
case class Melody(notes: List[Note]):
  /**
   * Creates a new melody by adding a note to the beginning of this melody.
   *
   * @param note The note to add to the melody
   * @return A new melody with the note added at the beginning
   */
  def add(note: Note): Melody = Melody(note :: notes)
  
  /**
   * String representation of the melody
   */
  override def toString: String = notes.mkString(" ")

object Melody:
  /**
   * Creates an empty melody with no notes
   */
  def empty: Melody = Melody(List.empty)