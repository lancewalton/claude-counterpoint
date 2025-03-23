package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MelodySpec extends AnyFlatSpec with Matchers:
  "Melody" should "start empty when created with empty" in {
    val melody = Melody.empty
    melody.toList should be(empty)
  }
  
  it should "allow adding notes" in {
    import NoteName.*
    
    val c4 = Note(C, 4)
    val d4 = Note(D, 4)
    val e4 = Note(E, 4)
    
    val melody = Melody.empty.add(c4).add(d4).add(e4)
    
    melody.toList should be(List(c4, d4, e4))
  }
  
  it should "be created with a list of notes" in {
    val notes = List(Note.C4, Note.D4, Note.E4)
    val melody = Melody(notes)
    
    melody.toList should be(notes)
  }
  
  it should "have a string representation" in {
    val melody = Melody(List(Note.C4, Note.D4, Note.E4))
    melody.toString should be("C4 D4 E4")
  }
  
  it should "handle toList method correctly" in {
    val melody = Melody.empty
      .add(Note.C4)
      .add(Note.D4)
      .add(Note.E4)
      
    melody.toList should be(List(Note.C4, Note.D4, Note.E4))
    
    melody.toString should be("C4 D4 E4")
  }
  
  it should "return C4 through B4 as valid next notes for an empty melody" in {
    val melody = Melody.empty
    val validNotes = melody.getValidNextNotes
    
    validNotes should contain (Note.C4)
    validNotes should contain (Note.D4)
    validNotes should contain (Note.E4)
    validNotes should contain (Note.F4)
    validNotes should contain (Note.G4)
    validNotes should contain (Note.A4)
    validNotes should contain (Note.B4)
    validNotes.size should be (7)
  }
  
  it should "return notes that are a step or skip away from the last note" in {
    val melodyWithC4 = Melody.empty.add(Note.C4)
    val validNotesAfterC4 = melodyWithC4.getValidNextNotes
    
    validNotesAfterC4 should contain (Note.B3)
    validNotesAfterC4 should contain (Note.D4)
    validNotesAfterC4 should contain (Note.A3)
    validNotesAfterC4 should contain (Note.E4)
    validNotesAfterC4 should not contain (Note.C4)
    validNotesAfterC4 should not contain (Note.F4)
    validNotesAfterC4 should not contain (Note.G3)
    
    val melodyWithG4 = Melody.empty.add(Note.G4)
    val validNotesAfterG4 = melodyWithG4.getValidNextNotes
    
    validNotesAfterG4 should contain (Note.F4)
    validNotesAfterG4 should contain (Note.A4)
    validNotesAfterG4 should contain (Note.E4)
    validNotesAfterG4 should contain (Note.B4)
    validNotesAfterG4 should not contain (Note.G4)
  }