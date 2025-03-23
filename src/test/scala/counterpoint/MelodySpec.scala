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
    
    // Notes should be in order of adding (first note added is first in list)
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
      .add(Note.C4)  // First added
      .add(Note.D4)  // Second added
      .add(Note.E4)  // Third added
      
    // toList should return notes in the order they were added
    melody.toList should be(List(Note.C4, Note.D4, Note.E4))
    
    // toString should use the same order
    melody.toString should be("C4 D4 E4")
  }