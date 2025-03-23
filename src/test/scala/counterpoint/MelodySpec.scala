package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MelodySpec extends AnyFlatSpec with Matchers:
  "Melody" should "start empty when created with empty" in {
    val melody = Melody.empty
    melody.notes should be(empty)
  }
  
  it should "allow adding notes" in {
    import NoteName.*
    
    val c4 = Note(C, 4)
    val d4 = Note(D, 4)
    val e4 = Note(E, 4)
    
    val melody = Melody.empty.add(e4).add(d4).add(c4)
    
    // Notes should be in reverse order of adding because we're prepending
    melody.notes should be(List(c4, d4, e4))
  }
  
  it should "be created with a list of notes" in {
    val notes = List(Note.C4, Note.D4, Note.E4)
    val melody = Melody(notes)
    
    melody.notes should be(notes)
  }
  
  it should "have a string representation" in {
    val melody = Melody(List(Note.C4, Note.D4, Note.E4))
    melody.toString should be("C4 D4 E4")
  }