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
    val melody = Melody(List(Note.C4, Note.D4, Note.E4))
    
    melody.toList should be(List(Note.C4, Note.D4, Note.E4))
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
  
  it should "support varargs in the constructor" in {
    val melody = Melody(Note.C4, Note.D4, Note.E4)
    
    melody.toList should be(List(Note.C4, Note.D4, Note.E4))
  }
  
  it should "provide a list of valid next notes" in {
    val emptyMelody = Melody.empty
    emptyMelody.validNextNotes should be(Note.allNotes)
    
    val melodyWithC4 = Melody.empty.add(Note.C4)
    melodyWithC4.validNextNotes should contain(Note.C5)
    melodyWithC4.validNextNotes should contain(Note.C3)
    
    val melodyWithG4 = Melody.empty.add(Note.G4)
    melodyWithG4.validNextNotes should contain(Note.G5)
    melodyWithG4.validNextNotes should contain(Note.G3)
    
    val melodyWithHighNote = Melody.empty.add(Note.G5)
    // G5 should have valid next notes
    melodyWithHighNote.validNextNotes should contain(Note.G4)
  }
  
  it should "use melodic rules to filter valid next notes" in {
    val melody = Melody.empty.add(Note.C4)
    melody.validNextNotes should contain(Note.G4)
    melody.validNextNotes should not contain(Note.B4)  // seventh interval
    melody.validNextNotes should not contain(Note.D5)  // outside octave
  }
  
  it should "disallow tritones" in {
    val melodyWithF4 = Melody.empty.add(Note.F4)
    melodyWithF4.validNextNotes should not contain(Note.B4)  // F to B is a tritone
    
    val melodyWithB3 = Melody.empty.add(Note.B3)
    melodyWithB3.validNextNotes should not contain(Note.F4)  // B to F is a tritone
  }
  
  it should "enforce the consecutive skips rule" in {
    // Create a melody with two consecutive ascending skips
    val melodyWithTwoAscendingSkips = Melody.empty
      .add(Note.C4)
      .add(Note.E4)  // First skip (third up)
      .add(Note.G4)  // Second skip (third up)
    
    // Valid next notes should contain notes that go down
    melodyWithTwoAscendingSkips.validNextNotes should contain(Note.F4)  // Step down
    melodyWithTwoAscendingSkips.validNextNotes should contain(Note.E4)  // Skip down
    
    // Valid next notes should not contain notes that go up
    melodyWithTwoAscendingSkips.validNextNotes should not contain(Note.A4)  // Step up
    melodyWithTwoAscendingSkips.validNextNotes should not contain(Note.C5)  // Skip up
    
    // Create a melody with two consecutive descending skips
    val melodyWithTwoDescendingSkips = Melody.empty
      .add(Note.G4)
      .add(Note.E4)  // First skip (third down)
      .add(Note.C4)  // Second skip (third down)
    
    // Valid next notes should contain notes that go up
    melodyWithTwoDescendingSkips.validNextNotes should contain(Note.D4)  // Step up
    melodyWithTwoDescendingSkips.validNextNotes should contain(Note.E4)  // Skip up
    
    // Valid next notes should not contain notes that go down
    melodyWithTwoDescendingSkips.validNextNotes should not contain(Note.B3)  // Step down
    melodyWithTwoDescendingSkips.validNextNotes should not contain(Note.G3)  // Skip down
  }