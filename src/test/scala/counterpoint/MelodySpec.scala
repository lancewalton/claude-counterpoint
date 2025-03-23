package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MelodicRulesSpec extends AnyFlatSpec with Matchers:
  "MelodicRules" should "apply the within octave rule" in {
    val rules = MelodicRules()
    val note = Note.C4
    
    rules.isWithinOctaveRule(note, Note.C3) should be(true)
    rules.isWithinOctaveRule(note, Note.C5) should be(true)
    rules.isWithinOctaveRule(note, Note.D5) should be(false)
  }
  
  it should "apply the not a seventh rule" in {
    val rules = MelodicRules()
    
    rules.notASeventhRule(Note.C4, Note.B4) should be(false)
    rules.notASeventhRule(Note.C4, Note.G4) should be(true)
    
    rules.notASeventhRule(Note.D4, Note.C5) should be(false)
    rules.notASeventhRule(Note.F4, Note.E5) should be(false)
  }

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