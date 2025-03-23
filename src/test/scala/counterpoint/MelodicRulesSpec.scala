package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MelodicRulesSpec extends AnyFlatSpec with Matchers:
  "MelodicRules" should "calculate intervals from the lowest note" in {
    val rules = MelodicRules()
    
    // Ascending
    rules.getIntervalSize(Note.C4, Note.G4) should be(5)  // fifth
    
    // Descending
    rules.getIntervalSize(Note.G4, Note.C4) should be(5)  // still a fifth
    
    // Same note
    rules.getIntervalSize(Note.C4, Note.C4) should be(1)  // unison
  }
  
  it should "apply the within octave rule" in {
    val rules = MelodicRules()
    val note = Note.C4
    
    rules.isWithinOctaveRule(note, Note.C3) should be(true)
    rules.isWithinOctaveRule(note, Note.C5) should be(true)
    rules.isWithinOctaveRule(note, Note.D5) should be(false)
  }
  
  it should "apply the not a seventh rule" in {
    val rules = MelodicRules()
    
    // Ascending seventh
    rules.notASeventhRule(Note.C4, Note.B4) should be(false)
    rules.notASeventhRule(Note.C4, Note.G4) should be(true)
    
    // Descending seventh
    rules.notASeventhRule(Note.B4, Note.C4) should be(false)
    
    // Other intervals that should be disallowed
    rules.notASeventhRule(Note.D4, Note.C5) should be(false)
    rules.notASeventhRule(Note.F4, Note.E5) should be(false)
  }
  
  it should "identify tritones correctly" in {
    val rules = MelodicRules()
    
    // F to B is a tritone
    rules.getSemitones(Note.F4, Note.B4) should be(6)
    rules.notATritoneRule(Note.F4, Note.B4) should be(false)
    
    // B to F is also a tritone
    rules.getSemitones(Note.B3, Note.F4) should be(6)
    rules.notATritoneRule(Note.B3, Note.F4) should be(false)
    
    // C to F is a fourth, not a tritone
    rules.getSemitones(Note.C4, Note.F4) should be(5)
    rules.notATritoneRule(Note.C4, Note.F4) should be(true)
  }