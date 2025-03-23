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
  
  it should "determine melodic direction correctly" in {
    val rules = MelodicRules()
    
    // Ascending
    rules.getDirection(Note.C4, Note.E4) should be(1)
    
    // Descending
    rules.getDirection(Note.G4, Note.D4) should be(-1)
    
    // Same note
    rules.getDirection(Note.C4, Note.C4) should be(0)
  }
  
  it should "identify skips correctly" in {
    val rules = MelodicRules()
    
    // Thirds are skips
    rules.isSkip(Note.C4, Note.E4) should be(true)  // major third
    rules.isSkip(Note.A4, Note.C5) should be(true)  // minor third
    
    // Fourths are skips
    rules.isSkip(Note.C4, Note.F4) should be(true)  // perfect fourth
    
    // Seconds are not skips
    rules.isSkip(Note.C4, Note.D4) should be(false)  // major second
    
    // Fifths are not skips
    rules.isSkip(Note.C4, Note.G4) should be(false)  // perfect fifth
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
  
  it should "apply the two skips rule correctly" in {
    val rules = MelodicRules()
    
    // Test ascending pattern
    val thirdLastAscending = Note.C4
    val secondLastAscending = Note.E4  // Third up from C4
    val lastAscending = Note.G4       // Third up from E4
    
    // After two skips up, going up further should be disallowed
    rules.afterTwoSkipsChangeDirectionRule(
      Some(thirdLastAscending), 
      secondLastAscending, 
      lastAscending, 
      Note.C5  // Going up
    ) should be(false)
    
    // After two skips up, going down should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      Some(thirdLastAscending), 
      secondLastAscending, 
      lastAscending, 
      Note.F4  // Going down
    ) should be(true)
    
    // Test descending pattern
    val thirdLastDescending = Note.G4
    val secondLastDescending = Note.E4  // Third down from G4
    val lastDescending = Note.C4       // Third down from E4
    
    // After two skips down, going down further should be disallowed
    rules.afterTwoSkipsChangeDirectionRule(
      Some(thirdLastDescending), 
      secondLastDescending, 
      lastDescending, 
      Note.A3  // Going down
    ) should be(false)
    
    // After two skips down, going up should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      Some(thirdLastDescending), 
      secondLastDescending, 
      lastDescending, 
      Note.D4  // Going up
    ) should be(true)
    
    // With fewer than 3 notes, the rule shouldn't apply
    rules.afterTwoSkipsChangeDirectionRule(
      None,
      Note.E4,
      Note.G4,
      Note.C5
    ) should be(true)
  }