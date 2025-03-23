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
      thirdLastAscending, 
      secondLastAscending, 
      lastAscending, 
      Note.C5  // Going up
    ) should be(false)
    
    // After two skips up, going down should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastAscending, 
      secondLastAscending, 
      lastAscending, 
      Note.F4  // Going down
    ) should be(true)
    
    // After two skips up, staying on the same note should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastAscending, 
      secondLastAscending, 
      lastAscending, 
      Note.G4  // Same note
    ) should be(true)
    
    // Test descending pattern
    val thirdLastDescending = Note.G4
    val secondLastDescending = Note.E4  // Third down from G4
    val lastDescending = Note.C4       // Third down from E4
    
    // After two skips down, going down further should be disallowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastDescending, 
      secondLastDescending, 
      lastDescending, 
      Note.A3  // Going down
    ) should be(false)
    
    // After two skips down, going up should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastDescending, 
      secondLastDescending, 
      lastDescending, 
      Note.D4  // Going up
    ) should be(true)
    
    // Test mixed directions (rule shouldn't apply)
    val mixedDirectionThird = Note.C4
    val mixedDirectionSecond = Note.E4  // Third up
    val mixedDirectionLast = Note.C4    // Third down
    
    // When skips are in different directions, the rule shouldn't apply
    rules.afterTwoSkipsChangeDirectionRule(
      mixedDirectionThird,
      mixedDirectionSecond,
      mixedDirectionLast,
      Note.A3  // Going down
    ) should be(true)  // Allowed because the previous skips weren't in same direction
    
    // Test with non-skip intervals
    val nonSkipThird = Note.C4
    val nonSkipSecond = Note.D4  // Second (not a skip)
    val nonSkipLast = Note.G4    // Fourth (is a skip)
    
    // When one of the two previous intervals isn't a skip
    rules.afterTwoSkipsChangeDirectionRule(
      nonSkipThird,
      nonSkipSecond,
      nonSkipLast,
      Note.C5  // Going up
    ) should be(true)  // Allowed because we don't have two consecutive skips
  }
  
  it should "apply the consecutive skips not spanning seventh rule correctly" in {
    val rules = MelodicRules()
    
    // Test with ascending skips that would span a seventh
    val secondLastAscending = Note.E4
    val lastAscending = Note.G4       // Third up from E4
    
    // E4 -> G4 -> B4 would span E4 to B4, which is a fifth - allowed
    rules.consecutiveSkipsNotSpanningSeventhRule(
      secondLastAscending,
      lastAscending,
      Note.B4  // Third up from G4
    ) should be(true)
    
    // C4 -> E4 -> A4 would span C4 to A4, which is a sixth - allowed
    rules.consecutiveSkipsNotSpanningSeventhRule(
      Note.C4,
      Note.E4,  // Third up from C4
      Note.A4   // Fourth up from E4, this is a skip, total span is a sixth (C4 -> A4)
    ) should be(true)
    
    // More relevant test - C4 -> E4 -> A4 are two skips spanning a sixth (allowed)
    // But if we add one more skip to C5, the span from C4 to C5 would be an octave (disallowed)
    rules.consecutiveSkipsNotSpanningSeventhRule(
      Note.E4,  // First note of second consecutive skip
      Note.A4,  // Second note
      Note.C5   // Candidate note - skip up from A4, creates span of a sixth (E4 -> C5)
    ) should be(true)  // Sixth is allowed
    
    // C4 -> E4 -> G4 would span C4 to G4, which is a fifth - allowed
    rules.consecutiveSkipsNotSpanningSeventhRule(
      Note.C4,
      Note.E4,  // Third up from C4
      Note.G4   // Third up from E4, total span is a fifth (C4 -> G4)
    ) should be(true)
    
    // Test with descending skips that would span a seventh
    val secondLastDescending = Note.G4
    val lastDescending = Note.E4       // Third down from G4
    
    // G4 -> E4 -> C4 would span G4 to C4, which is a fifth - allowed
    rules.consecutiveSkipsNotSpanningSeventhRule(
      secondLastDescending,
      lastDescending,
      Note.C4  // Third down from E4
    ) should be(true)
    
    // B4 -> G4 -> D4 would span B4 to D4, which is a sixth - allowed
    rules.consecutiveSkipsNotSpanningSeventhRule(
      Note.B4,
      Note.G4,  // Third down from B4
      Note.D4   // Fourth down from G4, total span is a sixth (B4 -> D4)
    ) should be(true)
    
    // B4 -> G4 -> D4 are two skips spanning a sixth (allowed)
    // But a theoretical B4 -> G4 -> D4 -> G3 would span B4 to G3, which is greater than a seventh
    rules.consecutiveSkipsNotSpanningSeventhRule(
      Note.G4,  // First note of second consecutive skip 
      Note.D4,  // Second note
      Note.A3   // Fourth down from D4, this span would be a seventh (G4 -> A3)
    ) should be(false)  // Seventh is disallowed
    
    // Test with mixed direction skips (rule shouldn't apply)
    val mixedDirectionSecond = Note.E4
    val mixedDirectionLast = Note.C4    // Third down from E4
    
    // When skips are in different directions, the rule shouldn't apply
    rules.consecutiveSkipsNotSpanningSeventhRule(
      mixedDirectionSecond,
      mixedDirectionLast,
      Note.E3  // Fifth down from C4 but different direction than E4->C4
    ) should be(true)  // Allowed because the skips aren't in same direction
    
    // Test with non-skip intervals
    val nonSkipSecond = Note.D4  // Second (not a skip)
    val nonSkipLast = Note.E4    // Second (not a skip)
    
    // When one of the intervals isn't a skip
    rules.consecutiveSkipsNotSpanningSeventhRule(
      nonSkipSecond,
      nonSkipLast,
      Note.A4  // Skip up from E4
    ) should be(true)  // Allowed because the first interval isn't a skip
  }