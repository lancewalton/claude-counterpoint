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
    
    // Fifths are skips
    rules.isSkip(Note.C4, Note.G4) should be(true)  // perfect fifth
    
    // Sixths are skips
    rules.isSkip(Note.C4, Note.A4) should be(true)  // major sixth
    
    // Sevenths are skips
    rules.isSkip(Note.C4, Note.B4) should be(true)  // major seventh
    
    // Octaves are skips (C4 to C5 is 12 semitones, which is considered an octave, not beyond)
    rules.isSkip(Note.C4, Note.C5) should be(false)  // octave (not a skip based on our rule interpretation)
    
    // Thirds are not skips
    rules.isSkip(Note.C4, Note.E4) should be(false)  // major third
    rules.isSkip(Note.A4, Note.C5) should be(false)  // minor third
    
    // Fourths are not skips
    rules.isSkip(Note.C4, Note.F4) should be(false)  // perfect fourth
    
    // Seconds are not skips
    rules.isSkip(Note.C4, Note.D4) should be(false)  // major second
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
    
    // Test ascending pattern with new skip definition (fifth or larger)
    val thirdLastAscending = Note.C4
    val secondLastAscending = Note.G4  // Fifth up from C4
    val lastAscending = Note.D5       // Fifth up from G4
    
    // After two skips up, going up further should be disallowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastAscending, 
      secondLastAscending, 
      lastAscending, 
      Note.G5  // Going up (G5 is the highest note in our range)
    ) should be(false)
    
    // After two skips up, going down should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastAscending, 
      secondLastAscending, 
      lastAscending, 
      Note.G4  // Going down
    ) should be(true)
    
    // After two skips up, staying on the same note should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastAscending, 
      secondLastAscending, 
      lastAscending, 
      Note.D5  // Same note
    ) should be(true)
    
    // Test descending pattern
    val thirdLastDescending = Note.D5
    val secondLastDescending = Note.G4  // Fifth down from D5
    val lastDescending = Note.C4       // Fifth down from G4
    
    // After two skips down, going down further should be disallowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastDescending, 
      secondLastDescending, 
      lastDescending, 
      Note.F3  // Going down
    ) should be(false)
    
    // After two skips down, going up should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastDescending, 
      secondLastDescending, 
      lastDescending, 
      Note.G4  // Going up
    ) should be(true)
    
    // Test mixed directions (rule shouldn't apply)
    val mixedDirectionThird = Note.C4
    val mixedDirectionSecond = Note.G4  // Fifth up
    val mixedDirectionLast = Note.C4    // Fifth down
    
    // When skips are in different directions, the rule shouldn't apply
    rules.afterTwoSkipsChangeDirectionRule(
      mixedDirectionThird,
      mixedDirectionSecond,
      mixedDirectionLast,
      Note.F3  // Going down
    ) should be(true)  // Allowed because the previous skips weren't in same direction
    
    // Test with non-skip intervals (now a fourth is not a skip)
    val nonSkipThird = Note.C4
    val nonSkipSecond = Note.F4  // Fourth (not a skip with new definition)
    val nonSkipLast = Note.C5    // Fifth (is a skip)
    
    // When one of the two previous intervals isn't a skip
    rules.afterTwoSkipsChangeDirectionRule(
      nonSkipThird,
      nonSkipSecond,
      nonSkipLast,
      Note.G5  // Going up (G5 is the highest note in our range)
    ) should be(true)  // Allowed because we don't have two consecutive skips
  }
  
  it should "apply the no seventh in same direction rule correctly" in {
    val rules = MelodicRules()
    
    // Test ascending pattern
    val secondLastAscending = Note.C4
    val lastAscending = Note.E4       // Third up from C4
    
    // C4 -> E4 -> B4 would span C4 to B4, which is a seventh - disallowed
    rules.noSeventhInSameDirectionRule(
      secondLastAscending,
      lastAscending,
      Note.B4  // Fifth up from E4, but seventh from C4
    ) should be(false)
    
    // C4 -> E4 -> A4 would span C4 to A4, which is a sixth - allowed
    rules.noSeventhInSameDirectionRule(
      secondLastAscending,
      lastAscending,
      Note.A4  // Fourth up from E4, sixth from C4
    ) should be(true)
    
    // C4 -> E4 -> G4 would span C4 to G4, which is a fifth - allowed
    rules.noSeventhInSameDirectionRule(
      secondLastAscending,
      lastAscending,
      Note.G4  // Third up from E4, fifth from C4
    ) should be(true)
    
    // Test descending pattern
    val secondLastDescending = Note.B4
    val lastDescending = Note.G4       // Third down from B4
    
    // B4 -> G4 -> C4 would span B4 to C4, which is a seventh - disallowed
    rules.noSeventhInSameDirectionRule(
      secondLastDescending,
      lastDescending,
      Note.C4  // Fifth down from G4, seventh from B4
    ) should be(false)
    
    // B4 -> G4 -> D4 would span B4 to D4, which is a sixth - allowed
    rules.noSeventhInSameDirectionRule(
      secondLastDescending,
      lastDescending,
      Note.D4  // Fourth down from G4, sixth from B4
    ) should be(true)
    
    // Test with mixed directions (rule shouldn't apply)
    val secondLastMixed = Note.C4
    val lastMixed = Note.E4    // Third up from C4
    
    // When the directions are different, the rule shouldn't apply
    rules.noSeventhInSameDirectionRule(
      secondLastMixed,
      lastMixed,
      Note.D4  // Second down from E4 - different direction
    ) should be(true)  // Allowed because the directions are different
    
    // Test with repeating the same note (no direction - rule doesn't apply)
    val secondLastRepeating = Note.C4
    val lastRepeating = Note.E4       // Third up from C4
    
    // When there's no direction in the second movement (same note), rule doesn't apply
    rules.noSeventhInSameDirectionRule(
      secondLastRepeating,
      lastRepeating,
      Note.E4  // Same as last note - no direction
    ) should be(true)  // Allowed because there's no direction
  }
  
  it should "apply the skip must be preceded by note in span rule correctly" in {
    val rules = MelodicRules()
    
    // Test skip with a preceding note inside the span
    val lastNote = Note.E4
    val candidateNoteSkip = Note.C5  // Skip of a fifth (E4 to C5)
    val precedingNoteInside = Note.G4  // G4 is inside the span from E4 to C5
    
    rules.skipMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteSkip,
      Some(precedingNoteInside)
    ) should be(true)  // Allowed because G4 is inside E4 to C5
    
    // Test skip with a preceding note outside the span
    val precedingNoteOutside = Note.D4  // D4 is outside the span from E4 to C5
    
    rules.skipMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteSkip,
      Some(precedingNoteOutside)
    ) should be(false)  // Disallowed because D4 is outside E4 to C5
    
    // Test descending skip with a preceding note inside the span
    val candidateNoteDescending = Note.A3  // Skip of a fifth down (E4 to A3)
    val precedingNoteInsideDescending = Note.C4  // C4 is inside the span from E4 to A3
    
    rules.skipMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteDescending,
      Some(precedingNoteInsideDescending)
    ) should be(true)  // Allowed because C4 is inside E4 to A3
    
    // Test with no preceding note (for first interval in melody)
    rules.skipMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteSkip,
      None
    ) should be(false)  // Disallowed because there's no preceding note
    
    // Test with a non-skip interval (rule doesn't apply)
    val candidateNoteNonSkip = Note.G4  // Third up from E4, not a skip
    
    rules.skipMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteNonSkip,
      Some(precedingNoteOutside)
    ) should be(true)  // Allowed because it's not a skip
  }
  
