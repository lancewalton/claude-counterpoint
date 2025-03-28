package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MelodicRulesSpec extends AnyFlatSpec with Matchers:
  "MelodicRules" should "calculate intervals from the lowest note" in {
    val rules = MelodicRules()
    
    // Ascending
    Interval.size(Note.C4, Note.G4) should be(5)  // fifth
    
    // Descending
    Interval.size(Note.G4, Note.C4) should be(5)  // still a fifth
    
    // Same note
    Interval.size(Note.C4, Note.C4) should be(1)  // unison
  }
  
  it should "determine melodic direction correctly" in {
    val rules = MelodicRules()
    
    // Ascending
    Interval.direction(Note.C4, Note.E4) should be(1)
    
    // Descending
    Interval.direction(Note.G4, Note.D4) should be(-1)
    
    // Same note
    Interval.direction(Note.C4, Note.C4) should be(0)
  }
  
  it should "identify skips correctly" in {
    val rules = MelodicRules()
    
    // Thirds are skips
    Interval.isSkip(Note.C4, Note.E4) should be(true)  // major third
    Interval.isSkip(Note.A4, Note.C5) should be(true)  // minor third
    
    // Fourths are skips
    Interval.isSkip(Note.C4, Note.F4) should be(true)  // perfect fourth
    
    // Seconds are not skips
    Interval.isSkip(Note.C4, Note.D4) should be(false)  // major second
    
    // Fifths and larger are not skips (they're leaps)
    Interval.isSkip(Note.C4, Note.G4) should be(false)  // perfect fifth
    Interval.isSkip(Note.C4, Note.A4) should be(false)  // major sixth
    
    // Compound intervals should not be considered skips
    Interval.isSkip(Note.C4, Note.E5) should be(false)  // tenth (compound third)
    Interval.isSkip(Note.C4, Note.F5) should be(false)  // eleventh (compound fourth)
  }
  
  it should "identify leaps correctly" in {
    val rules = MelodicRules()
    
    // Fifths are leaps
    Interval.isLeap(Note.C4, Note.G4) should be(true)  // perfect fifth
    
    // Sixths are leaps
    Interval.isLeap(Note.C4, Note.A4) should be(true)  // major sixth
    
    // Sevenths are leaps
    Interval.isLeap(Note.C4, Note.B4) should be(true)  // major seventh
    
    // Octaves are leaps
    Interval.isLeap(Note.C4, Note.C5) should be(true)  // octave is now considered a leap
    
    // Thirds are not leaps
    Interval.isLeap(Note.C4, Note.E4) should be(false)  // major third
    Interval.isLeap(Note.A4, Note.C5) should be(false)  // minor third
    
    // Fourths are not leaps
    Interval.isLeap(Note.C4, Note.F4) should be(false)  // perfect fourth
    
    // Seconds are not leaps
    Interval.isLeap(Note.C4, Note.D4) should be(false)  // major second
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
    Interval.semitones(Note.F4, Note.B4) should be(6)
    rules.notATritoneRule(Note.F4, Note.B4) should be(false)
    
    // B to F is also a tritone
    Interval.semitones(Note.B3, Note.F4) should be(6)
    rules.notATritoneRule(Note.B3, Note.F4) should be(false)
    
    // C to F is a fourth, not a tritone
    Interval.semitones(Note.C4, Note.F4) should be(5)
    rules.notATritoneRule(Note.C4, Note.F4) should be(true)
  }
  
  it should "apply the two skips rule correctly" in {
    val rules = MelodicRules()
    
    // Test ascending pattern with skips (thirds and fourths)
    val thirdLastAscending = Note.C4
    val secondLastAscending = Note.E4  // Third up from C4
    val lastAscending = Note.A4       // Fourth up from E4
    
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
      Note.A4  // Same note
    ) should be(true)
    
    // Test descending pattern
    val thirdLastDescending = Note.G4
    val secondLastDescending = Note.E4  // Third down from G4
    val lastDescending = Note.B3       // Fourth down from E4
    
    // After two skips down, going down further should be disallowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastDescending, 
      secondLastDescending, 
      lastDescending, 
      Note.G3  // Going down
    ) should be(false)
    
    // After two skips down, going up should be allowed
    rules.afterTwoSkipsChangeDirectionRule(
      thirdLastDescending, 
      secondLastDescending, 
      lastDescending, 
      Note.C4  // Going up
    ) should be(true)
    
    // Test mixed directions (rule shouldn't apply)
    val mixedDirectionThird = Note.C4
    val mixedDirectionSecond = Note.E4  // Third up (skip)
    val mixedDirectionLast = Note.C4    // Third down (skip)
    
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
  
  it should "apply the leap must be preceded by note in span rule correctly" in {
    val rules = MelodicRules()
    
    // Test leap with a preceding note inside the span
    val lastNote = Note.E4
    val candidateNoteLeap = Note.C5  // Leap of a fifth (E4 to C5)
    val precedingNoteInside = Note.G4  // G4 is inside the span from E4 to C5
    
    rules.leapMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteLeap,
      Some(precedingNoteInside)
    ) should be(true)  // Allowed because G4 is inside E4 to C5
    
    // Test leap with a preceding note outside the span
    val precedingNoteOutside = Note.D4  // D4 is outside the span from E4 to C5
    
    rules.leapMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteLeap,
      Some(precedingNoteOutside)
    ) should be(false)  // Disallowed because D4 is outside E4 to C5
    
    // Test descending leap with a preceding note inside the span
    val candidateNoteDescending = Note.A3  // Leap of a fifth down (E4 to A3)
    val precedingNoteInsideDescending = Note.C4  // C4 is inside the span from E4 to A3
    
    rules.leapMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteDescending,
      Some(precedingNoteInsideDescending)
    ) should be(true)  // Allowed because C4 is inside E4 to A3
    
    // Test with no preceding note (for first interval in melody)
    rules.leapMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteLeap,
      None
    ) should be(false)  // Disallowed because there's no preceding note
    
    // Test with a non-leap interval (rule doesn't apply)
    val candidateNoteNonLeap = Note.F4  // Second up from E4, not a leap
    
    rules.leapMustBePrecededByNoteInSpanRule(
      lastNote,
      candidateNoteNonLeap,
      Some(precedingNoteOutside)
    ) should be(true)  // Allowed because it's not a leap
  }
  
  it should "apply the after leap note in span rule correctly" in {
    val rules = MelodicRules()
    
    // Test ascending leap
    val secondLastAscendingLeap = Note.C4
    val lastAscendingLeap = Note.G4       // Fifth up from C4 (a leap)
    
    // C4 -> G4 -> E4 would put E4 inside the span of the leap - allowed
    rules.afterLeapNoteInSpanRule(
      secondLastAscendingLeap,
      lastAscendingLeap,
      Note.E4  // Inside the span from C4 to G4
    ) should be(true)
    
    // C4 -> G4 -> B4 would put B4 outside the span of the leap - disallowed
    rules.afterLeapNoteInSpanRule(
      secondLastAscendingLeap,
      lastAscendingLeap,
      Note.B4  // Outside the span from C4 to G4
    ) should be(false)
    
    // C4 -> G4 -> A3 would put A3 outside the span of the leap - disallowed
    rules.afterLeapNoteInSpanRule(
      secondLastAscendingLeap,
      lastAscendingLeap,
      Note.A3  // Outside the span from C4 to G4
    ) should be(false)
    
    // Test descending leap
    val secondLastDescendingLeap = Note.G4
    val lastDescendingLeap = Note.C4       // Fifth down from G4 (a leap)
    
    // G4 -> C4 -> E4 would put E4 inside the span of the leap - allowed
    rules.afterLeapNoteInSpanRule(
      secondLastDescendingLeap,
      lastDescendingLeap,
      Note.E4  // Inside the span from G4 to C4
    ) should be(true)
    
    // G4 -> C4 -> B3 would put B3 outside the span of the leap - disallowed
    rules.afterLeapNoteInSpanRule(
      secondLastDescendingLeap,
      lastDescendingLeap,
      Note.B3  // Outside the span from G4 to C4
    ) should be(false)
    
    // G4 -> C4 -> A4 would put A4 outside the span of the leap - disallowed
    rules.afterLeapNoteInSpanRule(
      secondLastDescendingLeap,
      lastDescendingLeap,
      Note.A4  // Outside the span from G4 to C4
    ) should be(false)
    
    // Test with a non-leap interval (rule doesn't apply)
    val secondLastNonLeap = Note.C4
    val lastNonLeap = Note.E4       // Third up from C4 (not a leap)
    
    // When the previous interval isn't a leap, the rule doesn't apply
    rules.afterLeapNoteInSpanRule(
      secondLastNonLeap,
      lastNonLeap,
      Note.G4  // Doesn't matter, rule doesn't apply
    ) should be(true)  // Allowed because there's no leap
  }
  
  it should "enforce the no consecutive repeated notes rule" in {
    val rules = MelodicRules()
    
    val note = Note.E4
    
    // The same note should be disallowed
    rules.notConsecutiveRepeatedNotesRule(note, note) should be(false)
    
    // A different note should be allowed
    rules.notConsecutiveRepeatedNotesRule(note, Note.F4) should be(true)
  }