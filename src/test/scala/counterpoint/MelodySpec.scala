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
    // Since octaves are now leaps, and leaps must be preceded by a note inside the span of the leap,
    // an octave jump would be disallowed from a single note
    melodyWithC4.validNextNotes should not contain(Note.C5)
    melodyWithC4.validNextNotes should not contain(Note.C3)
    
    val melodyWithG4 = Melody.empty.add(Note.G4)
    // Similarly, octave leaps from G4 should also be disallowed
    melodyWithG4.validNextNotes should not contain(Note.G5)
    melodyWithG4.validNextNotes should not contain(Note.G3)
    
    val melodyWithHighNote = Melody.empty.add(Note.G5)
    // G5 should have valid next notes, but not an octave leap
    melodyWithHighNote.validNextNotes should not contain(Note.G4)
    melodyWithHighNote.validNextNotes should contain(Note.F5)  // Step down is allowed
  }
  
  it should "use melodic rules to filter valid next notes" in {
    val melody = Melody.empty.add(Note.C4)
    melody.validNextNotes should contain(Note.F4)  // fourth (not a skip)
    melody.validNextNotes should not contain(Note.G4)  // fifth (a skip, but no preceding note in span)
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
    // Note: With the updated definition, a skip is an interval of a fifth or larger
    // So we need to update this test
    
    // Create a melody with consecutive notes so we can test skips
    // Note: Due to the new skip precedence rule, we need more complex test structures
    val melodyForSkipTest = Melody.empty
      .add(Note.C4)
      .add(Note.D4)  // Step up (not a skip)
      .add(Note.F4)  // Third up (not a skip)
    
    // Now we have C4 -> D4 -> F4
    // We should be able to move by step (not a skip)
    melodyForSkipTest.validNextNotes should contain(Note.G4)  // Step from F4
    
    // We should be able to move by a third (not a skip)
    melodyForSkipTest.validNextNotes should contain(Note.D4)  // Third down from F4
    
    // We shouldn't be able to repeat the same note
    melodyForSkipTest.validNextNotes should not contain(Note.F4)  // Same note
    
    // Test two consecutive skips rule, which is separate from the leap precedence rule
    
    // Build a melody where we have two skips (thirds/fourths) in the same direction
    val melodyForSkipDirectionTest = Melody.empty
      .add(Note.C4)
      .add(Note.E4)   // Third up (skip)
      .add(Note.A4)   // Fourth up (skip)
      
    // Now we have two consecutive skips in the same direction
    // We shouldn't be able to move up further
    melodyForSkipDirectionTest.validNextNotes should not contain(Note.C5)  // Up - disallowed
    
    // But going down should be allowed
    melodyForSkipDirectionTest.validNextNotes should contain(Note.G4)  // Step down
    
    // Test with a melody with mixed skip directions (rule shouldn't apply)
    val melodyWithMixedSkips = Melody.empty
      .add(Note.C4)
      .add(Note.E4)   // Skip up (third)
      .add(Note.C4)   // Skip down (third)
    
    // Should allow notes in both directions 
    melodyWithMixedSkips.validNextNotes should contain(Note.D4)  // Step up
    melodyWithMixedSkips.validNextNotes should contain(Note.B3)  // Step down
  }
  
  it should "enforce the no seventh in same direction rule" in {
    // Create a melody with ascending movement
    val melodyAscending = Melody.empty
      .add(Note.C4)
      .add(Note.E4)  // Third up from C4
    
    // An ascending movement to B4 would create a seventh from C4 to B4 - disallowed
    melodyAscending.validNextNotes should not contain(Note.B4)  // Seventh from C4
    
    // An ascending movement to A4 would create a sixth from C4 to A4 - allowed
    melodyAscending.validNextNotes should contain(Note.A4)  // Sixth from C4
    
    // Create a melody with descending movement
    val melodyDescending = Melody.empty
      .add(Note.B4)
      .add(Note.G4)  // Third down from B4
    
    // A descending movement to C4 would create a seventh from B4 to C4 - disallowed
    melodyDescending.validNextNotes should not contain(Note.C4)  // Seventh from B4
    
    // A descending movement to D4 would create a sixth from B4 to D4 - allowed
    melodyDescending.validNextNotes should contain(Note.D4)  // Sixth from B4
    
    // Create a melody with mixed direction movement
    val melodyMixed = Melody.empty
      .add(Note.E4)
      .add(Note.C4)  // Third down from E4
    
    // Going up to G4 is possible even though it would be a fifth from C4
    melodyMixed.validNextNotes should contain(Note.G4)  // The rule doesn't apply since directions differ
  }
  
  it should "enforce the leap must be preceded by note in span rule" in {
    // Single note melody - leaps should be disallowed because there is no preceding note
    val singleNoteMelody = Melody.empty
      .add(Note.C4)
      
    // A leap of a fifth should be disallowed (no preceding note)
    singleNoteMelody.validNextNotes should not contain(Note.G4)  // Fifth up, a leap
    
    // Steps, thirds and fourths should be allowed (not leaps)
    singleNoteMelody.validNextNotes should contain(Note.D4)  // Second up, not a leap
    singleNoteMelody.validNextNotes should contain(Note.E4)  // Third up, not a leap
    singleNoteMelody.validNextNotes should contain(Note.F4)  // Fourth up, not a leap
    
    // Melody with a preceding note inside the leap span (allowed)
    val melodyWithNoteInSpan = Melody.empty
      .add(Note.C4)
      .add(Note.E4)  // E4 is inside the span from C4 to G4
      
    // A leap to G4 should be allowed because E4 is inside the span from C4 to G4
    melodyWithNoteInSpan.validNextNotes should contain(Note.G4)  // Fifth up, a leap, but E4 is in span
    
    // Melody with a preceding note outside the leap span (disallowed)
    val melodyWithNoteOutsideSpan = Melody.empty
      .add(Note.C4)
      .add(Note.D4)  // D4 is outside the span from C4 to A4
      
    // A leap to A4 should be disallowed because D4 is not inside the span from C4 to A4
    melodyWithNoteOutsideSpan.validNextNotes should not contain(Note.A4)  // Sixth up, a leap, D4 not in span
    
    // For descending leaps
    val melodyForDescendingLeap = Melody.empty
      .add(Note.G4)
      .add(Note.E4)  // E4 is inside the span from G4 to C4
      
    // A leap down to C4 should be allowed because E4 is inside the span from G4 to C4
    melodyForDescendingLeap.validNextNotes should contain(Note.C4)  // Fifth down, a leap, but E4 is in span
  }
  
  it should "enforce the after leap note in span rule" in {
    // Create a melody with an ascending leap
    val melodyWithAscendingLeap = Melody.empty
      .add(Note.C4)
      .add(Note.G4)  // Fifth up from C4 (a leap)
    
    // After a leap, the next note should be inside the span of the leap
    melodyWithAscendingLeap.validNextNotes should contain(Note.E4)  // Inside the span from C4 to G4
    melodyWithAscendingLeap.validNextNotes should contain(Note.F4)  // Inside the span from C4 to G4
    
    // Notes outside the span should be disallowed
    melodyWithAscendingLeap.validNextNotes should not contain(Note.B4)  // Outside the span (above G4)
    melodyWithAscendingLeap.validNextNotes should not contain(Note.A3)  // Outside the span (below C4)
    
    // Create a melody with a descending leap
    val melodyWithDescendingLeap = Melody.empty
      .add(Note.G4)
      .add(Note.C4)  // Fifth down from G4 (a leap)
    
    // After a leap, the next note should be inside the span of the leap
    melodyWithDescendingLeap.validNextNotes should contain(Note.E4)  // Inside the span from G4 to C4
    melodyWithDescendingLeap.validNextNotes should contain(Note.F4)  // Inside the span from G4 to C4
    
    // Notes outside the span should be disallowed
    melodyWithDescendingLeap.validNextNotes should not contain(Note.B3)  // Outside the span (below C4)
    melodyWithDescendingLeap.validNextNotes should not contain(Note.A4)  // Outside the span (above G4)
    
    // Create a melody without a leap (rule shouldn't apply)
    val melodyWithoutLeap = Melody.empty
      .add(Note.C4)
      .add(Note.E4)  // Third up from C4 (not a leap)
    
    // When there isn't a leap, the rule doesn't apply, so notes outside the span are allowed
    melodyWithoutLeap.validNextNotes should contain(Note.G4)  // Outside the span from C4 to E4
    melodyWithoutLeap.validNextNotes should contain(Note.C4)  // Outside the span from C4 to E4
  }
  
  it should "not allow consecutive repeated notes" in {
    val melody = Melody.empty
      .add(Note.E4)
    
    melody.validNextNotes should not contain(Note.E4)  // Same note not allowed
    melody.validNextNotes should contain(Note.F4)      // Different note is allowed
  }