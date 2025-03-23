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
    
    // And we can always repeat a note
    melodyForSkipTest.validNextNotes should contain(Note.F4)  // Same note
    
    // Test two consecutive skips rule, which is separate from the skip precedence rule
    
    // Build a melody where we can have two skips in the same direction
    val melodyForSkipDirectionTest = Melody.empty
      .add(Note.C4)
      .add(Note.D4)   // Step up (for precedence rule)
      .add(Note.G4)   // Fourth up (precedence inside C4-C5)
      .add(Note.C5)   // Fourth up (precedence inside G4-G5)
    
    // We're trying to test the afterTwoSkipsChangeDirectionRule, but that only applies if we
    // have two skips (intervals of a fifth or greater) in the same direction
    // Since we changed the definition of a skip, this test is no longer valid
    // as we don't have two consecutive skips in our test melody
    val melodyForDirectionTest = Melody.empty
      .add(Note.C4)
      .add(Note.G4)   // Skip up (fifth)
      .add(Note.D5)   // Skip up (fifth)
      
    // Now check that we can't go up further  
    melodyForDirectionTest.validNextNotes should not contain(Note.G5)  // Up - disallowed
    
    // But going down should be allowed
    melodyForDirectionTest.validNextNotes should contain(Note.C5)  // Step down
    
    // Test with a melody with mixed skip directions (rule shouldn't apply)
    val melodyWithMixedSkips = Melody.empty
      .add(Note.C4)
      .add(Note.G4)   // Skip up (fifth)
      .add(Note.C4)   // Skip down (fifth)
    
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
  
  it should "enforce the skip must be preceded by note in span rule" in {
    // Single note melody - skips should be disallowed because there is no preceding note
    val singleNoteMelody = Melody.empty
      .add(Note.C4)
      
    // A skip of a fifth should be disallowed (no preceding note)
    singleNoteMelody.validNextNotes should not contain(Note.G4)  // Fifth up, a skip
    
    // Steps and thirds should be allowed (not skips)
    singleNoteMelody.validNextNotes should contain(Note.D4)  // Second up, not a skip
    singleNoteMelody.validNextNotes should contain(Note.E4)  // Third up, not a skip
    singleNoteMelody.validNextNotes should contain(Note.F4)  // Fourth up, not a skip
    
    // Melody with a preceding note inside the skip span (allowed)
    val melodyWithNoteInSpan = Melody.empty
      .add(Note.C4)
      .add(Note.E4)  // E4 is inside the span from C4 to G4
      
    // A skip to G4 should be allowed because E4 is inside the span from C4 to G4
    melodyWithNoteInSpan.validNextNotes should contain(Note.G4)  // Fifth up, a skip, but E4 is in span
    
    // Melody with a preceding note outside the skip span (disallowed)
    val melodyWithNoteOutsideSpan = Melody.empty
      .add(Note.C4)
      .add(Note.D4)  // D4 is outside the span from C4 to A4
      
    // A skip to A4 should be disallowed because D4 is not inside the span from C4 to A4
    melodyWithNoteOutsideSpan.validNextNotes should not contain(Note.A4)  // Sixth up, a skip, D4 not in span
    
    // For descending skips
    val melodyForDescendingSkip = Melody.empty
      .add(Note.G4)
      .add(Note.E4)  // E4 is inside the span from G4 to C4
      
    // A skip down to C4 should be allowed because E4 is inside the span from G4 to C4
    melodyForDescendingSkip.validNextNotes should contain(Note.C4)  // Fifth down, a skip, but E4 is in span
  }