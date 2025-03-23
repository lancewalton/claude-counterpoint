package counterpoint

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CounterpointRulesSpec extends AnyFunSuite with Matchers {
  
  test("noParallelPerfectFifthsRule should reject parallel perfect fifths") {
    val rules = CounterpointRules()
    
    // Test with parallel perfect fifths (ascending)
    // C4 to D4 in lower voice, G4 to A4 in upper voice (both are perfect fifths)
    val result1 = rules.noParallelPerfectFifthsRule(
      Note.C4, Note.D4, Note.G4, Note.A4
    )
    result1 shouldBe false
    
    // Test with parallel perfect fifths (descending)
    // D4 to C4 in lower voice, A4 to G4 in upper voice (both are perfect fifths)
    val result2 = rules.noParallelPerfectFifthsRule(
      Note.D4, Note.C4, Note.A4, Note.G4
    )
    result2 shouldBe false
    
    // Test with non-parallel perfect fifths (contrary motion)
    // C4 to D4 in lower voice, G4 to F4 in upper voice
    val result3 = rules.noParallelPerfectFifthsRule(
      Note.C4, Note.D4, Note.G4, Note.F4
    )
    result3 shouldBe true
    
    // Test with parallel motion but not perfect fifths
    // C4 to D4 in lower voice, E4 to F4 in upper voice
    val result4 = rules.noParallelPerfectFifthsRule(
      Note.C4, Note.D4, Note.E4, Note.F4
    )
    result4 shouldBe true
  }
  
  test("noParallelOctavesRule should reject parallel octaves") {
    val rules = CounterpointRules()
    
    // Test with parallel octaves (ascending)
    // C4 to D4 in lower voice, C5 to D5 in upper voice (both are octaves)
    val result1 = rules.noParallelOctavesRule(
      Note.C4, Note.D4, Note.C5, Note.D5
    )
    result1 shouldBe false
    
    // Test with parallel octaves (descending)
    // D4 to C4 in lower voice, D5 to C5 in upper voice (both are octaves)
    val result2 = rules.noParallelOctavesRule(
      Note.D4, Note.C4, Note.D5, Note.C5
    )
    result2 shouldBe false
    
    // Test with non-parallel octaves (contrary motion)
    // C4 to D4 in lower voice, C5 to B4 in upper voice
    val result3 = rules.noParallelOctavesRule(
      Note.C4, Note.D4, Note.C5, Note.B4
    )
    result3 shouldBe true
    
    // Test with parallel motion but not octaves
    // C4 to D4 in lower voice, G4 to A4 in upper voice
    val result4 = rules.noParallelOctavesRule(
      Note.C4, Note.D4, Note.G4, Note.A4
    )
    result4 shouldBe true
  }
  
  test("noDirectMotionToPerfectConsonanceRule should reject direct motion to perfect consonances") {
    val rules = CounterpointRules()
    
    // Test with direct motion to a perfect fifth
    // C4 to G4 in lower voice, E4 to D5 in upper voice (ending on a perfect fifth)
    val result1 = rules.noDirectMotionToPerfectConsonanceRule(
      Note.C4, Note.G4, Note.E4, Note.D5
    )
    result1 shouldBe false
    
    // Test with direct motion to a perfect octave
    // C4 to G4 in lower voice, E4 to G5 in upper voice (ending on a perfect octave)
    val result2 = rules.noDirectMotionToPerfectConsonanceRule(
      Note.C4, Note.G4, Note.E4, Note.G5
    )
    result2 shouldBe false
    
    // Test with contrary motion to a perfect fifth
    // C4 to G4 in lower voice, B4 to D4 in upper voice
    val result3 = rules.noDirectMotionToPerfectConsonanceRule(
      Note.C4, Note.G4, Note.B4, Note.D4
    )
    result3 shouldBe true
    
    // Test with direct motion but not to a perfect consonance
    // C4 to D4 in lower voice, E4 to F4 in upper voice (ending on a minor third)
    val result4 = rules.noDirectMotionToPerfectConsonanceRule(
      Note.C4, Note.D4, Note.E4, Note.F4
    )
    result4 shouldBe true
  }
  
  test("noVoiceCrossingRule should prevent voices from crossing") {
    val rules = CounterpointRules()
    
    // Test with proper voice separation
    val result1 = rules.noVoiceCrossingRule(Note.C4, Note.G4)
    result1 shouldBe true
    
    // Test with voices at the same pitch (not allowed)
    val result2 = rules.noVoiceCrossingRule(Note.C4, Note.C4)
    result2 shouldBe false
    
    // Test with voice crossing (not allowed)
    val result3 = rules.noVoiceCrossingRule(Note.G4, Note.C4)
    result3 shouldBe false
  }
  
  test("properVoiceSpacingRule should maintain proper spacing between voices") {
    val rules = CounterpointRules()
    
    // Test with proper spacing (within an octave)
    val result1 = rules.properVoiceSpacingRule(Note.C4, Note.G4)
    result1 shouldBe true
    
    // Test with exactly an octave spacing
    val result2 = rules.properVoiceSpacingRule(Note.C4, Note.C5)
    result2 shouldBe true
    
    // Test with spacing larger than an octave (not recommended)
    val result3 = rules.properVoiceSpacingRule(Note.C4, Note.D5)
    result3 shouldBe false
  }
  
  test("consonantIntervalRule should enforce consonant intervals between voices") {
    val rules = CounterpointRules()
    
    // Test with perfect unison (consonant)
    val result1 = rules.consonantIntervalRule(Note.C4, Note.C4)
    result1 shouldBe true
    
    // Test with minor third (consonant)
    val result2 = rules.consonantIntervalRule(Note.C4, Note.E4.lower)
    result2 shouldBe true
    
    // Test with major third (consonant)
    val result3 = rules.consonantIntervalRule(Note.C4, Note.E4)
    result3 shouldBe true
    
    // Test with perfect fifth (consonant)
    val result4 = rules.consonantIntervalRule(Note.C4, Note.G4)
    result4 shouldBe true
    
    // Test with major sixth (consonant)
    val result5 = rules.consonantIntervalRule(Note.C4, Note.A4)
    result5 shouldBe true
    
    // Test with minor seventh (dissonant)
    val result6 = rules.consonantIntervalRule(Note.C4, Note.B4.lower)
    result6 shouldBe false
    
    // Test with major second (dissonant)
    val result7 = rules.consonantIntervalRule(Note.C4, Note.D4)
    result7 shouldBe false
    
    // Test with perfect fourth (treated as dissonant in strict counterpoint)
    val result8 = rules.consonantIntervalRule(Note.C4, Note.F4)
    result8 shouldBe false
  }
}