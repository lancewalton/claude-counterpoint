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
  
  test("noParallelPerfectOctavesRule should reject parallel perfect octaves") {
    val rules = CounterpointRules()
    
    // Test with parallel perfect octaves (ascending)
    // C4 to D4 in lower voice, C5 to D5 in upper voice (both are perfect octaves)
    val result1 = rules.noParallelPerfectOctavesRule(
      Note.C4, Note.D4, Note.C5, Note.D5
    )
    result1 shouldBe false
    
    // Test with parallel perfect octaves (descending)
    // D4 to C4 in lower voice, D5 to C5 in upper voice (both are perfect octaves)
    val result2 = rules.noParallelPerfectOctavesRule(
      Note.D4, Note.C4, Note.D5, Note.C5
    )
    result2 shouldBe false
    
    // Test with non-parallel perfect octaves (contrary motion)
    // C4 to D4 in lower voice, C5 to B4 in upper voice
    val result3 = rules.noParallelPerfectOctavesRule(
      Note.C4, Note.D4, Note.C5, Note.B4
    )
    result3 shouldBe true
    
    // Test with parallel motion but not perfect octaves
    // C4 to D4 in lower voice, E4 to F4 in upper voice
    val result4 = rules.noParallelPerfectOctavesRule(
      Note.C4, Note.D4, Note.E4, Note.F4
    )
    result4 shouldBe true
  }
  
  test("noParallelOctavesAndCompoundsRule should reject parallel octaves and their compounds") {
    val rules = CounterpointRules()
    
    // Test with parallel perfect octaves
    // C4 to D4 in lower voice, C5 to D5 in upper voice (perfect octaves)
    val result1 = rules.noParallelOctavesAndCompoundsRule(
      Note.C4, Note.D4, Note.C5, Note.D5
    )
    result1 shouldBe false
    
    // Test with parallel compound octaves (double octaves)
    // C3 to D3 in lower voice, C5 to D5 in upper voice (double octaves)
    val result2 = rules.noParallelOctavesAndCompoundsRule(
      Note.C3, Note.D3, Note.C5, Note.D5
    )
    result2 shouldBe false
    
    // Test with parallel compound octaves (15th - double octave + perfect octave)
    // C3 to D3 in lower voice, C5 to D5 in upper voice (15th interval)
    val result3 = rules.noParallelOctavesAndCompoundsRule(
      Note.C3, Note.D3, Note.C5, Note.D5
    )
    result3 shouldBe false
    
    // Test with non-parallel compound octaves (contrary motion)
    // C3 to D3 in lower voice, C5 to B4 in upper voice
    val result4 = rules.noParallelOctavesAndCompoundsRule(
      Note.C3, Note.D3, Note.C5, Note.B4
    )
    result4 shouldBe true
    
    // Test with parallel motion but not octaves/compounds
    // C4 to D4 in lower voice, E4 to F4 in upper voice (major 3rds)
    val result5 = rules.noParallelOctavesAndCompoundsRule(
      Note.C4, Note.D4, Note.E4, Note.F4
    )
    result5 shouldBe true
  }
}