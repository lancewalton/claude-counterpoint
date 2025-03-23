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
}