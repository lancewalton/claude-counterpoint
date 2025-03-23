package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalOpsSpec extends AnyFlatSpec with Matchers:
  "IntervalOps" should "calculate large compound intervals correctly" in {
    // G2 to various higher notes
    IntervalOps.getIntervalSize(Note.G2, Note.A4) should be(16)  // compound 9th (16th)
    IntervalOps.getIntervalSize(Note.G2, Note.B4) should be(17)  // compound 10th (17th)
    IntervalOps.getIntervalSize(Note.G2, Note.C5) should be(18)  // compound 11th (18th)
    IntervalOps.getIntervalSize(Note.G2, Note.D5) should be(19)  // compound 12th (19th)
    IntervalOps.getIntervalSize(Note.G2, Note.E5) should be(20)  // compound 13th (20th)
    IntervalOps.getIntervalSize(Note.G2, Note.F5) should be(21)  // compound 14th (21st)
    IntervalOps.getIntervalSize(Note.G2, Note.G5) should be(22)  // triple octave
    
    // Also check the reverse direction
    IntervalOps.getIntervalSize(Note.G5, Note.G2) should be(22)  // triple octave
  }
  
  it should "correctly calculate simple intervals from compound ones" in {
    // G2 to various higher notes - get the simple intervals
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.A4) should be(2)  // simple 2nd
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.B4) should be(3)  // simple 3rd
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.C5) should be(4)  // simple 4th
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.D5) should be(5)  // simple 5th
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.E5) should be(6)  // simple 6th
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.F5) should be(7)  // simple 7th
    IntervalOps.getSimpleIntervalSize(Note.G2, Note.G5) should be(8)  // octave (simple)
  }
  
  it should "determine interval characteristics correctly" in {
    // Check if intervals are skips
    IntervalOps.isSkip(Note.G2, Note.B2) should be(true)   // 3rd is a skip
    IntervalOps.isSkip(Note.G2, Note.C3) should be(true)   // 4th is a skip
    IntervalOps.isSkip(Note.G2, Note.D3) should be(false)  // 5th is not a skip
    IntervalOps.isSkip(Note.G2, Note.B3) should be(false)  // compound 3rd is not a skip
    
    // Check if intervals are leaps
    IntervalOps.isLeap(Note.G2, Note.D3) should be(true)   // 5th is a leap
    IntervalOps.isLeap(Note.G2, Note.E3) should be(true)   // 6th is a leap
    IntervalOps.isLeap(Note.G2, Note.G3) should be(true)   // octave is a leap
    IntervalOps.isLeap(Note.G2, Note.B3) should be(true)   // compound 3rd is a leap
    IntervalOps.isLeap(Note.G2, Note.B2) should be(false)  // 3rd is not a leap
  }
  
  it should "correctly identify perfect fifths in different octaves" in {
    // Get intervals
    val simpleInterval = IntervalOps.getInterval(Note.C4, Note.G4)  // Perfect 5th
    val compoundInterval = IntervalOps.getInterval(Note.C4, Note.G5)  // Compound Perfect 5th (P12)
    
    // Test perfect fifth identification
    IntervalOps.isPerfectFifth(simpleInterval) should be(true)
    IntervalOps.isPerfectFifth(compoundInterval) should be(true)
    
    // Also test for a non-fifth interval
    val notFifth = IntervalOps.getInterval(Note.C4, Note.A4)  // Major 6th
    IntervalOps.isPerfectFifth(notFifth) should be(false)
  }

  it should "handle intervalSize calculations for all test cases" in {
    // Test all noteSize test cases
    IntervalOps.getIntervalSize(Note.C4, Note.G4) should be(5)  // Perfect fifth
    IntervalOps.getIntervalSize(Note.C4, Note.C5) should be(8)  // Perfect octave
    IntervalOps.getIntervalSize(Note.C4, Note.E5) should be(10) // Major tenth
    IntervalOps.getIntervalSize(Note.C3, Note.D4) should be(9)  // Major ninth
  }