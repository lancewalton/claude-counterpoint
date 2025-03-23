package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalOpsSpec extends AnyFlatSpec with Matchers:
  "Interval" should "calculate large compound intervals correctly" in {
    // G2 to various higher notes
    Interval.size(Note.G2, Note.A4) should be(16)  // compound 9th (16th)
    Interval.size(Note.G2, Note.B4) should be(17)  // compound 10th (17th)
    Interval.size(Note.G2, Note.C5) should be(18)  // compound 11th (18th)
    Interval.size(Note.G2, Note.D5) should be(19)  // compound 12th (19th)
    Interval.size(Note.G2, Note.E5) should be(20)  // compound 13th (20th)
    Interval.size(Note.G2, Note.F5) should be(21)  // compound 14th (21st)
    Interval.size(Note.G2, Note.G5) should be(22)  // triple octave
    
    // Also check the reverse direction
    Interval.size(Note.G5, Note.G2) should be(22)  // triple octave
  }
  
  it should "correctly calculate simple intervals from compound ones" in {
    // G2 to various higher notes - get the simple intervals
    Interval.simpleSize(Note.G2, Note.A4) should be(2)  // simple 2nd
    Interval.simpleSize(Note.G2, Note.B4) should be(3)  // simple 3rd
    Interval.simpleSize(Note.G2, Note.C5) should be(4)  // simple 4th
    Interval.simpleSize(Note.G2, Note.D5) should be(5)  // simple 5th
    Interval.simpleSize(Note.G2, Note.E5) should be(6)  // simple 6th
    Interval.simpleSize(Note.G2, Note.F5) should be(7)  // simple 7th
    Interval.simpleSize(Note.G2, Note.G5) should be(8)  // octave (simple)
  }
  
  it should "determine interval characteristics correctly" in {
    // Check if intervals are skips
    Interval.isSkip(Note.G2, Note.B2) should be(true)   // 3rd is a skip
    Interval.isSkip(Note.G2, Note.C3) should be(true)   // 4th is a skip
    Interval.isSkip(Note.G2, Note.D3) should be(false)  // 5th is not a skip
    Interval.isSkip(Note.G2, Note.B3) should be(false)  // compound 3rd is not a skip
    
    // Check if intervals are leaps
    Interval.isLeap(Note.G2, Note.D3) should be(true)   // 5th is a leap
    Interval.isLeap(Note.G2, Note.E3) should be(true)   // 6th is a leap
    Interval.isLeap(Note.G2, Note.G3) should be(true)   // octave is a leap
    Interval.isLeap(Note.G2, Note.B3) should be(true)   // compound 3rd is a leap
    Interval.isLeap(Note.G2, Note.B2) should be(false)  // 3rd is not a leap
  }
  
  it should "correctly identify perfect fifths in different octaves" in {
    // Get intervals
    val simpleInterval = Interval.between(Note.C4, Note.G4)  // Perfect 5th
    val compoundInterval = Interval.between(Note.C4, Note.G5)  // Compound Perfect 5th (P12)
    
    // Test perfect fifth identification
    simpleInterval.isPerfectFifth should be(true)
    compoundInterval.isPerfectFifth should be(true)
    
    // Also test for a non-fifth interval
    val notFifth = Interval.between(Note.C4, Note.A4)  // Major 6th
    notFifth.isPerfectFifth should be(false)
  }

  it should "handle intervalSize calculations for all test cases" in {
    // Test all noteSize test cases
    Interval.size(Note.C4, Note.G4) should be(5)  // Perfect fifth
    Interval.size(Note.C4, Note.C5) should be(8)  // Perfect octave
    Interval.size(Note.C4, Note.E5) should be(10) // Major tenth
    Interval.size(Note.C3, Note.D4) should be(9)  // Major ninth
  }