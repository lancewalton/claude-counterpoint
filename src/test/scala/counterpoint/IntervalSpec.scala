package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalSpec extends AnyFlatSpec with Matchers:
  "Interval" should "calculate Perfect Unison" in {
    val interval = Interval.between(Note.C4, Note.C4)
    interval should be(Interval.PerfectUnison)
  }
  
  it should "calculate ascending intervals" in {
    val majorSecond = Interval.between(Note.C4, Note.D4)
    majorSecond should be(Interval.MajorSecond)
    
    val minorThird = Interval.between(Note.E4, Note.G4)
    minorThird should be(Interval.MinorThird)
    
    val perfectFifth = Interval.between(Note.C4, Note.G4)
    perfectFifth should be(Interval.PerfectFifth)
    
    val perfectOctave = Interval.between(Note.C4, Note.C5)
    perfectOctave should be(Interval.PerfectOctave)
  }
  
  it should "calculate intervals correctly across octaves" in {
    val interval1 = Interval.between(Note.C3, Note.E5)
    interval1.size should be(IntervalSize.Third)
    interval1.quality should be(IntervalQuality.Major)
    
    val interval2 = Interval.between(Note.G2, Note.C4)
    interval2.size should be(IntervalSize.Fourth)
    interval2.quality should be(IntervalQuality.Perfect)
  }
  
  it should "convert intervals to string representation" in {
    Interval.PerfectUnison.toString should be("Perfect Unison")
    Interval.MajorThird.toString should be("Major Third")
    Interval.PerfectFifth.toString should be("Perfect Fifth")
  }
  
  it should "provide semitones value for intervals" in {
    Interval.PerfectUnison.semitones should be(0)
    Interval.MinorSecond.semitones should be(1)
    Interval.MajorSecond.semitones should be(2)
    Interval.MinorThird.semitones should be(3)
    Interval.MajorThird.semitones should be(4)
    Interval.PerfectFourth.semitones should be(5)
    Interval.PerfectFifth.semitones should be(7)
    Interval.PerfectOctave.semitones should be(12)
  }