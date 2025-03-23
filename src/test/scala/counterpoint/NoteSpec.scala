package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NoteSpec extends AnyFlatSpec with Matchers:
  "Note" should "correctly represent musical notes" in {
    import NoteName.*
    
    val c4 = Note(C, 4)
    c4.name should be (C)
    c4.octave should be (4)
    c4.toString should be ("C4")
  }
  
  "Note object" should "contain all notes of C major scale from G2 to G5" in {
    Note.cMajorScale.size should be (22)
    Note.cMajorScale.head should be (Note.G2)
    Note.cMajorScale.last should be (Note.G5)
    
    // Check some specific notes
    Note.cMajorScale should contain (Note.C4)
    Note.cMajorScale should contain (Note.G3)
    Note.cMajorScale should contain (Note.E5)
  }