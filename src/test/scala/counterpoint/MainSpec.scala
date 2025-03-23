package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyFlatSpec with Matchers:
  "Main" should "run without errors" in {
    // Simple test to ensure the main method runs without throwing exceptions
    noException should be thrownBy {
      main()
    }
  }
  
  "Note" should "correctly represent musical notes" in {
    import NoteName.*
    
    val c4 = Note(C, 4)
    c4.name should be (C)
    c4.octave should be (4)
    c4.toString should be ("C4")
  }
  
  "Notes object" should "contain all notes of C major scale from G2 to G5" in {
    Notes.cMajorScale.size should be (22)
    Notes.cMajorScale.head should be (Notes.G2)
    Notes.cMajorScale.last should be (Notes.G5)
    
    // Check some specific notes
    Notes.cMajorScale should contain (Notes.C4)
    Notes.cMajorScale should contain (Notes.G3)
    Notes.cMajorScale should contain (Notes.E5)
  }
