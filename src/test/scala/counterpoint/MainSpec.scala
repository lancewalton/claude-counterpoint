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
    import Accidental.*
    
    val c4 = Note(C, Natural, 4)
    c4.name should be (C)
    c4.accidental should be (Natural)
    c4.octave should be (4)
    c4.toString should be ("C4")
    
    val fSharp3 = Note(F, Sharp, 3)
    fSharp3.toString should be ("F#3")
    
    val bFlat4 = Note(B, Flat, 4)
    bFlat4.toString should be ("Bb4")
  }
  
  "Notes object" should "contain all notes of C major scale from G2 to G5" in {
    Notes.cMajorScale.size should be (22)
    Notes.cMajorScale.head should be (Notes.G2)
    Notes.cMajorScale.last should be (Notes.G5)
    
    // Check that all notes in the scale have Natural accidental
    Notes.cMajorScale.forall(_.accidental == Accidental.Natural) should be (true)
    
    // Check some specific notes
    Notes.cMajorScale should contain (Notes.C4)
    Notes.cMajorScale should contain (Notes.G3)
    Notes.cMajorScale should contain (Notes.E5)
  }
