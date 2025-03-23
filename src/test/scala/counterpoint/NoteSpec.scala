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
  
  it should "calculate MIDI note numbers correctly" in {
    import NoteName.*
    
    Note(C, 4).midiNumber should be(60)
    Note(G, 4).midiNumber should be(67)
    Note(C, 5).midiNumber should be(72)
  }
  
  it should "detect notes within an octave" in {
    import NoteName.*
    
    Note(C, 4).isWithinOctave(Note(C, 5)) should be(true)
    Note(C, 4).isWithinOctave(Note(B, 4)) should be(true)
    Note(C, 4).isWithinOctave(Note(D, 5)) should be(false)
    Note(C, 4).isWithinOctave(Note(G, 5)) should be(false)
  }
  
  it should "calculate interval sizes correctly" in {
    import NoteName.*
    
    Note(C, 4).intervalSize(Note(C, 4)) should be(1)  // unison
    Note(C, 4).intervalSize(Note(D, 4)) should be(2)  // second
    Note(C, 4).intervalSize(Note(E, 4)) should be(3)  // third
    Note(C, 4).intervalSize(Note(F, 4)) should be(4)  // fourth
    Note(C, 4).intervalSize(Note(G, 4)) should be(5)  // fifth
    Note(C, 4).intervalSize(Note(A, 4)) should be(6)  // sixth
    Note(C, 4).intervalSize(Note(B, 4)) should be(7)  // seventh
    Note(C, 4).intervalSize(Note(C, 5)) should be(1)  // octave
    
    // Test both ascending and descending intervals
    // G4 to C5 is a fourth (5 semitones up)
    Note(G, 4).intervalSize(Note(C, 5)) should be(4)
    // C5 to G4 is a fifth (7 semitones down)
    Note(C, 5).intervalSize(Note(G, 4)) should be(5)
  }
  
  "Note object" should "contain all notes of C major scale from G2 to G5" in {
    Note.cMajorScale.size should be (22)
    Note.cMajorScale.head should be (Note.G2)
    Note.cMajorScale.last should be (Note.G5)
    
    Note.cMajorScale should contain (Note.C4)
    Note.cMajorScale should contain (Note.G3)
    Note.cMajorScale should contain (Note.E5)
  }
  
  it should "have all notes in the range G2 to G5" in {
    Note.allNotes should have size 22
    Note.allNotes should be(Note.cMajorScale)
  }