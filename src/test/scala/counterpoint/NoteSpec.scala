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