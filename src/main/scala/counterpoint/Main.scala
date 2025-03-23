package counterpoint

@main def main(): Unit = 
  println("C Major Scale from G2 to G5:")
  Note.cMajorScale.foreach(println)
  
  println("\nC Major Scale as a Melody:")
  val cMajorMelody = Melody(Note.cMajorScale)
  println(cMajorMelody)
  
  println("\nCreating a melody by adding notes:")
  val simpleMelody = Melody.empty
    .add(Note.C4)
    .add(Note.D4)
    .add(Note.E4)
    .add(Note.F4)
    .add(Note.G4)
  println(simpleMelody)
  
  println("\nGetting the list of notes from a melody:")
  println(s"Melody notes: ${simpleMelody.toList}")
  
  println("\nFinding valid next notes for a melody:")
  val melodyWithC4 = Melody.empty.add(Note.C4)
  println(s"Valid notes after C4: ${melodyWithC4.validNextNotes.mkString(", ")}")
  
  val melodyWithG5 = Melody.empty.add(Note.G5)
  println(s"Valid notes after G5: ${melodyWithG5.validNextNotes.mkString(", ")}")
  
  println("\nShowing the disallowed intervals of a seventh:")
  println(s"C4 to B4 is an interval of size: ${Note.C4.intervalSize(Note.B4)}")
  println(s"Is B4 in valid next notes after C4? ${melodyWithC4.validNextNotes.contains(Note.B4)}")
