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
  
  println("\nGetting valid next notes for an empty melody:")
  val emptyMelody = Melody.empty
  println(s"Valid next notes: ${emptyMelody.getValidNextNotes.mkString(", ")}")
  
  println("\nGetting valid next notes for a melody with C4:")
  val melodyWithC4 = Melody.empty.add(Note.C4)
  println(s"Valid next notes after C4: ${melodyWithC4.getValidNextNotes.mkString(", ")}")
