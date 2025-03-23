package counterpoint

@main def main(): Unit = 
  println("C Major Scale from G2 to G5:")
  Note.cMajorScale.foreach(println)
  
  println("\nC Major Scale as a Melody:")
  val cMajorMelody = Melody(Note.cMajorScale)
  println(cMajorMelody)
  
  println("\nCreating a melody by adding notes:")
  val simpleMelody = Melody.empty
    .add(Note.C4)  // First added
    .add(Note.D4)  // Second added
    .add(Note.E4)  // Third added
    .add(Note.F4)  // Fourth added
    .add(Note.G4)  // Fifth added
  println(simpleMelody)
  
  println("\nGetting the list of notes from a melody:")
  println(s"Melody notes: ${simpleMelody.toList}")
