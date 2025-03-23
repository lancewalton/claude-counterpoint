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
