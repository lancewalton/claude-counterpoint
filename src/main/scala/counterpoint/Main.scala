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
  
  println("\nCalculating intervals between notes:")
  println(s"C4 to E4: ${Interval.between(Note.C4, Note.E4)} (${Interval.between(Note.C4, Note.E4).semitones} semitones)")
  println(s"C4 to G4: ${Interval.between(Note.C4, Note.G4)} (${Interval.between(Note.C4, Note.G4).semitones} semitones)")
  println(s"C4 to C5: ${Interval.between(Note.C4, Note.C5)} (${Interval.between(Note.C4, Note.C5).semitones} semitones)")
  
  println("\nCommon intervals:")
  println(s"Perfect Unison: ${Interval.PerfectUnison.semitones} semitones")
  println(s"Major Second: ${Interval.MajorSecond.semitones} semitones")
  println(s"Major Third: ${Interval.MajorThird.semitones} semitones")
  println(s"Perfect Fifth: ${Interval.PerfectFifth.semitones} semitones")
  println(s"Perfect Octave: ${Interval.PerfectOctave.semitones} semitones")
