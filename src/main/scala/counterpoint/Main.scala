package counterpoint

@main def main(): Unit = 
  println("C Major Scale from G2 to G5:")
  Note.cMajorScale.foreach(println)
