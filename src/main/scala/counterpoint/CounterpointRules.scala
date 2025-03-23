package counterpoint

case class CounterpointRules():
  import IntervalOps._
  
  def noParallelPerfectFifthsRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val lastMovementDirection = getDirection(lowerVoiceLastNote, lowerVoiceCandidateNote)
    val upperMovementDirection = getDirection(upperVoiceLastNote, upperVoiceCandidateNote)
    
    // If both voices move in the same direction
    if lastMovementDirection == upperMovementDirection && lastMovementDirection != 0 then
      // Check if both intervals are perfect fifths
      val lastInterval = getInterval(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = getInterval(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      !(isPerfectFifth(lastInterval) && isPerfectFifth(candidateInterval))
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true