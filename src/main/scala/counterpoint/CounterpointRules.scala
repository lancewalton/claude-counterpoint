package counterpoint

case class CounterpointRules():
  import MotionType.Type
  
  def noParallelPerfectFifthsRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val motionType = MotionType.getMotionType(
      lowerVoiceLastNote,
      lowerVoiceCandidateNote,
      upperVoiceLastNote,
      upperVoiceCandidateNote
    )
    
    // If both voices move in parallel motion
    if motionType == Type.Parallel then
      // Get intervals and their simple form
      val lastInterval = Interval.between(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = Interval.between(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      val (lastSimpleName, lastQuality) = lastInterval.getSimpleForm
      val (candidateSimpleName, candidateQuality) = candidateInterval.getSimpleForm
      
      // Check for perfect fifths in their simple form (handles compounds)
      !(lastSimpleName == IntervalName.Fifth &&
        candidateSimpleName == IntervalName.Fifth && 
        lastQuality == IntervalQuality.Perfect && 
        candidateQuality == IntervalQuality.Perfect)
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true
      
  def noParallelOctavesRule(
    lowerVoiceLastNote: Note,
    lowerVoiceCandidateNote: Note,
    upperVoiceLastNote: Note,
    upperVoiceCandidateNote: Note
  ): Boolean =
    val motionType = MotionType.getMotionType(
      lowerVoiceLastNote,
      lowerVoiceCandidateNote,
      upperVoiceLastNote,
      upperVoiceCandidateNote
    )
    
    // If both voices move in parallel motion
    if motionType == Type.Parallel then
      // Get intervals and their simple form
      val lastInterval = Interval.between(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = Interval.between(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      val (lastSimpleName, lastQuality) = lastInterval.getSimpleForm
      val (candidateSimpleName, candidateQuality) = candidateInterval.getSimpleForm
      
      // Check for octaves in their simple form (handles compounds)
      !(lastSimpleName == IntervalName.Octave && 
        candidateSimpleName == IntervalName.Octave && 
        lastQuality == IntervalQuality.Perfect && 
        candidateQuality == IntervalQuality.Perfect)
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true