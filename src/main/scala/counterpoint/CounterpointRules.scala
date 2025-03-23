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
      // Check if both intervals are perfect fifths
      val lastInterval = Interval.between(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = Interval.between(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      !(lastInterval.isPerfectFifth && candidateInterval.isPerfectFifth)
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true
      
  def noParallelPerfectOctavesRule(
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
      // Check if both intervals are perfect octaves
      val lastInterval = Interval.between(lowerVoiceLastNote, upperVoiceLastNote)
      val candidateInterval = Interval.between(lowerVoiceCandidateNote, upperVoiceCandidateNote)
      
      !(lastInterval.isPerfectOctave && candidateInterval.isPerfectOctave)
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true
      
  def noParallelOctavesAndCompoundsRule(
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
      
      val (lastSimpleName, _) = lastInterval.getSimpleForm
      val (candidateSimpleName, _) = candidateInterval.getSimpleForm
      
      // Check for octaves in their simple form (handles compounds)
      !(lastSimpleName == IntervalName.Octave && 
        candidateSimpleName == IntervalName.Octave && 
        lastInterval.quality == IntervalQuality.Perfect && 
        candidateInterval.quality == IntervalQuality.Perfect)
    else
      // If voices move in different directions or one of them doesn't move, 
      // it's not parallel motion, so this rule doesn't apply
      true