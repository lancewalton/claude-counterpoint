package counterpoint

/**
 * A class representing a two-voice counterpoint.
 * cantus: the given melody (often the lower voice)
 * counterpoint: the second melody (often the upper voice)
 * 
 * The Counterpoint class applies the rules of counterpoint to validate and generate
 * contrapuntal lines against a cantus firmus.
 */
case class Counterpoint(cantus: Melody, counterpoint: Melody):
  private val rules = CounterpointRules()
  private val melodicRules = MelodicRules()
  
  /**
   * Check if the entire counterpoint is valid according to all counterpoint rules.
   */
  def isValid: Boolean =
    if cantus.isEmpty || counterpoint.isEmpty then
      true
    else if cantus.size != counterpoint.size then
      false
    else
      // Check each pair of notes
      val cantusNotes = cantus.notes
      val counterpointNotes = counterpoint.notes
      
      // Check each pair and also adjacent pairs for motion rules
      val allPairsValid = (cantusNotes zip counterpointNotes).forall {
        case (cantusNote, counterpointNote) =>
          // Check voice crossing and spacing
          rules.noVoiceCrossingRule(cantusNote, counterpointNote) &&
          rules.properVoiceSpacingRule(cantusNote, counterpointNote) &&
          rules.consonantIntervalRule(cantusNote, counterpointNote)
      }
      
      // Check motion between pairs of notes
      val allMotionsValid = (0 until cantusNotes.size - 1).forall { i =>
        val cantusNote = cantusNotes(i)
        val cantusNextNote = cantusNotes(i + 1)
        val counterpointNote = counterpointNotes(i)
        val counterpointNextNote = counterpointNotes(i + 1)
        
        rules.noParallelPerfectFifthsRule(
          cantusNote, cantusNextNote, counterpointNote, counterpointNextNote
        ) &&
        rules.noParallelOctavesRule(
          cantusNote, cantusNextNote, counterpointNote, counterpointNextNote
        ) && 
        rules.noDirectMotionToPerfectConsonanceRule(
          cantusNote, cantusNextNote, counterpointNote, counterpointNextNote
        ) &&
        rules.dissonanceResolutionRule(
          cantusNote, cantusNextNote, counterpointNote, counterpointNextNote
        )
      }
      
      allPairsValid && allMotionsValid
      
  /**
   * Get all possible valid next notes for the counterpoint melody,
   * given the next note in the cantus firmus.
   */
  def validNextCounterpointNotes(cantusNextNote: Note): List[Note] =
    if cantus.isEmpty || counterpoint.isEmpty then
      // If we're just starting, allow any note that makes a consonant interval
      Note.allNotes.filter { note =>
        rules.noVoiceCrossingRule(cantusNextNote, note) &&
        rules.properVoiceSpacingRule(cantusNextNote, note) &&
        rules.consonantIntervalRule(cantusNextNote, note)
      }
    else
      // Get the melodic constraints
      val lastCounterpointNote = counterpoint.lastNote.get
      val melodicallyValidNotes = Note.allNotes.filter { note =>
        counterpoint.validNextNotes(lastCounterpointNote).contains(note)
      }
      
      // Apply counterpoint rules
      val lastCantusNote = cantus.lastNote.get
      melodicallyValidNotes.filter { note =>
        // Voice-to-voice (intervallic) rules
        rules.noVoiceCrossingRule(cantusNextNote, note) &&
        rules.properVoiceSpacingRule(cantusNextNote, note) &&
        rules.consonantIntervalRule(cantusNextNote, note) &&
        
        // Motion rules
        rules.noParallelPerfectFifthsRule(
          lastCantusNote, cantusNextNote, lastCounterpointNote, note
        ) &&
        rules.noParallelOctavesRule(
          lastCantusNote, cantusNextNote, lastCounterpointNote, note
        ) && 
        rules.noDirectMotionToPerfectConsonanceRule(
          lastCantusNote, cantusNextNote, lastCounterpointNote, note
        ) &&
        rules.dissonanceResolutionRule(
          lastCantusNote, cantusNextNote, lastCounterpointNote, note
        )
      }
      
  /**
   * Generate a valid counterpoint line against the cantus firmus.
   * This is a simple implementation that just picks the first valid note at each step.
   */
  def generateCounterpoint: Option[Melody] =
    val cantusNotes = cantus.notes
    
    if cantusNotes.isEmpty then
      return Some(Melody())
    
    // Start with an empty melody
    var currentCounterpoint = Melody()
    
    // Try to add a valid note for each cantus note
    for cantusNote <- cantusNotes do
      val validNotes = validNextCounterpointNotes(cantusNote)
      
      if validNotes.isEmpty then
        return None  // No valid counterpoint possible
      
      // Add the first valid note
      currentCounterpoint = currentCounterpoint.addNote(validNotes.head)
    
    Some(currentCounterpoint)