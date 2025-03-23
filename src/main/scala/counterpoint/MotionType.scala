package counterpoint

import scala.annotation.targetName

object MotionType:
  enum Type:
    case Parallel, Similar, Contrary, Oblique

  def getMotionType(
    firstVoiceNote1: Note,
    firstVoiceNote2: Note,
    secondVoiceNote1: Note,
    secondVoiceNote2: Note
  ): Type =
    import Type._
    
    val firstDirection = Interval.direction(firstVoiceNote1, firstVoiceNote2)
    val secondDirection = Interval.direction(secondVoiceNote1, secondVoiceNote2)
    
    (firstDirection, secondDirection) match
      case (d1, d2) if d1 == 0 || d2 == 0 => Oblique
      case (d1, d2) if d1 == d2 => Parallel
      case (d1, d2) if d1 * d2 > 0 => Similar
      case _ => Contrary