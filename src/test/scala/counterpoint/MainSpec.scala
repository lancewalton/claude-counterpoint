package counterpoint

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyFlatSpec with Matchers:
  "Main" should "run without errors" in {
    // Simple test to ensure the main method runs without throwing exceptions
    noException should be thrownBy {
      main()
    }
  }
