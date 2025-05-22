package intentions

import model.Observation

trait ScoredIntention extends Intention {
  def score(observation: Observation): Double
}

