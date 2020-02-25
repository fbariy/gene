package gene

import gene.Gene.Population
import gene.RNG._
import gene.State._

object Generator {
  def generator(populationSize: Int)(maxGene: Int, minGene: Int)(n: Int): State[Long, Population] =
    sequence(List.fill(populationSize)(ints(n)(maxGene, minGene)))
}
