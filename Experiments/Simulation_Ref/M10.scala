import sessl._
import sessl.mlrules._

execute {
  new Experiment with Observation with ParallelExecution with CSVOutput {

    model = "../../Model/Model_publication.mlrj"

    simulator = SimpleSimulator()
    parallelThreads = -1

    val stoppingTime = 1440
    stopTime = stoppingTime
    replications = 10

    // Set initial species count as well as experiment specific reaction rate
    // coefficients
    set( 
      "nRos" <~ 0,
      "nICAT" <~ 1200, 
      "nSox" <~ 100, 
      "kTcfSyn" <~ 100,
      "nTCF" <~  7714, 
      "kICATsyn" <~ 250
    )

  scan("kRosSyn" <~ (0))


    observe("bCat_Nuc" ~ count("Cell/Nuc/Bcat"))
    observe("bCat_Cyt" ~ count("Cell/Bcat"))
    observe("Axin" ~ count("Cell/Dummy"))
    observe("Axin_real" ~ count("Cell/Axin"))

    observeAt(range(0, 10, stoppingTime))

    withRunResult(writeCSV)
  }
}
