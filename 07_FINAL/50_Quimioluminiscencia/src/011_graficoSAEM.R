require(lixoftConnectors)
initializeLixoftConnectors()
require(glue)
require(progress)

rundir <- "run200"
bootdir <- file.path(rundir, "boot_results")

pb <- progress_bar$new(total = 1000)

for (i in 1:1000) {
  pb$tick()
  
  id <- sprintf("%03d", i)
  
  loadProject(file.path(bootdir, glue("B{id}"), 'run200.mlxtran'))
  
  getSAEMiterations()
  
  scenario = getScenario()
  scenario$plotList = c("saemresults")
  setScenario(scenario)
  computeChartsData()
}







# runPopulationParameterEstimation()
# runStandardErrorEstimation()
# 
# scenario$tasks = c(populationParameterEstimation = T, plots = TRUE, standardErrorEstimation=TRUE)
# scenario$linearization = FALSE
# 
# runScenario()
# 
# 
# getScenario()
