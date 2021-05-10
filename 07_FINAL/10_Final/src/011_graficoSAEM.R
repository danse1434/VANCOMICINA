require(lixoftConnectors)
initializeLixoftConnectors()
require(glue)

bootdir <- file.path('FINAL', 'results')
i = 1

loadProject(file.path(bootdir, paste0('B', 2), 'FINAL.mlxtran'))

getSAEMiterations()

runPopulationParameterEstimation()
runStandardErrorEstimation()
scenario = getScenario()

scenario$tasks = c(populationParameterEstimation = T, plots = TRUE, standardErrorEstimation=TRUE)
scenario$linearization = FALSE
scenario$plotList = c("saemresults")
setScenario(scenario)

runScenario()
computeChartsData()


getScenario()
