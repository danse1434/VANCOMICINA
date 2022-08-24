require(lixoftConnectors)
initializeLixoftConnectors()
require(glue)

bootdir <- file.path('Run002b_Final_Sesgo_Aditivo_Variable_ref_quimiolum', 'results')
i = 1

loadProject(file.path(bootdir, paste0('B', 2), 'Run002b_Final_Sesgo_Aditivo_Variable_ref_quimiolum.mlxtran'))

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
