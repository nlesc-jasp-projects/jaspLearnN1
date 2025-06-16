# Tests for analysis "How Will Symptoms Develop?"

test_that("Simulated data input works", {
  options <- analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$forecastSave <- ""
  options$simArEffects <- list(list(simArLag = 1, simArEffect = 0.1), list(simArLag = 1,
      simArEffect = 0.2))
  options$simMaEffects <- list(list(simMaLag = 1, simMaEffect = 0.5))
  set.seed(1)
  results <- runAnalysis("Forecasting", "debug.csv", options)

	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("TRUE", 0.149081861166404, "AR(1)", 1.41608222598456, 1.12015707043668,
			 1.77635683940025e-15, 9.49868894113107, 1.71200738153244, "FALSE",
			 0.0980749639769158, "AR(2)", -0.584911226756313, -0.779588491603621,
			 4.09471490048219e-08, -5.96391987351619, -0.390233961909005,
			 "TRUE", 0.1516836160891, "MA(1)", -0.770534231597206, -1.07162382984906,
			 1.85583795664357e-06, -5.07987778419385, -0.469444633345348
			))

	plotName <- results[["results"]][["dataPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-plot")
})
