# Tests for analysis "Does the Treatment Work?"

test_that("Simulated data input works", {
  options <- analysisOptions("Treatment")
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$simPhaseEffects <- list(list(simPhaseName = "pre", simPhaseEffectSimple = 0, simPhaseEffectInteraction = 0,
      simPhaseEffectN = 20), list(simPhaseName = "treat", simPhaseEffectSimple = 5,
      simPhaseEffectInteraction = 0, simPhaseEffectN = 20), list(
      simPhaseName = "post", simPhaseEffectSimple = 5, simPhaseEffectInteraction = -0.1,
      simPhaseEffectN = 20))
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("Treatment", dataset, options)

  table <- results[["results"]][["autoCorTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.0656162298976082, -0.226118382049556, "AR(1)", 0.346548815538686
			))

	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.551363303786328, 4.88221430164827, 3.77679743362012, "(Intercept)",
			 4.21992004511858e-12, 8.85480456918529, 5.98763116967642, 0.0376419437310569,
			 -0.0627102719817359, -0.138177825335272, "time", 0.101511186874476,
			 -1.66596795398735, 0.0127572813718004, 0.638745629273717, -4.80992001802711,
			 -6.09052790057863, "phasepre", 5.67593959671081e-10, -7.530258991355,
			 -3.52931213547559, 0.640802162905123, -0.140736691665649, -1.42546767590024,
			 "phasetreat", 0.826990704437069, -0.219625806235748, 1.14399429256894,
			 0.0532320886028335, 0.0811657736001533, -0.025558138306659,
			 "time:phasepre", 0.133156578295302, 1.52475275215545, 0.187889685506966,
			 0.0534916972063876, 0.081709821691388, -0.0255345741277254,
			 "time:phasetreat", 0.132467616741153, 1.52752344679074, 0.188954217510501
			))

  plotName <- results[["results"]][["dataPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-plot")
})
