#
# Copyright (C) 2025 University of Amsterdam and Netherlands eScience Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

Forecasting <- function(jaspResults, dataset = NULL, options) {
  jaspResults$title <- gettext("Random Effects")

  .ln1Intro(jaspResults, options, .ln1ForeIntroText)

  dataset <- .ln1ForeData(jaspResults, dataset, options)

  ready <- !is.null(jaspResults[["simulatedDataState"]])

  .ln1CreateDataPlot(jaspResults, dataset, options, .ln1ForeGetSimulatedDataDependencies)
  .ln1ForeEstimateModel(jaspResults, dataset, options, ready)

  options[["intercept"]] <- FALSE # Required by .tsCreateTableCoefficients

  jaspTimeSeries:::.tsCreateTableCoefficients(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    options,
    ready,
    2,
    .ln1ForeGetSimulatedDataDependencies()
  )

  jaspTimeSeries:::.tsForecastPlot(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    dataset,
    options,
    ready,
    3,
    c(.ln1ForeGetSimulatedDataDependencies(),
      "forecastLength",
      "forecastTimeSeries",
      "forecastTimeSeriesType",
      "forecastTimeSeriesObserved")
  )

  jaspTimeSeries:::.tsCreateTableForecasts(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    dataset,
    options,
    ready,
    4,
    c(.ln1ForeGetSimulatedDataDependencies(),
      "forecastLength",
      "forecastTable")
  )

  jaspTimeSeries:::.tsSaveForecasts(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    dataset,
    options,
    ready
  )

  return()
}

.ln1ForeIntroText <- function() {
  return(gettext("Welcome to Forecasting ..."))
} 

.ln1ForeData <- function(jaspResults, dataset, options) {
  if (options[["inputType"]] == "simulateData") {
    if (is.null(jaspResults[["simulatedDataState"]])) {
      dataset <- .ln1ForeSimulateData(options)
      dataState <- createJaspState(object = dataset)
      dataState$dependOn(.ln1ForeGetSimulatedDataDependencies())
      jaspResults[["simulatedDataState"]] <- dataState
    } else {
      dataset <- jaspResults[["simulatedDataState"]]$object
    }
  }

  return(dataset)
}

.ln1ForeGetSimulatedDataDependencies <- function() {
  return(c(
    "inputType",
    "dependent",
    "time",
    "covariates",
    "noiseSd",
    "simArEffects",
    "simIEffect",
    "simMaEffects",
    "numSamples",
    "seed"
  ))
}

.ln1ForecastDependencies <- function() {
  return(c(
    "forecastLength", 
  ))
}

.ln1ForeSimulateData <- function(options) {
  set.seed(options[["seed"]])

  arEffects <- sapply(options[["simArEffects"]], function(x) x[["simArEffect"]])
  maEffects <- sapply(options[["simMaEffects"]], function(x) x[["simMaEffect"]])

  y <- stats::arima.sim(
    model = list(
      "ar" = arEffects,
      "ma" = maEffects,
      "order" = c(length(arEffects), options$simIEffect, length(maEffects))
    ),
    n = options$numSamples,
    sd = options$noiseSd
  )

  y <- y[-1]

  simData <- data.frame(
    y = as.numeric(y),
    t = seq_along(y),
    phase = 0
  )

  return(simData)
}

.ln1ForeEstimateModel <- function(jaspResults, dataset, options, ready) {
  if (ready && is.null(jaspResults[["modelState"]])) {
    modelObject <- .ln1ForeEstimateModelHelper(dataset, options)
    modelState <- createJaspState(object = modelObject)
    modelState$dependOn(.ln1ForeGetSimulatedDataDependencies())
    jaspResults[["modelState"]] <- modelState
  }
}

.ln1ForeEstimateModelHelper <- function(dataset, options) {
  variableNames <- .ln1GetVariableNames(options)

  mod <-try(forecast::auto.arima(
    dataset[[variableNames[["dependent"]]]],
    allowdrift = FALSE,
    allowmean = FALSE
  ))

  if (jaspBase::isTryError(mod)) {
    .quitAnalysis(jaspBase::.extractErrorMessage(mod))
  }

  return(mod)
}
