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
  return(gettext("Forecasting is an analysis to make predictions on the future development of a dynamic process.  You can think of making predicting on the weather or on the economy. In clinical practice, forecasting can be applied to the symptom develop of a client during treatment. This helps to anticipate new developments. For example when the expectation is that a client wont improve much, a therapist and client can discuss if a different therapeutic strategy is needed. However, if it is expected that more progress will be made, a treatment should of course continue as planned. 

<b>How does it work?</b> 

These analyses can predict a patient's future mental health states based on their past symptom trajectories, helping therapists anticipate potential crises or treatment responses. They require a structured dataset with frequent, time-stamped observations of symptoms, behaviors, and external influences. The notable aspect of forecasting models is their emphasis on trend detection and anomaly identification, which aids in early intervention strategies. By leveraging past data, therapists can proactively adjust treatment plans rather than reacting to deteriorations after they occur. However, the accuracy of predictions depends on the quality and granularity of the available data.

An important aspect of applying forecasting in clinical practice is therefore how frequent measurements are collected. In general, one can say that the more frequent data is being collected, to better the predictions are expected to be. However, one most also consider what is feasible for a client, and at what level meaningful fluctuations occur. For instance, if the symptoms do not really vary daily, it would not help to measure these daily. On the other hand, if one measures only once every three months, you can miss important changes in the treatment process, and this will make predictions on future developments less accurate. As a rule of thumb, you can consider measuring weekly.  

Forecasting is an umbrella term that captures many different analysis techniques, which a re also implemented in JASP. This tutorial is based on the autoregressive integrated moving average (ARIMA) a gold standard in forecasting. The 'autoregressive' (AR) part of ARIMA indicates that the evolving variable of interest is regressed on its prior values. The 'moving average' (MA) part indicates that the regression error is a linear combination of error terms whose values occurred contemporaneously and at various times in the past.[3] The 'integrated' (I) part indicates that the data values have been replaced with the difference between each value and the previous value. Other techniques include baysian forecasting implementen in the prophet module [LINK].
"))
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
