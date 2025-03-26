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

Treatment <- function(jaspResults, dataset = NULL, options) {
  jaspResults$title <- gettext("Does The Treatment Work?")

  .ln1Intro(jaspResults, options, .ln1TreatIntroText)

  dataset <- .ln1TreatData(jaspResults, dataset, options)

  ready <- !is.null(jaspResults[["simulatedDataState"]])

  .ln1CreateDataPlot(jaspResults, dataset, options, .ln1TreatGetSimulatedDataDependencies)
  .ln1TreatEstimateModel(jaspResults, dataset, options, ready)
  .ln1TreatCreateCoefficientsTable(jaspResults, options, ready)

  return()
}

.ln1TreatIntroText <- function() {
  return(gettext("Welcome to Does The Treatment Work? ..."))
}

.ln1TreatData <- function(jaspResults, dataset, options) {
  if (options[["inputType"]] == "simulateData") {
    if (is.null(jaspResults[["simulatedDataState"]])) {
      dataset <- .ln1TreatSimulateData(options)
      dataState <- createJaspState(object = dataset)
      dataState$dependOn(.ln1TreatGetSimulatedDataDependencies())
      jaspResults[["simulatedDataState"]] <- dataState
    } else {
      dataset <- jaspResults[["simulatedDataState"]]$object
    }
  }

  return(dataset)
}

.ln1TreatSimulateData <- function(options) {
  set.seed(options[["seed"]])

  phaseN <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectN"]])
  phaseEffects <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectSimple"]])
  phaseIntTime <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectInteraction"]])
  phaseNames <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseName"]])

  totalN <- sum(phaseN)

  yNoise <- rnorm(totalN, options[["simDependentMean"]], options[["simDependentSd"]])

  phaseName <- rep(phaseNames, phaseN)
  phaseBeta <- rep(phaseEffects, phaseN)
  phaseInt <- rep(phaseIntTime, phaseN)

  time <- unlist(lapply(phaseN, seq_len))
  timeEffect <- options[["simTimeEffect"]]

  y <- timeEffect * time + phaseBeta + time * phaseInt + yNoise

  simData <- data.frame(
    y = y,
    time = time,
    t = 1:length(time),
    phase = phaseName
  )

  return(simData)
}

.ln1TreatGetSimulatedDataDependencies <- function() {
  return(c(
    "inputType",
    "dependent",
    "time",
    "phase",
    "simDependentMean",
    "simDependentSd",
    "simTimeEffect",
    "simTimeEffectAutocorrelation",
    "simPhaseEffects",
    "seed"
  ))
}

.ln1TreatCreateModelFormula <- function(options) {
  variableNames <- .ln1GetVariableNames(options)

  f <- as.formula(paste(variableNames[["dependent"]], "~", variableNames[["time"]], "*", variableNames[["phase"]]))

  return(f)
}

.ln1TreatEstimateModelHelper <- function(dataset, options) {
  f <- .ln1TreatCreateModelFormula(options)

  mod <- stats::lm(f, data=dataset)

  return(mod)
}

.ln1TreatEstimateModel <- function(jaspResults, dataset, options, ready) {
  if (ready && is.null(jaspResults[["modelState"]])) {
    modelObject <- .ln1TreatEstimateModelHelper(dataset, options)
    modelState <- createJaspState(object = modelObject)
    modelState$dependOn(.ln1TreatGetSimulatedDataDependencies())
    jaspResults[["modelState"]] <- modelState
  }
}

.ln1TreatCreateCoefficientsTable <- function(jaspResults, options, ready) {
  if (is.null(jaspResults[["coefTable"]])) {
    table <- createJaspTable(gettext("Coefficients"))
    table$dependOn(.ln1TreatGetSimulatedDataDependencies())

    table$addColumnInfo(name = "name",         title = "",                        type = "string")
    table$addColumnInfo(name = "coef",         title = gettext("Estimate"),       type = "number")
    table$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")
    table$addColumnInfo(name = "t",            title = gettext("t"),              type = "number")
    table$addColumnInfo(name = "p",            title = gettext("p"),              type = "pvalue")

    overtitle <- gettextf("%.0f%% CI", 100 * options$coefficientCiLevel)

    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

    if (!is.null(jaspResults[["modelState"]]) && ready) {
      .ln1TreatFillCoefficientsTable(table, jaspResults[["modelState"]]$object, options)
    }

    jaspResults[["coefTable"]] <- table
  }
}

# .ln1TreatFormatCoefficientNames <- function(modelObject) {
#   modelTerms <- terms(modelObject)

#   coefNames <- c("(Intercept)")

#   for (name in attr(modelTerms, "term.labels")) {
#     if (grepl(":", name)) {
#       newTerms <- c()
#       for (subName in strsplit(name, ":")[[1]]) {
#         if (subName %in% names(modelObject[["xlevels"]])) {
#           for (level in modelObject[["xlevels"]][[subName]][-1]) {
#             newTerms <- c(newTerms, paste0(subName, " (", level, ")"))
#           }
#         } else {
#           newTerms <- c(newTerms, subName)
#         }
#       }
#       name <- paste(newTerms, collapse = " * ")
#     } else {

#     }
#   }

#   return(coefNames)
# }

.ln1TreatFillCoefficientsTable <- function(table, modelObject, options) {
  print(terms(modelObject))
  modelSummary <- summary(modelObject)
  modelCoefficients <- data.frame(modelSummary[["coefficients"]])

  table[["name"]] <- row.names(modelCoefficients)
  table[["coef"]] <- modelCoefficients[["Estimate"]]
  table[["SE"]] <- modelCoefficients[["Std..Error"]]
  table[["t"]] <- modelCoefficients[["t.value"]]
  table[["p"]] <- modelCoefficients[["Pr...t.."]]

  ci <- confint(modelObject, level = options$coefficientCiLevel)

  table[["lower"]] <- ci[ ,1]
  table[["upper"]] <- ci[ ,2]
}
