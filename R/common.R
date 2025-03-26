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

.ln1Intro <- function(jaspResults, options, textFun) {
  if (options[["enableIntroText"]] && is.null(jaspResults[["introText"]])) {
    introText <- createJaspHtml(
      textFun(),
      title = gettext("Introduction"),
      position = 1
    )
    introText$dependOn("enableIntroText")

    jaspResults[["introText"]] <- introText
  }
}

.ln1CreateDataPlot <- function(jaspResults, dataset, options, dependencyFun) {
  if (options[["plotData"]] && is.null(jaspResults[["dataPlot"]])) {
    dataPlot <- createJaspPlot(
      title = gettext("Data plot"),
      height = 480,
      width = 480,
      position = 2
    )
    dataPlot$dependOn(c("plotData", dependencyFun()))
    dataPlot$plotObject <- .ln1CreateDataPlotFill(dataset, options)
    # jaspDescriptives::.tsFillTimeSeriesPlot(dataPlot, dataset, options, "both", "none")
    jaspResults[["dataPlot"]] <- dataPlot
  }
}

.ln1GetVariableNames <- function(options) {
  varList <- switch(options[["inputType"]],
    "simulateData" = list("time" = "time", "dependent" = "y", "phase" = "phase"),
    "loadData" = list("time" = options[["time"]], "dependent" = options[["dependent"]], "phase" = options[["phase"]])
  )

  return(varList)
}

.ln1CreateDataPlotFill <- function(dataset, options) {
  variableNames <- .ln1GetVariableNames(options)

  yName <- variableNames[["dependent"]]

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[["t"]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[yName]])

  p <- ggplot2::ggplot(
      dataset,
      ggplot2::aes(
        x = .data[["t"]],
        y = .data[[yName]],
        color = .data[[variableNames[["phase"]]]]
      )
    ) +
    jaspGraphs::geom_point() +
    jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
