//
// Copyright (C) 2025 University of Amsterdam and Netherlands eScience Center
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

import "./common" as Common

Form
{
	Group
	{
		columns: 2

		Common.IntroText{}

		CheckBox
		{
			name: "plotData"
			label: qsTr("Plot data")
			checked: true
		}
	}

	Section
	{
		title: qsTr("Data")
		id: sectionData
		columns:1

		Common.InputType
		{
			id: inputType
		}

		VariablesForm
		{
			visible: inputType.value == "loadData"

			AvailableVariablesList { name: "allVariablesList" }
			AssignedVariablesList
			{
				name:				"dependent"
				title:				qsTr("Dependent Variable")
				allowedColumns:		["scale"]
				singleVariable:		true
			}
			AssignedVariablesList
			{
				name:			"time"
				title:			qsTr("Time")
				singleVariable: true
			}
			AssignedVariablesList
			{
				name:			"covariates"
				title:			qsTr("Covariates")
				allowedColumns:	["scale"]
			}
		}

		Group
		{
			title: qsTr("Simulation Options")
			visible: inputType.value == "simulateData"

			columns: 1

			Group
			{
				columns: 2

				DoubleField
				{
					name: "noiseSd"
					label: "Noise std. deviation"
					defaultValue: 1.0
				}

				IntegerField
				{
					name: "numSamples"
					label: qsTr("N")
					defaultValue: 100
				}
			}

			Group
			{
				columns: 1

				Group
				{
					title: qsTr("Autoregressive (AR) order p")

					ComponentsList
					{
						name: "simArEffects"
						preferredWidth: (sectionData.width - 8 * jaspTheme.contentMargin) / 2
						headerLabels: [qsTr("Lag"), qsTr("Effect")]
						defaultValues: [
							{"simArLag": 1, "simArEffect": 0.2}
						]
						rowComponent: RowLayout
						{
							IntegerField
							{
								name: "simArLag"
								enabled: false
								defaultValue: rowIndex + 1
							}
						
							DoubleField
							{
								name: "simArEffect"
								defaultValue: 0.2
								negativeValues: true
							}
						}
					}
				}

				IntegerField
				{
					name:   "simIEffect"
					id:     d
					label:  qsTr("Difference (I) degree d")
					defaultValue: 1
				}

				Group
				{
					title: qsTr("Moving average (MA) order q")

					ComponentsList
					{
						name: "simMaEffects"
						preferredWidth: (sectionData.width - 8 * jaspTheme.contentMargin) / 2
						headerLabels: [qsTr("Lag"), qsTr("Effect")]
						defaultValues: [
							{"simMaLag": 1, "simMaEffect": 0.8}
						]
						rowComponent: RowLayout
						{
							IntegerField
							{
								name: "simMaLag"
								enabled: false
								defaultValue: rowIndex + 1
							}
						
							DoubleField
							{
								name: "simMaEffect"
								defaultValue: 0.8
								negativeValues: true
							}
						}
					}
				}
			}

			IntegerField
			{
				name: "seed"
				label: qsTr("Seed")
				defaultValue: 1
			}
		}
	}

	Section
	{
		title: qsTr("Options")

		CIField { name: "coefficientCiLevel"; label: qsTr("Confidence interval")}
	}

	Section
    {
        title: qsTr("Forecasting")
        IntegerField
        {
            name: "forecastLength"
            id: forecastLength
            label: qsTr("Number of forecasts")
            min: 0
            max: 1e6
            defaultValue: 0
            info: qsTr("Determines the number forecasts to make.")
        }
        FileSelector
        {
            name:	             "forecastSave"
            label:	            qsTr("Save forecasts as")
            placeholderText:    qsTr("e.g. forecasts.csv")
            filter:	            "*.csv"
            save:	              true
            enabled:            forecastLength.value > 0
            fieldWidth:         180 * preferencesModel.uiScale
            info:               qsTr("Saves the forecasts in a seperate .csv file.")
        }
        CheckBox
        {
            name:     "forecastTimeSeries"
            id:       forecastTimeSeries
            label:    qsTr("Time series plot")
            info:     qsTr("Plots the forecasts (and observed values) (y-axis) over time (x-axis)")
            RadioButtonGroup
            {
                name:	"forecastTimeSeriesType"
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points") }
                RadioButton { value: "line";	label: qsTr("Line") }
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
            CheckBox
            {
                name:       "forecastTimeSeriesObserved"
                id:         forecastTimeSeriesObserved
                label:      qsTr("Observed data")
                checked:    true
            }
        }
        CheckBox
        {
            name:   "forecastTable"
            id:     forecastTable
            label:  qsTr("Forecasts table")
        }
    }
}
