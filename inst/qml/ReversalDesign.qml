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

Form
{
	Group
	{
		columns: 2

		CheckBox
		{
			name: "enableIntroText"
			label: qsTr("Introductory text")
		}

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

		RadioButtonGroup
		{
			name: "inputType"
			title: qsTr("Input Type")
			columns: 3
			
			RadioButton { value: "simulateData"; id: simulateData; label: qsTr("Simulate data"); checked: true }
			RadioButton { value: "loadData"; id: loadData; label: qsTr("Load data") }
			RadioButton { value: "enterData"; id: enterData; label: qsTr("Enter data") }
		}

		VariablesForm
		{
			visible: loadData.checked

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
			}
			AssignedVariablesList
			{
				name:			"phase"
				title:			qsTr("Phase Variable")
				allowedColumns:	["nominal"]
			}
		}

		Group
		{
			title: qsTr("Simulation Options")
			visible: simulateData.checked

			Group
			{
				columns: 3

				Group
				{
					title: qsTr("Dependent Variable")

					DoubleField
					{
						name: "simDependentMean"
						label: "Mean"
						defaultValue: 0.0
					}

					DoubleField
					{
						name: "simDependentSd"
						label: "Standard deviation"
						defaultValue: 1.0
					}
				}

				Group
				{
					title: qsTr("Time")

					DoubleField
					{
						name: "simTimeEffect"
						label: qsTr("Effect")
						defaultValue: 0.0
						negativeValues: true
					}

					DoubleField
					{
						name: "simTimeEffectAutocorrelation"
						label: qsTr("Auto-correlation")
						defaultValue: 1.0
						min: -1
						max: 1
						negativeValues: true
					}
				}

				IntegerField
				{
					name: "seed"
					label: qsTr("Seed")
					defaultValue: 1
				}
			}

			Group
			{
				title: qsTr("Phase Effect")

				ComponentsList
				{
					id: simPhaseEffects
					name: "simPhaseEffects"
					preferredWidth: sectionData.width - 8 * jaspTheme.contentMargin
					minimumItems: 1
					headerLabels: [qsTr("Name"), qsTr("Phase"), qsTr("Phase × Time"), qsTr("n")]
					defaultValues: [
						{"simPhaseName": "Pre-treament", "simPhaseEffectSimple": 0.0, "simPhaseEffectInteraction": 0.0, "simPhaseEffectN": 20},
						{"simPhaseName": "Treatment", "simPhaseEffectSimple": 5.0, "simPhaseEffectInteraction": 0.0, "simPhaseEffectN": 20},
						{"simPhaseName": "Post-treatment", "simPhaseEffectSimple": 5.0, "simPhaseEffectInteraction": -0.1, "simPhaseEffectN": 20}
					]
					rowComponent: RowLayout
					{
						Layout.columnSpan: 4

						spacing: 72 * preferencesModel.uiScale

						TextField
						{
							name: "simPhaseName"
							defaultValue: qsTr("Phase ") + (rowIndex + 1)
						}

						DoubleField
						{
							name: "simPhaseEffectSimple"
							defaultValue: 0.0
							negativeValues: true
						}

						DoubleField
						{
							name: "simPhaseEffectInteraction"
							defaultValue: 0.0
							negativeValues: true
						}

						IntegerField
						{
							name: "simPhaseEffectN"
							defaultValue: 20
						}
					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Model")
	}

	Section
	{
		title: qsTr("Options")

		CIField { name: "coefficientCiLevel"; label: qsTr("Confidence interval")}
	}

	Section
	{
		title: qsTr("Plots")
	}
}
