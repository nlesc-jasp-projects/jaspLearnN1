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
	}

	InputListView
	{
		id: problems
		name: "problems"
		title: qsTr("Problems")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		optionKey: "problem"
		defaultValues: ["Problem 1", "Problem 2", "Problem 3"]
		placeHolder: qsTr("New Problem")
		minRows: 2
		maxRows: 10
		rowComponentTitle: "Severity"

		rowComponent: Slider
		{
			name: "problemSeverity"
			value: 0.5
			min: 0
			max: 1
			vertical: false
		}
	}

	TabView
	{
		id: connectionList
		name: "connectionList"
		// title: qsTr("Problem Connections")
		maximumItems: 10
		newItemName: qsTr("Time ") + (connectionList.count + 1)
		optionKey: "name"
		content: Group
		{
			childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

			ComponentsList
			{
				id: connections
				name: "connections"
				title: qsTr("Problem Connections")
				preferredWidth: connectionList.width - 2 * jaspTheme.contentMargin
				minimumItems: 0
				maximumItems: 20
				headerLabels: [qsTr("From"), qsTr("To"), qsTr("Strength")]
				defaultValues: connectionList.count === 1 ? [
					{"connectionFrom": qsTr("Problem 1"), "connectionTo": qsTr("Problem 2"), "connectionStrength": 0.5},
					{"connectionFrom": qsTr("Problem 2"), "connectionTo": qsTr("Problem 3"), "connectionStrength": -0.5},
					{"connectionFrom": qsTr("Problem 1"), "connectionTo": qsTr("Problem 3"), "connectionStrength": -0.5}
				] : [{"connectionFrom": "", "connectionTo": "", "connectionStrength": 0}]
				rowComponent: RowLayout
				{
					Layout.columnSpan: 4

					spacing: 72 * preferencesModel.uiScale

					DropDown
					{
						id: from
						name: "connectionFrom"
						source: problems
						addEmptyValue: true
						onCurrentValueChanged:
						{
							if (to.currentValue == currentValue)
								addControlError("Same value!")
							else
							{
								clearControlError()
								to.clearControlError()
							}
						}
					}

					DropDown
					{
						id: to
						name: "connectionTo"
						source: problems
						addEmptyValue: true
						onCurrentValueChanged:
						{
							if (from.currentValue == currentValue)
								addControlError("Same value!")
							else
							{
								clearControlError()
								from.clearControlError()
							}
						}
					}

					Slider
					{
						name: "connectionStrength"
						value: 0
						min: -1
						max: 1
						vertical: false
					}
				}
			}

			Group
			{
				title: qsTr("Options")

				columns: 2

				CheckBox
				{
					name: "plotNetwork"
					label: qsTr("Network plot")
					checked: true
				}

				CheckBox
				{
					name: "centrality"
					label: qsTr("Centrality statistics")
					checked: false
				}
			}
		}
	}

	FileSelector
	{
		name:	"problemSavePath"
		label:	qsTr("Save problems")
		filter:	"*.csv"
		save:	true
	}

	FileSelector
	{
		name:	"connectionSavePath"
		label:	qsTr("Save connections")
		filter:	"*.csv"
		save:	true
	}

	Section
	{
		title: qsTr("Plots")

		columns: 1

		Group
		{
			columns: 1

			DropDown
			{
				id: layout
				name: "plotLayout"
				label: qsTr("Layout")
				values: [
					
					{ label: qsTr("Circular"), value: "linear" },
					{ label: qsTr("Sugiyama"), value: "sugiyama" }
				]
			}

			DropDown
			{
				name: "colorPalette"
				label: qsTr("Color palette")
				values: [
					{ label: qsTr("Viridis"),	value: "viridis" },
					{ label: qsTr("Gray"),		value: "gray"	 },
					{ label: qsTr("Blue"),		value: "blue"	 }
				]
			}
		}

		Group
		{
			columns: 2

			Group {
				title: qsTr("Problem Severity")

				CheckBox
				{
					name: "plotSeverityFill"
					label: qsTr("Color")
				}

				CheckBox
				{
					name: "plotSeveritySize"
					label: qsTr("Size")
					checked: true
				}

				CheckBox
				{
					name: "plotSeverityAlpha"
					label: qsTr("Opacity")
				}
			}

			Group {
				title: qsTr("Connection Strength")

				CheckBox
				{
					name: "plotStrengthColor"
					label: qsTr("Color")
					checked: true
				}

				CheckBox
				{
					name: "plotStrengthWidth"
					label: qsTr("Width")
					checked: true
				}

				CheckBox
				{
					name: "plotStrengthAlpha"
					label: qsTr("Opacity")
				}
			}
		}
	}
}
