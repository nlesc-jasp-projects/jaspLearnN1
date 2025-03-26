import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspLearnN1"
	title		: qsTr("Learn N=1")
	description	: qsTr("Discover analyses for N=1 data.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	preloadData : true

	Analysis
	{
		title	:	qsTr("Does The Treatment Work?")
		func	:	"Treatment"
		qml		:	"Treatment.qml"
	}

	Analysis
	{
		title	:	qsTr("Forecasting")
		func	:	"Forecasting"
		qml		:	"Forecasting.qml"
	}
}
