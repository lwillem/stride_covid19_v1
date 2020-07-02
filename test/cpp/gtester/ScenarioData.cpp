
/*
 *  This is free software: you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *  The software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  You should have received a copy of the GNU General Public License
 *  along with the software. If not, see <http://www.gnu.org/licenses/>.
 *
 *  Copyright 2020 Willem L, Kuylen E, Stijven S & Broeckhove J
 */

/**
 * @file
 * Implementation of scenario tests data.
 */

#include "ScenarioData.h"

#include "util/RunConfigManager.h"

#include <boost/property_tree/ptree.hpp>
#include <map>

using namespace std;
using namespace stride;
using namespace stride::util;
using boost::property_tree::ptree;

namespace Tests {

tuple<ptree, unsigned int, double> ScenarioData::Get(string tag)
{

	// Set model config (default = Measles)
	ptree pt = RunConfigManager::Create("TestsMeasles");


	// ... or set model config for influenza simulations
	if(tag.substr(0, 9) == "influenza"){
		pt = RunConfigManager::Create("TestsInfluenza");
	}

	// ... or set model config for covid19 simulations
	if(tag.substr(0, 7) == "covid19"){
		pt = RunConfigManager::Create("TestsCovid19");
	}

	// Set target values per scenario
	const map<string, unsigned int> targets_default = {
		{"influenza_a", 500000U}, {"influenza_b", 0U}, {"influenza_c", 5U}, {"measles_16", 47000U},
		{"measles_26", 600000U},  {"r0_0", 1200U},     {"r0_4", 3400U},     {"r0_8", 9500U},
		{"r0_12", 24000U},        {"r0_16", 47000U},   {"covid19_base", 80000U}, {"covid19_all", 80000U},
		{"covid19_daily", 88000U},{"covid19_distancing", 19000U}, {"covid19_age_15min",88000U},
		{"covid19_householdclusters", 49000U}, {"covid19_tracing",37000U}, {"covid19_tracing_all",37000U}};

	// Set margins per scenario
	const map<string, double> margins_default = {
		{"influenza_a", 1.0e-02}, {"influenza_b", 0.0}, {"influenza_c", 2.0e-02}, {"measles_16", 1.0e-01},
		{"measles_26", 5.0e-02},  {"r0_0", 5.0e-02},    {"r0_4", 1.0e-01},        {"r0_8", 1.0e-01},
		{"r0_12", 5.0e-02},       {"r0_16", 5.0e-02},   {"covid19_base", 1.0e-01},  {"covid19_all", 1.0e-01},
		{"covid19_daily", 1.0e-01},{"covid19_distancing", 1.0e-01},{"covid19_age_15min",1.0e-1},
		{"covid19_householdclusters", 1.0e-01}, {"covid19_tracing",1.0e-01}, {"covid19_tracing_all",1.0e-01}};

	unsigned int target;
	double       margin;
	target = targets_default.at(tag);
	margin = margins_default.at(tag);

	// Adjust some  parameters, according the scenario
	if (tag == "influenza_b") {
			pt.put("run.num_infected_seeds", 0);
	}
	if (tag == "influenza_c") {
			pt.put("run.num_infected_seeds", 5);
			pt.put("run.immunity_rate", 0.9991);
	}
	if (tag == "measles_16") {
			pt.put("run.r0", 16U);
	}
	if (tag == "measles_26") {
			pt.put("run.r0", 26U);
			pt.put("run.immunity_rate", 0U);
			pt.put("run.vaccine_rate", 0U);
			pt.put("run.num_days", 200U);
	}

	if (tag == "r0_0") {
			pt.put("run.r0", 0.0);
	}
	if (tag == "r0_4") {
			pt.put("run.r0", 4.0);
	}
	if (tag == "r0_8") {
			pt.put("run.r0", 8.0);
	}
	if (tag == "r0_12") {
			pt.put("run.r0", 12.0);
	}
	if (tag == "r0_16") {
			pt.put("run.r0", 16.0);
	}

	if (tag == "covid19_all") {
		pt.put("run.event_log_level", "All");
	}
	if (tag == "covid19_daily") {
			pt.put("run.num_daily_imported_cases", 10U);
	}
	if (tag == "covid19_distancing") {
			pt.put("run.holidays_file","calendar_belgium_2020_covid19_april.json");
			pt.put("run.cnt_reduction_workplace",0.3);
			pt.put("run.cnt_reduction_other",0.4);
			pt.put("run.compliance_delay_workplace",2);
			pt.put("run.compliance_delay_other",3);
	}
	if (tag == "covid19_age_15min") {
			pt.put("run.disease_config_file", "disease_covid19_age_15min.xml");
			pt.put("run.age_contact_matrix_file", "contact_matrix_flanders_conditional_teachers_15min.xml");
	}
	if (tag == "covid19_householdclusters") {
			pt.put("run.holidays_file", "calendar_belgium_2020_covid19_exit_school_adjusted.json");
			pt.put("run.start_date", "2020-06-01");
			pt.put("run.population_file", "pop_belgium600k_c500_teachers_censushh_extended3_size2.csv");
			pt.put("run.cnt_intensity_householdCluster", 4/7);
	}
	// set default tracing parameters
	if (tag == "covid19_tracing" || tag == "covid19_tracing_all") {
		    pt.put("run.event_log_level", "Transmissions");
			pt.put("run.holidays_file", "calendar_belgium_2020_covid19_exit_school_adjusted.json");
			pt.put("run.start_date", "2020-06-01");
			pt.put("run.detection_probability", 0.5);
			pt.put("run.tracing_efficiency_household", 1.0);
			pt.put("run.tracing_efficiency_other", 0.7);
			pt.put("run.test_false_negative", 0.1);
			pt.put("run.case_finding_capacity", 1000U);

	}
	// change log parameter to use the optimized version
	if (tag == "covid19_tracing_all") {
			pt.put("run.event_log_level", "ContactTracing");
	}

	return make_tuple(pt, target, margin);
}

} // namespace Tests
