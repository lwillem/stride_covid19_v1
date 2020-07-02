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
 *  Copyright 2020, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Implementation for the Simulator class.
 */

#include "Sim.h"

#include "calendar/Calendar.h"
#include "contact/ContactType.h"
#include "contact/InfectorExec.h"
#include "disease/DiseaseSeeder.h"
#include "pop/Population.h"
#include "sim/SimBuilder.h"
#include "util/RunConfigManager.h"

#include <omp.h>
#include <utility>

namespace stride {

using namespace std;
using namespace util;
using namespace EventLogMode;

Sim::Sim()
    : m_config(), m_event_log_mode(Id::None), m_num_threads(1U), m_track_index_case(false),
      m_calendar(nullptr), m_contact_profiles(), m_handlers(), m_infector_default(),m_infector_tracing(),
      m_population(nullptr), m_rn_man(), m_transmission_profile(), m_cnt_reduction_workplace(0), m_cnt_reduction_other(0),
	  m_cnt_reduction_workplace_exit(0),m_cnt_reduction_other_exit(0), m_cnt_reduction_school_exit(0), m_cnt_reduction_intergeneration(0),
	  m_cnt_reduction_intergeneration_cutoff(0), m_compliance_delay_workplace(0), m_compliance_delay_other(0),m_cnt_other_exit_delay(0),
	  m_day_of_community_distancing(0), m_day_of_workplace_distancing(0), m_day_of_community_distancing_exit(0),m_cnt_intensity_householdCluster(0),
	  m_public_health_agency(),m_num_daily_imported_cases(0)

{
}

std::shared_ptr<Sim> Sim::Create(const boost::property_tree::ptree& config, shared_ptr<Population> pop,
                                 util::RnMan rnMan)
{
        struct make_shared_enabler : public Sim
        {
                explicit make_shared_enabler() : Sim() {}
        };
        shared_ptr<Sim> sim = make_shared<make_shared_enabler>();
        SimBuilder(config).Build(sim, std::move(pop), std::move(rnMan));
        return sim;
}

void Sim::TimeStep()
{
        // Logic where you compute (on the basis of input/config for initial day or on the basis of
        // number of sick persons, duration of epidemic etc) what kind of DaysOff scheme you apply.
        const bool isRegularWeekday     = m_calendar->IsRegularWeekday();
        const bool isPreSchoolOff       = m_calendar->IsPreSchoolOff();
        const bool isPrimarySchoolOff   = m_calendar->IsPrimarySchoolOff();
        const bool isSecondarySchoolOff = m_calendar->IsSecondarySchoolOff();
        const bool isCollegeOff         = m_calendar->IsCollegeOff();
        const bool isWorkplaceDistancingEnforced   = m_calendar->IsWorkplaceDistancingEnforced();
        const bool isCommunityDistancingEnforced   = m_calendar->IsCommunityDistancingEnforced();
        const bool isHouseholdClusteringAllowed    = m_calendar->IsHouseholdClusteringAllowed();

        // skip all K12 schools?
        bool areAllK12SchoolsOff = (isPreSchoolOff && isPrimarySchoolOff && isSecondarySchoolOff);

        // increment the number of days in lock-down and account for compliance
		double workplace_distancing_factor = 0.0;
		if(isWorkplaceDistancingEnforced){
			m_day_of_workplace_distancing += 1;

			workplace_distancing_factor = m_cnt_reduction_workplace;

			if(m_day_of_workplace_distancing < m_compliance_delay_workplace){
				workplace_distancing_factor *= 1.0 * m_day_of_workplace_distancing / m_compliance_delay_workplace;
			}
		} else if(m_day_of_workplace_distancing > 0){
			workplace_distancing_factor = m_cnt_reduction_workplace_exit;
		}

		 // increment the number of days in lock-down and account for compliance
		double community_distancing_factor = 0.0;
		double intergeneration_distancing_factor = 0.0;
		if(isCommunityDistancingEnforced){
			m_day_of_community_distancing += 1;

			community_distancing_factor = m_cnt_reduction_other;
			intergeneration_distancing_factor = m_cnt_reduction_intergeneration;

			if(m_day_of_community_distancing < m_compliance_delay_other){
				community_distancing_factor *= 1.0 * m_day_of_community_distancing / m_compliance_delay_other;
			}
		} else if (m_day_of_community_distancing > 0){

			m_day_of_community_distancing_exit += 1;

			if(m_day_of_community_distancing_exit < m_cnt_other_exit_delay){
				community_distancing_factor       = m_cnt_reduction_other;
			} else{
				community_distancing_factor       = m_cnt_reduction_other_exit;
			}
			intergeneration_distancing_factor = m_cnt_reduction_intergeneration;
		}

		// get distancing at school
		double school_distancing_factor = (m_day_of_workplace_distancing > 0) ? m_cnt_reduction_school_exit : 0 ;

		// To be used in update of population & contact pools.
        Population& population    = *m_population;
        auto&       poolSys       = population.RefPoolSys();
        auto        eventLogger   = population.RefEventLogger();
        const auto  simDay        = m_calendar->GetSimulationDay();

        // Select infector, based on tracing
        const auto& infector      = m_public_health_agency.IsContactTracingActive(m_calendar) ? *m_infector_tracing : *m_infector_default;

        // set HouseholdCluster intensity
        double cnt_intensity_householdCluster = 0.0;
		if (isHouseholdClusteringAllowed && poolSys.RefPools(ContactType::Id::HouseholdCluster).size() > 1){
			cnt_intensity_householdCluster = m_cnt_intensity_householdCluster;
		}

        // Import infected cases into the population
        if(m_num_daily_imported_cases > 0){
        	DiseaseSeeder(m_config, m_rn_man).ImportInfectedCases(m_population, m_num_daily_imported_cases, simDay);
        }

#pragma omp parallel num_threads(m_num_threads)
        {
        	const auto thread_num = static_cast<unsigned int>(omp_get_thread_num());
			// Update health status and presence/absence in contact pools
			// depending on health status, work/school day and whether
			// we want to track index cases without adaptive behavior
#pragma omp for schedule(static)
			for (size_t i = 0; i < population.size(); ++i) {

				// adjust K12SchoolOff boolean to school type for individual 'i'
				bool isK12SchoolOff = m_public_health_agency.IsK12SchoolOff(population[i].GetAge(),isPreSchoolOff,isPrimarySchoolOff,
						isSecondarySchoolOff, isCollegeOff);

				// update health and presence at diffent contact pools
				population[i].Update(isRegularWeekday, isK12SchoolOff, isCollegeOff,
						isWorkplaceDistancingEnforced, isHouseholdClusteringAllowed,
						m_handlers[thread_num]);
			}
        }// end pragma openMP

		 // Perform contact tracing (if activated)
		 m_public_health_agency.PerformContactTracing(m_population, m_handlers[0], m_calendar);

#pragma omp parallel num_threads(m_num_threads)
        {
		    const auto thread_num = static_cast<unsigned int>(omp_get_thread_num());
			// Infector updates individuals for contacts & transmission within each pool.
			// Skip pools with id = 0, because it means Not Applicable.
			for (auto typ : ContactType::IdList) {
					if ((typ == ContactType::Id::Workplace && !isRegularWeekday) ||
						(typ == ContactType::Id::K12School && areAllK12SchoolsOff) ||
						(typ == ContactType::Id::College && isCollegeOff) ||
						(typ == ContactType::Id::HouseholdCluster && !isHouseholdClusteringAllowed)) {
							continue;
					}
#pragma omp for schedule(static)
					for (size_t i = 1; i < poolSys.RefPools(typ).size(); i++) { // NOLINT
							infector(poolSys.RefPools(typ)[i], m_contact_profiles[typ], m_transmission_profile,
									 m_handlers[thread_num], simDay, eventLogger,
									 workplace_distancing_factor,
									 community_distancing_factor,
									 school_distancing_factor,
									 intergeneration_distancing_factor,m_cnt_reduction_intergeneration_cutoff,
									 m_population,cnt_intensity_householdCluster);
					}
			}
        } // end pragma openMP

        m_population->RefEventLogger()->flush();
        m_calendar->AdvanceDay();
}

} // namespace stride
