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
 * Header for the Simulator class.
 */

#pragma once

#include "contact/AgeContactProfiles.h"
#include "contact/ContactHandler.h"
#include "contact/InfectorExec.h"
#include "contact/TransmissionProfile.h"
#include "disease/PublicHealthAgency.h"

#include "util/RnMan.h"


#include <boost/property_tree/ptree.hpp>
#include <string>
#include "../contact/EventLogMode.h"

namespace stride {

class Calendar;
class Population;

namespace util {
class RnMan;
}

/**
 * Simulator can time step and reveal some of the key data.
 * The Subject base class used for the interaction with the python environment only.
 */
class Sim
{
public:
        /// Create Sim initialized by the configuration in property tree and population.
        static std::shared_ptr<Sim> Create(const boost::property_tree::ptree& config, std::shared_ptr<Population> pop,
                                           util::RnMan rnMan);

        /// Calendar for the simulated world. Initialized with the start date in the simulation
        /// world. Use GetCalendar()->GetSimulationDay() for the number of days simulated.
        std::shared_ptr<Calendar> GetCalendar() const { return m_calendar; }

        /// Get the Sim configuration for the given attribute.
        std::string GetConfigValue(const std::string& attribute) const { return m_config.get<std::string>(attribute); }

        /// Get the population.
        std::shared_ptr<Population> GetPopulation() { return m_population; }

        /// Get the stored transmission probability.
        double GetTransmissionProbability() const { return m_transmission_profile.GetProbability(); }

        /// Get the random number manager.
        util::RnMan& RefRnManager() { return m_rn_man; }

        /// Get the transmission profile.
        const TransmissionProfile& RefTransmissionProfile() const { return m_transmission_profile; }

        /// Run one time step, computing full simulation (default) or only index case.
        void TimeStep();

private:
        /// Constructor for empty Simulator.
        explicit Sim();

        friend class SimBuilder;

private:
        boost::property_tree::ptree m_config;                        ///< Configuration property tree
        EventLogMode::Id            m_event_log_mode;                ///< Specifies contact/transmission logging mode.
        unsigned int                m_num_threads;                   ///< The number of (OpenMP) threads.
        bool                        m_track_index_case;              ///< General simulation or tracking index case.

        std::shared_ptr<Calendar>   m_calendar;         ///< Management of calendar.
        AgeContactProfiles          m_contact_profiles; ///< Contact profiles w.r.t age.
        std::vector<ContactHandler> m_handlers;         ///< Contact handlers (random numbers & probabilities).
        InfectorExec*               m_infector_default; ///< Executes optimized transmission loops in contact pools.
        InfectorExec*               m_infector_tracing; ///< Executes all or optimized transmission loops in contact pools.
        std::shared_ptr<Population> m_population;       ///< Pointer to the Population.
        util::RnMan                 m_rn_man;           ///< Random number generation management.

        TransmissionProfile         m_transmission_profile; ///< Profile of disease.

        // temporary...
        double                      m_cnt_reduction_workplace;
        double                      m_cnt_reduction_other;
        double                      m_cnt_reduction_workplace_exit;
        double                      m_cnt_reduction_other_exit;
        double                      m_cnt_reduction_school_exit;
        double                      m_cnt_reduction_intergeneration;
        unsigned int                m_cnt_reduction_intergeneration_cutoff;
        unsigned int                m_compliance_delay_workplace;
        unsigned int                m_compliance_delay_other;
        unsigned int                m_cnt_other_exit_delay;
        unsigned int                m_day_of_community_distancing;
        unsigned int     	        m_day_of_workplace_distancing;
        unsigned int     			m_day_of_community_distancing_exit;
        double                      m_cnt_intensity_householdCluster;

        PublicHealthAgency          m_public_health_agency;

        // Introduce new infected cases on a daily basis?
        unsigned int                m_num_daily_imported_cases;


};

} // namespace stride
