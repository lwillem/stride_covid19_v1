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
 *  Copyright 2017, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Header file for the core Population class
 */

#pragma once

#include "contact/ContactPool.h"
#include "contact/ContactPoolSys.h"
#include "contact/ContactType.h"
#include "pop/Person.h"
#include "util/RnMan.h"
#include "util/SegmentedVector.h"

#include <boost/property_tree/ptree_fwd.hpp>
#include <memory>
#include <spdlog/spdlog.h>

namespace stride {

namespace util {
class RnMan;
}

/**
 * Key Data structure: container for
 * (a) all individuals in the population
 * (b) the ContactPoolSys which is used to loop over ContactPools of each type
 */
class Population : public util::SegmentedVector<Person, 2048>
{
public:
        /// Create a Population initialized by the configuration in property tree.
        static std::shared_ptr<Population> Create(const boost::property_tree::ptree& config,
                                                  std::shared_ptr<spdlog::logger> strideLogger = nullptr);

        /// Create an empty Population, used in gengeopop.
        static std::shared_ptr<Population> Create();

public:
        /// Create Person in the population.
        Person* CreatePerson(unsigned int id, double age, unsigned int householdId, unsigned int k12SchoolId,
                             unsigned int collegeId, unsigned int workId, unsigned int primaryCommunityId,
                             unsigned int secondaryCommunityId, unsigned int householdClusterId);

        /// Get the cumulative number of cases.
        unsigned int GetTotalInfected() const;

        /// Get the current number of infected cases.
        unsigned int CountInfectedCases() const;

        /// Get the current number of exposed cases.
        unsigned int CountExposedCases() const;

        /// Get the current number of infectious cases.
        unsigned int CountInfectiousCases() const;

        /// Get the current number of symptomatic cases.
        unsigned int CountSymptomaticCases() const;

        /// Get the maximum age in the population.
        unsigned int GetMaxAge() const;

        /// The ContactPoolSys of the simulator.
        const ContactPoolSys& CRefPoolSys() const { return m_pool_sys; }

        /// Return the InfectorLogger.
        std::shared_ptr<spdlog::logger>& RefEventLogger() { return m_event_logger; }

        /// Reference the ContactPoolSys of the Population.
        ContactPoolSys& RefPoolSys() { return m_pool_sys; }

        /// Get the ContactPool size of a given type and id
        unsigned int GetPoolSize(ContactType::Id typeId, const Person* p) const;


private:
        /// Non-trivial default constructor.
        Population();

private:
        ContactPoolSys                  m_pool_sys;       ///< The global @ContactPoolSys.
        std::shared_ptr<spdlog::logger> m_event_logger; ///< Logger for contact/transmission/tracing/...
};

} // namespace stride
