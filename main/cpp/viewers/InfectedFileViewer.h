#pragma once
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
 *  Copyright 2018, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Observer for Infected output.
 */

#include "InfectedFile.h"
#include "sim/event/Id.h"

#include <iostream>
#include <spdlog/spdlog.h>

namespace stride {

class SimRunner;

namespace viewers {

/// Viewer of Simulator for cases output.
class InfectedFileViewer
{
public:
        /// Instantiate cases viewer.
        InfectedFileViewer(std::shared_ptr<SimRunner> runner, const std::string& output_prefix);

        /// Let viewer perform update.
        void Update(sim_event::Id id);

private:
        const std::string&         m_output_prefix;
        std::vector<unsigned int>  m_infected;
        std::vector<unsigned int>  m_exposed;
        std::vector<unsigned int>  m_infectious;
        std::vector<unsigned int>  m_symptomatic;
        std::vector<unsigned int>  m_infected_total;

        std::shared_ptr<SimRunner> m_runner;
};

} // namespace viewers
} // namespace stride
