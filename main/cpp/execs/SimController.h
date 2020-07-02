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
 *  Copyright 2017, 2018, Kuylen E, Willem L, Broeckhove J
 */

/**
 * @file
 * Header for the command line controller.
 */

#pragma once

#include "ControlHelper.h"

#include <boost/property_tree/ptree_fwd.hpp>
#include <memory>

namespace stride {

class Sim;

/**
 * Controls a simulation run initiated with the command line interface (cli).
 *
 * SimController setup functions include (@see ControlHelper):
 * \li checks the OpenMP environment
 * \li checks the file system environment
 * \li interprets and executes the output prefix
 * \li installs a stride logger
 *
 * The SimController execution:
 * \li creates a population (@see Population)
 * \li creates a simulation runner (@see SimRunner)
 * \li registers the appropriate viewers
 * \li runs the simulation
 */
class SimController : protected ControlHelper
{
public:
        /// Straight initialization.
        explicit SimController(const boost::property_tree::ptree& config, const std::string& name = "SimController");

        /// Control the execution of the simulation.
        void Control();

        /// Reference the simulator (method used mostly in tests).
        std::shared_ptr<Sim> GetSim() const { return m_simulator; };

private:
        std::shared_ptr<Sim> m_simulator;
};

} // namespace stride
