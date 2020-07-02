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
 *  Copyright 2019, Willem L, Kuylen E, Broeckhove J
 */

/**
 * @file
 * Header for the ContactHandler class.
 */

#pragma once

#include <cmath>
#include <functional>

namespace stride {

/**
 * Processes the contacts between persons and determines whether transmission occurs.
 */
class ContactHandler
{
public:
        /// Constructor sets the transmission probability and random number generator.
        explicit ContactHandler(std::function<double()> gen) : m_uniform01_generator(std::move(gen)) {}

        /// Make a draw on the uniform generator.
        double operator()() { return m_uniform01_generator(); }

        /// Check if two individuals have contact and transmission occurs.
        bool HasContactAndTransmission(double contact_probability, double transmission_probability)
        {
                return m_uniform01_generator() < transmission_probability * contact_probability;
        }

        /// Check if two individuals have contact.
        bool HasContact(double contact_probability) { return m_uniform01_generator() < contact_probability; }

        /// Check whether transmission occurs.
        bool HasTransmission(double transmission_probability)
        {
                return m_uniform01_generator() < transmission_probability;
        }

private:
        /// Convert rate into probability
        double RateToProbability(double rate) { return 1.0 - std::exp(-rate); }

private:
        std::function<double()> m_uniform01_generator; ///< Random number generator: double in [0.0, 1.0)
};

} // namespace stride
