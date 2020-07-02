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
 *  Copyright 2020, Willem L
 */

/**
 * @file
 * Header for the EventLogMode class.
 */

#pragma once

#include <string>

namespace stride {
/**
 * Enum specifiying the level of logging required:
 * \li none at all
 * \li only contacts where transmission occurs
 * \li all contacts.
 */
namespace EventLogMode {

enum class Id
{
        None          = 0U,
        Transmissions = 1U,
        All           = 2U
};

/// Converts a LogMode value to corresponding name.
std::string ToString(EventLogMode::Id w);

/// Check whether string is name of LogMode value.
bool IsMode(const std::string& s);

/// Converts a string with name to LogMode value.
EventLogMode::Id ToMode(const std::string& s);

} // namespace EventLogMode
} // namespace stride
