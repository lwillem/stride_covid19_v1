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
 * Implementation of the CasesFile class.
 */

#include "InfectedFile.h"
#include "util/FileSys.h"

namespace stride {
namespace output {

using namespace std;
using namespace stride::util;

InfectedFile::InfectedFile(const std::string& output_prefix, const std::string& file_name) : m_fstream()
{
        const auto p = FileSys::BuildPath(output_prefix, file_name + ".csv");
        m_fstream.open(p.c_str());
}

InfectedFile::~InfectedFile() { m_fstream.close(); }

void InfectedFile::Print(const vector<unsigned int>& infectionCounts)
{
        for (unsigned int i = 0; i < (infectionCounts.size() - 1); i++) {
                m_fstream << infectionCounts[i] << ",";
        }
        m_fstream << infectionCounts[infectionCounts.size() - 1] << endl;
}

} // namespace output
} // namespace stride
