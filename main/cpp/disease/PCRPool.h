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
 *  Copyright 2020, Libin P, Willem L
 */

/**
 * @file
 * Header for the PCRPool class.
 */

#include <vector>
#include <string>

namespace stride {

class Person;

/// A PCR pool consists out of a number of individuals that is to be tested, simultaneously, in one PCR test, i.e., by pooling the samples.
class PCRPool 
{
public:
        PCRPool():m_individuals(),
                  m_households(), 
                  m_georegion(), 
                  m_id() {}
       
        void SetGeoRegion(const std::string &georegion) { m_georegion = georegion; }
        std::string GetGeoRegion() const { return m_georegion; }
        void SetId(unsigned int id) { m_id = id; }
        unsigned int GetId() const { return m_id; }

        void AddHousehold(const std::vector<Person*>& household) { 
            m_households.push_back(household);
            for (const auto& hh_member : household) {
                m_individuals.push_back(hh_member);
            }
        }

        const std::vector<Person*>& GetIndividuals() const { return m_individuals; } 
        const std::vector<std::vector<Person*>>& GetHouseholds() const { return m_households; } 

        bool operator< (const PCRPool& p) const {
            return m_individuals < p.m_individuals;
        }
private:
        std::vector<Person*> m_individuals;
        std::vector<std::vector<Person*>> m_households;
        std::string m_georegion;
        unsigned int m_id;
};

} // namespace stride
