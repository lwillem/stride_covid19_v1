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
 * Header file for the Person class.
 */

#include "Person.h"

#include "contact/ContactType.h"
#include "pop/Age.h"

namespace stride {

using namespace std;
using namespace stride::ContactType;

void Person::Update(bool isRegularWeekday, bool isK12SchoolOff, bool isCollegeOff,
		bool isTeleworkEnforced, bool isHouseholdClusteringAllowed,
		ContactHandler& cHandler)

{
        // Update health and disease status
        m_health.Update();

        // by default: a person is at home
        m_in_pools[Id::Household]          = true;

        // is household clustering allowed?
        m_in_pools[Id::HouseholdCluster]   = isHouseholdClusteringAllowed ? true : false;

        // Update presence in contact pools by type of day
        if (isRegularWeekday) {
        	m_in_pools[Id::Workplace]          = true;
			m_in_pools[Id::PrimaryCommunity]   = false;
			m_in_pools[Id::SecondaryCommunity] = true;
        } else{
            m_in_pools[Id::Workplace]          = false;
            m_in_pools[Id::PrimaryCommunity]   = true;
            m_in_pools[Id::SecondaryCommunity] = false;
        }

        // Update presence at school and college
        m_in_pools[Id::K12School] = isK12SchoolOff ? false : true;
        m_in_pools[Id::College]   = isCollegeOff   ? false : true;


        // Update presence in contact pools by health state
        if (m_health.IsSymptomatic()) {

        	// probability of staying home from school/work given symptoms
        	if(cHandler() < m_health.GetSymptomaticCntReductionWorkSchool()){
        		m_in_pools[Id::K12School]          = false;
				m_in_pools[Id::College]            = false;
				m_in_pools[Id::Workplace]          = false;
        	}

            // probability of staying home from community pools given symptoms
        	if(cHandler() < m_health.GetSymptomaticCntReductionCommunity()){
				m_in_pools[Id::PrimaryCommunity]   = false;
				m_in_pools[Id::SecondaryCommunity] = false;
        	}

        	// stay home from household cluster when symptomatic
			m_in_pools[Id::HouseholdCluster]   = false;

        }

        if(isTeleworkEnforced && IsAbleToTelework()){
			m_in_pools[Id::Workplace]          = false;
		}

        // Update presence in contact pools if person is in quarantine
        if(m_health.IsInIsolation()){
        	m_in_pools[Id::Household]          = false;  //TODO: no household transmission in quarantine?
        	m_in_pools[Id::K12School]          = false;
			m_in_pools[Id::College]            = false;
			m_in_pools[Id::Workplace]          = false;
        	m_in_pools[Id::PrimaryCommunity]   = false;
        	m_in_pools[Id::SecondaryCommunity] = false;
        	m_in_pools[Id::HouseholdCluster]   = false;
        }

} // Person::Update()

} // namespace stride
