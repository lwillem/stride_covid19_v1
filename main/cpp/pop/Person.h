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

#pragma once

#include "contact/ContactType.h"
#include "contact/IdSubscriptArray.h"
#include "contact/ContactHandler.h"
#include "disease/Health.h"

#include <cstddef>

namespace stride {

/**
 * Store and handle person data.
 */
class Person
{
public:
        /// Default construction (for population vector).
        Person() : m_age(0.0), m_id(0), m_pool_ids(), m_health(), m_in_pools(), m_is_participant(),
		m_able_to_telework(), m_is_tracing_index(false), m_contact_tracing_list() {}

        /// Constructor: set the person data.
        Person(unsigned int id, float age, unsigned int householdId, unsigned int k12SchoolId, unsigned int collegeId,
               unsigned int workId, unsigned int primaryCommunityId, unsigned int secondaryCommunityId, unsigned int householdClusterId)
            : m_age(age), m_id(id), m_pool_ids{householdId, k12SchoolId,        collegeId,
                                               workId,      primaryCommunityId, secondaryCommunityId,
											   householdClusterId},
              m_health(), m_in_pools(true), m_is_participant(false), m_able_to_telework(false),
			  m_is_tracing_index(false), m_contact_tracing_list()
        {
        }

        /// Is this person not equal to the given person?
        bool operator!=(const Person& p) const { return p.m_id != m_id; }

        /// Get the age.
        float GetAge() const { return m_age; }

        /// Return person's health status.
        Health& GetHealth() { return m_health; }

        /// Return person's health status.
        const Health& GetHealth() const { return m_health; }

        /// Get the id.
        unsigned int GetId() const { return m_id; }

        /// Get ID of contactpool_type
        unsigned int GetPoolId(const ContactType::Id& poolType) const { return m_pool_ids[poolType]; }

        /// Check if a person is present today in a given contact pool
        bool IsInPool(const ContactType::Id& poolType) const { return m_in_pools[poolType]; }

        /// Does this person participates in the social contact study?
        bool IsSurveyParticipant() const { return m_is_participant; }

        /// Participate in social contact study and log person details
        void ParticipateInSurvey() { m_is_participant = true; }

        /// Update the health status and presence in contact pools.
        void Update(bool isRegularWeekday, bool isK12SchoolOff, bool isCollegeOff,
        		bool isWorkplaceDistancingEnforced, bool isHouseholdClusteringAllowed, ContactHandler& cHandler);

        /// Set the age of the person
        void SetAge(unsigned int newAge) { m_age = newAge; }

        /// Set the id.
        void SetId(unsigned int id) { m_id = id; }

        /// Sets (for the type of ContactPool) the Id of the ContactPool the person belongs to.
        void SetPoolId(ContactType::Id type, unsigned int poolId)
        {
                m_pool_ids[type] = poolId;
                m_in_pools[type] = (poolId != 0); // Means present in Household, absent elsewhere.
        }

        /// set ability to telework
        void SetTeleworkAbility() { m_able_to_telework = true; }

        /// Is this person able to telework
        bool IsAbleToTelework() const { return m_able_to_telework;}

        /// Set this person as index case for track&trace strategies
        void SetTracingIndexCase(){ m_is_tracing_index = true; }

        /// Set this person as index case for track&trace strategies
        bool IsTracingIndexCase() const { return m_is_tracing_index; }

        /// Register contact, if this person is an index case for track&trace
        void RegisterContact(Person* p) {
        	if(m_is_tracing_index){
        		m_contact_tracing_list.push_back(p);
        	}
        }

        /// Get register with contacts during infected period
        std::vector<Person*>& GetContactRegister () {
        	return m_contact_tracing_list;
        }

private:
        float        m_age; ///< The age.
        unsigned int m_id;  ///< The id.

        ///< Ids (school, work, etc) of pools you belong to Id value 0 means you do not belong to any
        ///< pool of that type (e.g. school and work are mutually exclusive).
        ContactType::IdSubscriptArray<unsigned int> m_pool_ids;

        ///< Health info (immune, infected, etc) for this person.
        Health m_health;

        ///< Is person present/absent in pools of each of the types (school, work, etc)?
        ContactType::IdSubscriptArray<bool> m_in_pools;

        ///< Is this a participant in the social contact study?
        bool m_is_participant;

        ///< Is the participant able to telework?
        bool m_able_to_telework;

        ///< Is this an index case for track,trace, isolate strategies
        bool m_is_tracing_index;

        ///< Vector with the id of contacts during infected period
        std::vector<Person*> m_contact_tracing_list;
};

} // namespace stride
