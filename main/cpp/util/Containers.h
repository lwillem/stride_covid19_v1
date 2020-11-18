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

#ifndef CONTAINERS_H 
#define CONTAINERS_H 

#include <vector>
#include <map>
#include <math.h>

namespace stride {
namespace util {

template <typename K, typename V>
std::vector<V> MapValuesToVec(const std::map<K,V> &m)
{
    std::vector<V> values;
    for(const auto& key_val : m) {
        values.push_back(key_val.second);
    }
    return values;
}

template <typename T>
std::vector<std::vector<T>> SplitVec(std::vector<T> v, size_t chunk_size)
{
    std::vector<std::vector<T>> chunks;
    size_t n_chunks = ceil(v.size()/(float)chunk_size);

    for (unsigned int i = 0; i < n_chunks; ++i) {
        size_t from = (i*chunk_size);
        size_t to;
        if (from + chunk_size > v.size()) {
            to = v.size();
        } else {
            to = from + chunk_size;
        }
        auto chunk = std::vector<T>(v.begin() + from, v.begin() + to);
        chunks.push_back(chunk);
    }

    return chunks;
}


} // namespace util
} // namespace stride

#endif //CONTAINERS_H 
