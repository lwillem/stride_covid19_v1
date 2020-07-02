#############################################################################
#  This file is part of the Stride software.
#  It is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or any
#  later version.
#  The software is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  You should have received a copy of the GNU General Public License,
#  along with the software. If not, see <http://www.gnu.org/licenses/>.
#  see http://www.gnu.org/licenses/.
#
#  Copyright 2019, Willem L, Kuylen E & Broeckhove J
#############################################################################
#
#  Report config info.
#
#############################################################################

#============================================================================
# Overview report:
#============================================================================
message( STATUS " \n " )
if ( GIT_FOUND )
	message( STATUS "------> Git revision id: ${STRIDE_WC_REVISION_HASH} "       )
	message( STATUS "------> Git commit date: ${STRIDE_WC_LAST_CHANGED_DATE} "   )
endif()
message( STATUS " " )
message( STATUS "------> STRIDE_INCLUDE_DOC          : ${STRIDE_INCLUDE_DOC} "      )
message( STATUS "------> STRIDE_FORCE_NO_OPENMP      : ${STRIDE_FORCE_NO_OPENMP}"   )
message( STATUS "------> STRIDE_BUILD_TEST_CASES     : ${STRIDE_BUILD_TEST_CASES}"   )

#
message( STATUS " " )
message( STATUS "------> CMAKE_SYSTEM                : ${CMAKE_SYSTEM} "           )
message( STATUS "------> CMAKE_SYSTEM_VERSION        : ${CMAKE_SYSTEM_VERSION} "   )
message( STATUS "------> CMAKE_SYSTEM_PROCESSOR      : ${CMAKE_SYSTEM_PROCESSOR} " )
message( STATUS "------> PROCESSOR_COUNT             : ${PROCCOUNT} "              )
#
message( STATUS " " )
message( STATUS "------> CMAKE_VERSION               : ${CMAKE_VERSION} "          )
message( STATUS "------> CMAKE_PREFIX_PATH           : ${CMAKE_PREFIX_PATH} "      )
message( STATUS "------> ENVIRONMENT PATH            : $ENV{PATH} "                )
#
message( STATUS " " )
message( STATUS "------> CMAKE_BUILD_TYPE            : ${CMAKE_BUILD_TYPE} "          )
message( STATUS "------> CMAKE_BINARY_DIR            : ${CMAKE_BINARY_DIR} "          )
message( STATUS "------> CMAKE_INSTALL_PREFIX        : ${CMAKE_INSTALL_PREFIX}"       )
message( STATUS "------> CMAKE_CXX_COMPILER          : ${CMAKE_CXX_COMPILER}"         )
message( STATUS "------> CMAKE_CXX_COMPILER_ID       : ${CMAKE_CXX_COMPILER_ID}"      )
message( STATUS "------> CMAKE_CXX_COMPILER_VERSION  : ${CMAKE_CXX_COMPILER_VERSION}" )
message( STATUS "------> CMAKE_CXX_FLAGS             : ${CMAKE_CXX_FLAGS}"            )
if( CMAKE_BUILD_TYPE MATCHES "^[Rr]elease$" )
	message( STATUS "------> CMAKE_CXX_FLAGS_RELEASE     : ${CMAKE_CXX_FLAGS_RELEASE}")
endif()
if( CMAKE_BUILD_TYPE MATCHES "^[Dd]ebug$" )
	message( STATUS "------> CMAKE_CXX_FLAGS_DEBUG       : ${CMAKE_CXX_FLAGS_DEBUG}"  )
endif()
#
message( STATUS "" )
if ( Boost_FOUND )
	message( STATUS "------> Boost_FOUND                 : ${Boost_FOUND} "               )
	message( STATUS "------> Boost_VERSION               : ${Boost_VERSION} "             )
	message( STATUS "------> Boost_INCLUDE_DIRS          : ${Boost_INCLUDE_DIRS} "        )
	message( STATUS "------> Boost_Libraries             : ${Boost_LIBRARIES} "           )
else()
	message( STATUS "------> Boost_VERSION               : using included Boost source 1.69"    )
endif()
#
if( NOT STRIDE_FORCE_NO_OPENMP )
    message( STATUS " " )
	message( STATUS "------> HAVE_CHECKED_OpenMP         : ${HAVE_CHECKED_OpenMP}"     )
	message( STATUS "------> HAVE_FOUND_OpenMP           : ${HAVE_FOUND_OpenMP}"       )
	message( STATUS "------> OpenMP_CXX_FLAGS            : ${OpenMP_CXX_FLAGS}"        )
	message( STATUS "------> OpenMP_CXX_LIBRARIES        : ${OpenMP_CXX_LIBRARIES}"    )
endif()
#
message( STATUS "" )
if ( STRIDE_INCLUDE_DOC )
	message( STATUS "------> DOXYGEN_FOUND               : ${DOXYGEN_FOUND} "          )
	message( STATUS "------> DOXYGEN_DOT_FOUND           : ${DOXYGEN_DOT_FOUND} "      )
	if ( DOXYGEN_FOUND )
		message( STATUS "------> DOXYGEN_EXECUTABLE          : ${DOXYGEN_EXECUTABLE} " )
		message( STATUS "------> DOXYGEN_VERSION             : ${DOXYGEN_VERSION} "    )
	endif()
	if ( DOXYGEN_DOT_FOUND )
		message( STATUS "------> DOXYGEN_DOT_EXECUTABLE      : ${DOXYGEN_DOT_EXECUTABLE} " )
	endif()
endif()
message( STATUS "" )
message( STATUS "Report complete." )

#############################################################################
