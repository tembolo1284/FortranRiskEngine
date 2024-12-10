# Minimum required version of CMake
cmake_minimum_required(VERSION 3.15)

# Project details
project(FortranRiskEngine
    VERSION 1.0
    DESCRIPTION "A high-performance Monte Carlo simulation engine for quantitative finance using Fortran"
    LANGUAGES Fortran)

# Set build type to Release by default for performance
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Set GFortran as the default compiler
set(CMAKE_Fortran_COMPILER gfortran)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/include)

# Find and enable OpenMP for parallelism
find_package(OpenMP REQUIRED)

# Compiler options
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fopenmp -O3")
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -pedantic")
endif()

# Source files
set(SRC_FILES
    ${CMAKE_SOURCE_DIR}/src/main.f90
    ${CMAKE_SOURCE_DIR}/src/monte_carlo.f90
    ${CMAKE_SOURCE_DIR}/src/random.f90
    ${CMAKE_SOURCE_DIR}/src/payoff.f90
    ${CMAKE_SOURCE_DIR}/src/models.f90
)

# Add executable for main program
add_executable(monte_carlo_simulation ${SRC_FILES})

# Link OpenMP for parallel execution
target_link_libraries(monte_carlo_simulation PRIVATE OpenMP::OpenMP_Fortran)

# Installation rules
install(TARGETS monte_carlo_simulation DESTINATION bin)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/data DESTINATION share/FortranRiskEngine)

