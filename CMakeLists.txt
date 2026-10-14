cmake_minimum_required(VERSION 3.14)

# This library has been superseded by quanta. Consumers are warned when it is configured
# as a subproject or an ESP-IDF component (below) or found as an installed package
# (cmake/frequencyConfig.cmake.in), but not when this is the top-level project.
set(FREQUENCY_DEPRECATION_MESSAGE "frequency-cpp is no longer maintained and has been superseded by quanta (https://github.com/cleishm/quanta-cpp), which includes it as quanta::frequency; on ESP-IDF use the component cleishm/quanta. See https://github.com/cleishm/quanta-cpp#migrating-from-thermo-cpp-frequency-cpp-electro-cpp-and-spatial-cpp")

# ESP-IDF component support (must be before project() for script mode compatibility)
if(ESP_PLATFORM)
    # IDF also evaluates this file in script mode to expand component requirements
    # (CMAKE_BUILD_EARLY_EXPANSION); warn only during the real configure.
    if(NOT CMAKE_BUILD_EARLY_EXPANSION)
        message(DEPRECATION "${FREQUENCY_DEPRECATION_MESSAGE}")
    endif()
    idf_component_register(INCLUDE_DIRS "include")
    return()
endif()

project(frequency
    VERSION 1.3.2
    DESCRIPTION "Type-safe frequency handling library modeled after std::chrono"
    HOMEPAGE_URL "https://github.com/cleishm/frequency-cpp"
    LANGUAGES CXX
)

# Not PROJECT_IS_TOP_LEVEL, which needs CMake 3.21.
if(NOT CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    message(DEPRECATION "${FREQUENCY_DEPRECATION_MESSAGE}")
endif()

option(FREQUENCY_BUILD_TESTS "Build tests" ${frequency_IS_TOP_LEVEL})

set(CMAKE_CXX_EXTENSIONS OFF)

# Main library target (header-only)
add_library(frequency INTERFACE)
add_library(frequency::frequency ALIAS frequency)

# The headers contain UTF-8 unit symbols; MSVC needs /utf-8 to read them correctly.
target_compile_options(frequency INTERFACE "$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

target_include_directories(frequency INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

target_compile_features(frequency INTERFACE cxx_std_20)

# Formatting (only when this is the top-level project)
if(frequency_IS_TOP_LEVEL)
    find_program(CLANG_FORMAT_EXECUTABLE NAMES clang-format)

    if(CLANG_FORMAT_EXECUTABLE)
        # Get all source files
        file(GLOB_RECURSE ALL_SOURCE_FILES
            ${CMAKE_CURRENT_SOURCE_DIR}/include/*.h
            ${CMAKE_CURRENT_SOURCE_DIR}/include/*.hpp
            ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.cpp
            ${CMAKE_CURRENT_SOURCE_DIR}/tests/*.h
        )

        # Target to check formatting
        add_custom_target(format-check
            COMMAND ${CLANG_FORMAT_EXECUTABLE} --dry-run --Werror ${ALL_SOURCE_FILES}
            COMMENT "Checking code formatting"
            VERBATIM
        )

        # Target to fix formatting
        add_custom_target(format
            COMMAND ${CLANG_FORMAT_EXECUTABLE} -i ${ALL_SOURCE_FILES}
            COMMENT "Applying code formatting"
            VERBATIM
        )

        message(STATUS "clang-format found: format targets available")
    else()
        message(WARNING "clang-format not found: format targets unavailable")
    endif()
endif()

# Tests
if(FREQUENCY_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Documentation
option(FREQUENCY_BUILD_DOCS "Build documentation" OFF)

if(FREQUENCY_BUILD_DOCS)
    find_package(Doxygen)
    if(DOXYGEN_FOUND)
        set(DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/docs)
        add_custom_target(docs
            COMMAND ${CMAKE_COMMAND} -E make_directory ${DOXYGEN_OUTPUT_DIRECTORY}
            COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/docs/Doxyfile
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/docs
            COMMENT "Generating API documentation with Doxygen"
            VERBATIM
        )
        message(STATUS "Doxygen found: documentation can be built with 'make docs'")
    else()
        message(WARNING "Doxygen not found: documentation cannot be built")
    endif()
endif()

# Installation
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(TARGETS frequency
    EXPORT frequencyTargets
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(DIRECTORY include/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(EXPORT frequencyTargets
    FILE frequencyTargets.cmake
    NAMESPACE frequency::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/frequency
)

configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/frequencyConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/frequencyConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/frequency
)

write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/frequencyConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/frequencyConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/frequencyConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/frequency
)
