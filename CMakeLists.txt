cmake_minimum_required(VERSION 3.14)

# ESP-IDF component support (must be before project() for script mode compatibility)
if(ESP_PLATFORM)
    idf_component_register(INCLUDE_DIRS "include")
    return()
endif()

project(frequency
    VERSION 1.1.2
    DESCRIPTION "Type-safe frequency handling library modeled after std::chrono"
    HOMEPAGE_URL "https://github.com/cleishm/frequency-cpp"
    LANGUAGES CXX
)

option(FREQUENCY_BUILD_TESTS "Build tests" ${frequency_IS_TOP_LEVEL})

set(CMAKE_CXX_EXTENSIONS OFF)

# Main library target (header-only)
add_library(frequency INTERFACE)
add_library(frequency::frequency ALIAS frequency)

target_include_directories(frequency INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
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
