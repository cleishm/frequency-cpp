// Regression test: the std::formatter specialization must be usable when
// compiled with exceptions disabled (-fno-exceptions), as on embedded targets
// such as ESP-IDF with CONFIG_COMPILER_CXX_EXCEPTIONS unset.
//
// This is a separate target from frequency_tests because Catch2 requires
// exceptions. It deliberately avoids any test framework so the whole
// translation unit builds without them.

#include <cstdio>
#include <frequency/frequency>
#include <string>

#ifdef __cpp_exceptions
#error "this test must be compiled with exceptions disabled"
#endif

using namespace freq;

namespace {

int failures = 0;

void check(const std::string& actual, const std::string& expected, const char* what) {
    if (actual != expected) {
        std::printf("FAIL: %s: expected \"%s\", got \"%s\"\n", what, expected.c_str(), actual.c_str());
        ++failures;
    }
}

} // namespace

int main() {
    // Formatting with no spec: instantiating parse() must not pull in a throw.
    check(std::format("{}", hertz(50)), "50Hz", "{} hertz");
    check(std::format("{}", millihertz(1500)), "1500mHz", "{} millihertz");
    check(std::format("{}", kilohertz(433)), "433kHz", "{} kilohertz");
    check(std::format("{}", megahertz(868)), "868MHz", "{} megahertz");

    // Formatting with an explicit spec.
    check(std::format("{:.1f}", millihertz(1500)), "1.5Hz", "{:.1f} millihertz");
    check(std::format("{:.1f}", kilohertz(433)), "433000.0Hz", "{:.1f} kilohertz");

    // A precision with no SI prefix is still usable with an explicit spec.
    using decihertz = frequency<int64_t, std::deci>;
    check(std::format("{:.1f}", decihertz(225)), "22.5Hz", "{:.1f} decihertz");

    if (failures != 0) {
        std::printf("%d check(s) failed\n", failures);
        return 1;
    }
    std::printf("all checks passed\n");
    return 0;
}
