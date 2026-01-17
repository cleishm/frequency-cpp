// Unit tests for frequency library
// Uses Catch2 test framework with compile-time static_asserts

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <frequency/frequency>

using namespace freq;
using namespace frequency_literals;

// =============================================================================
// Compile-time tests (static_assert)
// These verify correctness at compile time - if this file compiles, they pass.
// =============================================================================

// Frequency construction and arithmetic
static_assert(hertz(10).count() == 10);
static_assert(hertz(5) + hertz(3) == hertz(8));
static_assert(hertz(5) - hertz(3) == hertz(2));
static_assert(hertz(5) * 2 == hertz(10));
static_assert(hertz(10) / 2 == hertz(5));

// Frequency precision conversions
static_assert(hertz(1) == millihertz(1000));
static_assert(kilohertz(1) == hertz(1000));
static_assert(megahertz(1) == kilohertz(1000));
static_assert(gigahertz(1) == megahertz(1000));
static_assert(terahertz(1) == gigahertz(1000));

// Construction and count
static_assert(millihertz(1000).count() == 1000);
static_assert(hertz(1000).count() == 1000);
static_assert(kilohertz(80).count() == 80);
static_assert(megahertz(80).count() == 80);
static_assert(gigahertz(2).count() == 2);
static_assert(terahertz(1).count() == 1);

// Precision conversions (implicit lossless)
constexpr millihertz mhz_from_hz = hertz(1);
static_assert(mhz_from_hz.count() == 1000);
constexpr hertz hz_from_khz = kilohertz(1);
static_assert(hz_from_khz.count() == 1000);
constexpr kilohertz khz_from_mhz = megahertz(1);
static_assert(khz_from_mhz.count() == 1000);
constexpr megahertz mhz_from_ghz = gigahertz(1);
static_assert(mhz_from_ghz.count() == 1000);
constexpr gigahertz ghz_from_thz = terahertz(1);
static_assert(ghz_from_thz.count() == 1000);

// Explicit lossy conversions
static_assert(hertz(millihertz(1500)).count() == 1);
static_assert(kilohertz(hertz(1500)).count() == 1);

// Comparison operators
static_assert(hertz(1000) == hertz(1000));
static_assert(hertz(1000) != hertz(1001));
static_assert(hertz(1000) < hertz(1001));
static_assert(hertz(1001) > hertz(1000));
static_assert(hertz(1000) <= hertz(1001));
static_assert(hertz(1001) >= hertz(1000));
static_assert(millihertz(hertz(1)) == millihertz(1000));

// Literals
static_assert((1000_mHz).count() == 1000);
static_assert((1000_Hz).count() == 1000);
static_assert((80_kHz).count() == 80);
static_assert((80_MHz).count() == 80);
static_assert((2_GHz).count() == 2);
static_assert((1_THz).count() == 1);

// min/max
static_assert(hertz::min().count() < 0);
static_assert(hertz::max().count() > 0);

// Modulo operations
static_assert((hertz(10) % 3).count() == 1);
static_assert((hertz(10) % hertz(3)).count() == 1);

// Division returning scalar
static_assert(hertz(10) / hertz(2) == 5);
static_assert(kilohertz(10) / kilohertz(2) == 5);

// Harmonics and subharmonics
static_assert(hertz(100).harmonic(2) == hertz(200));
static_assert(hertz(100).harmonic(3) == hertz(300));
static_assert(hertz(440).harmonic(2) == hertz(880));
static_assert(kilohertz(1).harmonic(5) == kilohertz(5));
static_assert(hertz(1000).subharmonic(2) == hertz(500));
static_assert(hertz(900).subharmonic(3) == hertz(300));
static_assert(hertz(880).subharmonic(2) == hertz(440));
static_assert(kilohertz(10).subharmonic(5) == kilohertz(2));

// Rounding functions
static_assert(floor<kilohertz>(hertz(1500)).count() == 1);
static_assert(floor<kilohertz>(hertz(1999)).count() == 1);
static_assert(floor<kilohertz>(hertz(2000)).count() == 2);
static_assert(ceil<kilohertz>(hertz(1500)).count() == 2);
static_assert(ceil<kilohertz>(hertz(1001)).count() == 2);
static_assert(ceil<kilohertz>(hertz(1000)).count() == 1);
static_assert(round<kilohertz>(hertz(1500)).count() == 2); // ties to even
static_assert(round<kilohertz>(hertz(2500)).count() == 2); // ties to even
static_assert(round<kilohertz>(hertz(1499)).count() == 1);
static_assert(round<kilohertz>(hertz(1501)).count() == 2);
static_assert(abs(hertz(100)).count() == 100);
static_assert(abs(hertz(-100)).count() == 100);
static_assert(abs(hertz(0)).count() == 0);

// beat
static_assert(beat(100_Hz, 40_Hz) == 60_Hz);
static_assert(beat(40_Hz, 100_Hz) == 60_Hz);        // commutative
static_assert(beat(1000_Hz, 1000_Hz).count() == 0); // same frequency

// =============================================================================
// Runtime tests (Catch2 TEST_CASE)
// =============================================================================

TEST_CASE("frequency construction", "[frequency]") {
    REQUIRE(hertz(10).count() == 10);
    REQUIRE(hertz::zero().count() == 0);
}

TEST_CASE("frequency arithmetic", "[frequency]") {
    REQUIRE((hertz(5) + hertz(3)).count() == 8);
    REQUIRE((hertz(5) - hertz(3)).count() == 2);
    REQUIRE((hertz(5) * 2).count() == 10);
    REQUIRE((hertz(10) / 2).count() == 5);
}

TEST_CASE("frequency precision conversion Hz to mHz", "[frequency]") {
    // Hz to mHz (implicit, lossless - multiplies by 1000)
    millihertz mhz = hertz(1);
    REQUIRE(mhz.count() == 1000);

    millihertz mhz2 = hertz(25);
    REQUIRE(mhz2.count() == 25000);
}

TEST_CASE("frequency precision conversion mHz to Hz", "[frequency]") {
    // mHz to Hz (explicit, lossy - divides by 1000)
    hertz hz = frequency_cast<hertz>(millihertz(1500));
    REQUIRE(hz.count() == 1); // truncates

    hertz hz2 = frequency_cast<hertz>(millihertz(1999));
    REQUIRE(hz2.count() == 1); // still truncates
}

TEST_CASE("frequency precision conversion kHz to Hz", "[frequency]") {
    hertz hz = kilohertz(1); // implicit, lossless
    REQUIRE(hz.count() == 1000);

    hertz hz2 = kilohertz(80);
    REQUIRE(hz2.count() == 80000);
}

TEST_CASE("frequency precision conversion Hz to kHz", "[frequency]") {
    // Hz to kHz (explicit, lossy)
    kilohertz khz = frequency_cast<kilohertz>(hertz(1500));
    REQUIRE(khz.count() == 1); // truncates
}

TEST_CASE("frequency precision conversion MHz to kHz", "[frequency]") {
    kilohertz khz = megahertz(1); // implicit, lossless
    REQUIRE(khz.count() == 1000);

    kilohertz khz2 = megahertz(80);
    REQUIRE(khz2.count() == 80000);
}

TEST_CASE("frequency precision conversion GHz to MHz", "[frequency]") {
    megahertz mhz = gigahertz(1); // implicit, lossless
    REQUIRE(mhz.count() == 1000);

    megahertz mhz2 = gigahertz(2);
    REQUIRE(mhz2.count() == 2000);
}

TEST_CASE("frequency increment/decrement", "[frequency]") {
    hertz f(10);

    ++f;
    REQUIRE(f.count() == 11);

    --f;
    REQUIRE(f.count() == 10);

    f++;
    REQUIRE(f.count() == 11);

    f--;
    REQUIRE(f.count() == 10);
}

TEST_CASE("frequency compound assignment", "[frequency]") {
    hertz f(10);

    f += hertz(5);
    REQUIRE(f.count() == 15);

    f -= hertz(3);
    REQUIRE(f.count() == 12);

    f *= 2;
    REQUIRE(f.count() == 24);

    f /= 4;
    REQUIRE(f.count() == 6);
}

TEST_CASE("frequency comparison", "[frequency]") {
    REQUIRE(hertz(1000) == hertz(1000));
    REQUIRE(hertz(1000) != hertz(1001));
    REQUIRE(hertz(1000) < hertz(1001));
    REQUIRE(hertz(1001) > hertz(1000));
    REQUIRE(hertz(1000) <= hertz(1000));
    REQUIRE(hertz(1000) <= hertz(1001));
    REQUIRE(hertz(1001) >= hertz(1001));
    REQUIRE(hertz(1001) >= hertz(1000));
}

TEST_CASE("frequency cross-precision comparison", "[frequency]") {
    REQUIRE(hertz(1) == millihertz(1000));
    REQUIRE(kilohertz(1) == hertz(1000));
    REQUIRE(megahertz(1) == kilohertz(1000));
    REQUIRE(gigahertz(1) == megahertz(1000));
}

TEST_CASE("frequency literals", "[frequency][literals]") {
    REQUIRE((1000_mHz).count() == 1000);
    REQUIRE((1000_Hz).count() == 1000);
    REQUIRE((80_kHz).count() == 80);
    REQUIRE((80_MHz).count() == 80);
    REQUIRE((2_GHz).count() == 2);
}

TEST_CASE("frequency modulo", "[frequency]") {
    REQUIRE((hertz(10) % 3).count() == 1);
    REQUIRE((hertz(10) % hertz(3)).count() == 1);
}

TEST_CASE("frequency division returning scalar", "[frequency]") {
    REQUIRE(hertz(10) / hertz(2) == 5);
    REQUIRE(kilohertz(10) / kilohertz(2) == 5);
}

TEST_CASE("to_string", "[frequency][string]") {
    REQUIRE(to_string(millihertz(1000)) == "1000mHz");
    REQUIRE(to_string(hertz(1000)) == "1000Hz");
    REQUIRE(to_string(kilohertz(80)) == "80kHz");
    REQUIRE(to_string(megahertz(80)) == "80MHz");
    REQUIRE(to_string(gigahertz(2)) == "2GHz");
}

TEST_CASE("frequency period", "[frequency][period]") {
    using namespace std::chrono;

    // 1 Hz = 1 second period
    auto p1 = hertz(1).period<seconds>();
    REQUIRE(p1.count() == 1);
    REQUIRE(p1 == seconds(1));

    // 1000 Hz = 1 millisecond period
    auto p2 = hertz(1000).period<duration<double, std::milli>>();
    REQUIRE(p2.count() == 1.0);
    REQUIRE(duration_cast<milliseconds>(p2) == milliseconds(1));

    // 1 kHz = 1 millisecond period
    auto p3 = kilohertz(1).period<milliseconds>();
    REQUIRE(p3 == milliseconds(1));

    // 1 MHz = 1 microsecond period
    auto p4 = megahertz(1).period<microseconds>();
    REQUIRE(p4 == microseconds(1));

    // 1 GHz = 1 nanosecond period
    auto p5 = gigahertz(1).period<nanoseconds>();
    REQUIRE(p5 == nanoseconds(1));

    // Test with different return types
    auto p6 = hertz(10).period<milliseconds>();
    REQUIRE(p6 == milliseconds(100));

    // Zero frequency should return max duration
    auto p7 = hertz(0).period<duration<double>>();
    REQUIRE(p7 == duration<double>::max());
}

TEST_CASE("frequency period precision", "[frequency][period]") {
    using namespace std::chrono;

    // 2500 Hz = 0.4 ms = 400 microseconds
    auto p1 = hertz(2500).period<microseconds>();
    REQUIRE(p1 == microseconds(400));

    // 250 kHz = 4 microseconds
    auto p2 = kilohertz(250).period<microseconds>();
    REQUIRE(p2 == microseconds(4));

    // 2 GHz = 0.5 nanoseconds
    auto p3 = gigahertz(2).period<nanoseconds>();
    REQUIRE(p3 == nanoseconds(0)); // truncates to 0

    // But as double it should be correct
    auto p4 = gigahertz(2).period<duration<double, std::nano>>();
    REQUIRE(p4.count() == Catch::Approx(0.5).epsilon(0.01));
}

TEST_CASE("frequency period integer arithmetic", "[frequency][period][integer]") {
    using namespace std::chrono;

    // Test exact integer-to-integer period conversions
    // These should all be exact with the optimized integer path

    // Common exact conversions
    REQUIRE(hertz(1).period<seconds>() == seconds(1));
    REQUIRE(hertz(1000).period<milliseconds>() == milliseconds(1));
    REQUIRE(kilohertz(1).period<milliseconds>() == milliseconds(1));
    REQUIRE(hertz(100).period<microseconds>() == microseconds(10000));
    REQUIRE(kilohertz(100).period<microseconds>() == microseconds(10));
    REQUIRE(megahertz(1).period<microseconds>() == microseconds(1));
    REQUIRE(megahertz(100).period<nanoseconds>() == nanoseconds(10));
    REQUIRE(gigahertz(1).period<nanoseconds>() == nanoseconds(1));

    // Less common but still exact
    REQUIRE(hertz(500).period<milliseconds>() == milliseconds(2));
    REQUIRE(hertz(250).period<milliseconds>() == milliseconds(4));
    REQUIRE(hertz(125).period<milliseconds>() == milliseconds(8));
    REQUIRE(kilohertz(500).period<microseconds>() == microseconds(2));
    REQUIRE(kilohertz(250).period<microseconds>() == microseconds(4));

    // Verify these work with larger values (testing overflow protection)
    REQUIRE(hertz(1000000).period<microseconds>() == microseconds(1));
    REQUIRE(kilohertz(1000).period<microseconds>() == microseconds(1));
}

TEST_CASE("frequency period floating point", "[frequency][period][floating]") {
    using namespace std::chrono;
    using hertz_d = frequency<double>;
    using kilohertz_d = frequency<double, std::kilo>;

    // Floating-point frequency types should work correctly
    auto p1 = hertz_d{440.0}.period<duration<double, std::milli>>();
    REQUIRE(p1.count() == Catch::Approx(2.272727).epsilon(0.0001));

    auto p2 = hertz_d{1000.5}.period<duration<double, std::milli>>();
    REQUIRE(p2.count() == Catch::Approx(0.9995).epsilon(0.0001));

    auto p3 = kilohertz_d{88.5}.period<duration<double, std::micro>>();
    REQUIRE(p3.count() == Catch::Approx(11.2994).epsilon(0.001));

    // Mixed: integer frequency, floating-point duration
    auto p4 = hertz(440).period<duration<double, std::milli>>();
    REQUIRE(p4.count() == Catch::Approx(2.272727).epsilon(0.0001));
}

TEST_CASE("frequency period with literals", "[frequency][period][literals]") {
    using namespace std::chrono;

    auto p1 = (1000_Hz).period<milliseconds>();
    REQUIRE(p1 == milliseconds(1));

    auto p2 = (1_kHz).period<milliseconds>();
    REQUIRE(p2 == milliseconds(1));

    auto p3 = (1_MHz).period<microseconds>();
    REQUIRE(p3 == microseconds(1));
}

TEST_CASE("frequency wavelength", "[frequency][wavelength]") {
    using namespace std::chrono;

    // For testing, we'll use duration types as distance-like types
    // Using a simple model: if time_per_meter = 1 nanosecond,
    // then velocity = 1 meter/nanosecond = 1,000,000,000 m/s

    // 1 GHz with 1 ns/meter -> wavelength = 1 meter
    // period = 1 ns, wavelength = 1 ns / (1 ns/m) = 1 m
    using meters = duration<int64_t, std::ratio<1>>;
    auto w1 = gigahertz(1).wavelength<meters>(nanoseconds(1));
    REQUIRE(w1.count() == 1);

    // 500 MHz with 1 ns/meter -> wavelength = 2 meters
    // period = 2 ns, wavelength = 2 ns / (1 ns/m) = 2 m
    auto w2 = megahertz(500).wavelength<meters>(nanoseconds(1));
    REQUIRE(w2.count() == 2);

    // 2 GHz with 1 ns/meter -> wavelength = 0.5 meters
    using meters_double = duration<double, std::ratio<1>>;
    auto w3 = gigahertz(2).wavelength<meters_double>(nanoseconds(1));
    REQUIRE(w3.count() == Catch::Approx(0.5).epsilon(0.01));

    // 1 kHz with 1 microsecond/meter -> wavelength = 1000 meters
    // period = 1 ms = 1000 us, wavelength = 1000 us / (1 us/m) = 1000 m
    auto w4 = kilohertz(1).wavelength<meters>(microseconds(1));
    REQUIRE(w4.count() == 1000);

    // Zero frequency should return max wavelength
    auto w5 = hertz(0).wavelength<meters_double>(nanoseconds(1));
    REQUIRE(w5 == meters_double::max());
}

TEST_CASE("frequency wavelength with electromagnetic waves", "[frequency][wavelength]") {
    using namespace std::chrono;

    // Speed of light: ~299,792,458 m/s
    // Time per meter: 1/299792458 seconds ≈ 3.336 nanoseconds
    using meters = duration<double, std::ratio<1>>;
    auto time_per_meter = duration<double, std::nano>(3.33564095);

    // 1 Hz electromagnetic wave has wavelength of ~299,792,458 meters
    auto w1 = hertz(1).wavelength<meters>(time_per_meter);
    REQUIRE(w1.count() == Catch::Approx(299792458.0).epsilon(0.0001));

    // 1 MHz electromagnetic wave has wavelength of ~299.79 meters
    auto w2 = megahertz(1).wavelength<meters>(time_per_meter);
    REQUIRE(w2.count() == Catch::Approx(299.792458).epsilon(0.0001));

    // 2.4 GHz (WiFi) has wavelength of ~0.125 meters (12.5 cm)
    auto w3 = megahertz(2400).wavelength<meters>(time_per_meter);
    REQUIRE(w3.count() == Catch::Approx(0.1249).epsilon(0.01));

    // 100 MHz (FM radio) has wavelength of ~3 meters
    auto w4 = megahertz(100).wavelength<meters>(time_per_meter);
    REQUIRE(w4.count() == Catch::Approx(2.998).epsilon(0.01));
}

TEST_CASE("frequency wavelength with velocity", "[frequency][wavelength]") {
    using namespace std::chrono;

    // Simple distance type using duration as a stand-in (1 unit = 1 meter)
    using meters = duration<double, std::ratio<1>>;

    // Default speed of light
    // 1 MHz -> ~299.79 meters
    auto w1 = megahertz(1).wavelength<meters>();
    REQUIRE(w1.count() == Catch::Approx(299.792458).epsilon(0.0001));

    // 2.4 GHz (WiFi) -> ~0.125 meters (12.5 cm)
    using ghz_double = frequency<double, std::giga>;
    auto w2 = ghz_double(2.4).wavelength<meters>();
    REQUIRE(w2.count() == Catch::Approx(0.12491).epsilon(0.001));

    // 100 MHz (FM radio) -> ~3 meters
    auto w3 = megahertz(100).wavelength<meters>();
    REQUIRE(w3.count() == Catch::Approx(2.998).epsilon(0.01));

    // 5 GHz (WiFi 5) -> ~0.06 meters (6 cm)
    auto w4 = gigahertz(5).wavelength<meters>();
    REQUIRE(w4.count() == Catch::Approx(0.05996).epsilon(0.001));

    // Custom velocity: sound in air at 20°C (~343 m/s)
    // 440 Hz (A4 note) -> ~0.78 meters
    auto w5 = hertz(440).wavelength<meters>(343.0);
    REQUIRE(w5.count() == Catch::Approx(0.7795).epsilon(0.01));

    // 1000 Hz sound -> ~0.343 meters
    auto w6 = kilohertz(1).wavelength<meters>(343.0);
    REQUIRE(w6.count() == Catch::Approx(0.343).epsilon(0.01));

    // Zero frequency returns max
    auto w7 = hertz(0).wavelength<meters>();
    REQUIRE(w7 == meters::max());
}

TEST_CASE("frequency harmonic operations", "[frequency][harmonics]") {
    SECTION("harmonics") {
        hertz fundamental(440); // A4 concert pitch

        REQUIRE(fundamental.harmonic(1).count() == 440);
        REQUIRE(fundamental.harmonic(2).count() == 880);  // 2nd harmonic (octave up)
        REQUIRE(fundamental.harmonic(3).count() == 1320); // 3rd harmonic
        REQUIRE(fundamental.harmonic(4).count() == 1760); // 4th harmonic (2 octaves up)
        REQUIRE(fundamental.harmonic(5).count() == 2200); // 5th harmonic
    }

    SECTION("subharmonics") {
        hertz freq(1000);

        REQUIRE(freq.subharmonic(1).count() == 1000);
        REQUIRE(freq.subharmonic(2).count() == 500);
        REQUIRE(freq.subharmonic(4).count() == 250);
        REQUIRE(freq.subharmonic(5).count() == 200);
        REQUIRE(freq.subharmonic(10).count() == 100);
    }

    SECTION("harmonics with different precisions") {
        kilohertz khz(1);
        REQUIRE(khz.harmonic(3).count() == 3);
        REQUIRE(khz.harmonic(10).count() == 10);

        megahertz mhz(5);
        REQUIRE(mhz.harmonic(2).count() == 10);
        REQUIRE(mhz.harmonic(4).count() == 20);
    }

    SECTION("subharmonics with different precisions") {
        kilohertz khz(10);
        REQUIRE(khz.subharmonic(2).count() == 5);
        REQUIRE(khz.subharmonic(5).count() == 2);

        megahertz mhz(100);
        REQUIRE(mhz.subharmonic(10).count() == 10);
        REQUIRE(mhz.subharmonic(25).count() == 4);
    }

    SECTION("harmonic/subharmonic round trip") {
        hertz base(100);
        auto up = base.harmonic(5);    // 500 Hz
        auto down = up.subharmonic(5); // 100 Hz
        REQUIRE(down == base);
    }

    SECTION("harmonics with literals") {
        REQUIRE((440_Hz).harmonic(2) == 880_Hz);
        REQUIRE((1_kHz).harmonic(3) == 3_kHz);
        REQUIRE((100_MHz).harmonic(10) == 1000_MHz);
    }

    SECTION("subharmonics with literals") {
        REQUIRE((1000_Hz).subharmonic(2) == 500_Hz);
        REQUIRE((10_kHz).subharmonic(5) == 2_kHz);
        REQUIRE((100_MHz).subharmonic(4) == 25_MHz);
    }
}
// =============================================================================
// Octave and semitone operations tests
// =============================================================================

TEST_CASE("frequency octave operations", "[frequency][octaves]") {
    SECTION("octave_shift up") {
        hertz a4(440);

        // One octave up doubles the frequency
        auto a5 = a4.octave_shift(1.0);
        REQUIRE(a5.count() == 880);

        // Two octaves up quadruples the frequency
        auto a6 = a4.octave_shift(2.0);
        REQUIRE(a6.count() == 1760);

        // Half octave up
        auto between = a4.octave_shift(0.5);
        REQUIRE(between.count() == 622); // sqrt(2) * 440 ≈ 622
    }

    SECTION("octave_shift down") {
        hertz a4(440);

        // One octave down halves the frequency
        auto a3 = a4.octave_shift(-1.0);
        REQUIRE(a3.count() == 220);

        // Two octaves down
        auto a2 = a4.octave_shift(-2.0);
        REQUIRE(a2.count() == 110);
    }

    SECTION("octaves_from") {
        hertz a3(220);
        hertz a4(440);
        hertz a5(880);

        REQUIRE(a4.octaves_from(a3) == Catch::Approx(1.0));
        REQUIRE(a5.octaves_from(a4) == Catch::Approx(1.0));
        REQUIRE(a5.octaves_from(a3) == Catch::Approx(2.0));
        REQUIRE(a3.octaves_from(a4) == Catch::Approx(-1.0));
    }

    SECTION("octave round trip") {
        hertz base(440);
        auto up = base.octave_shift(2.5);
        auto back = up.octave_shift(-2.5);
        REQUIRE(back.count() == 440);
    }
}

TEST_CASE("frequency semitone operations", "[frequency][semitones]") {
    SECTION("semitone_shift up") {
        hertz a4(440);

        // 12 semitones = 1 octave
        auto octave_up = a4.semitone_shift(12);
        REQUIRE(octave_up.count() == 880);

        // 1 semitone (minor second)
        auto a_sharp = a4.semitone_shift(1);
        REQUIRE(a_sharp.count() == 466); // 440 * 2^(1/12) ≈ 466

        // 7 semitones (perfect fifth)
        auto e5 = a4.semitone_shift(7);
        REQUIRE(e5.count() == 659); // 440 * 2^(7/12) ≈ 659
    }

    SECTION("semitone_shift down") {
        hertz a4(440);

        // 12 semitones down = 1 octave down
        auto octave_down = a4.semitone_shift(-12);
        REQUIRE(octave_down.count() == 220);

        // 1 semitone down
        auto g_sharp = a4.semitone_shift(-1);
        REQUIRE(g_sharp.count() == 415); // 440 / 2^(1/12) ≈ 415
    }

    SECTION("semitones_from") {
        hertz a4(440);
        hertz a5(880);

        // One octave = 12 semitones
        REQUIRE(a5.semitones_from(a4) == Catch::Approx(12.0));
        REQUIRE(a4.semitones_from(a5) == Catch::Approx(-12.0));

        // Perfect fifth ≈ 7 semitones (if we had exact frequency)
        hertz perfect_fifth(659);
        REQUIRE(perfect_fifth.semitones_from(a4) == Catch::Approx(7.0).epsilon(0.01));
    }

    SECTION("semitone round trip") {
        hertz base(440);
        auto up = base.semitone_shift(7);
        auto back = up.semitone_shift(-7);
        REQUIRE(back.count() == 440);
    }

    SECTION("octave and semitone equivalence") {
        hertz base(440);

        auto octave_method = base.octave_shift(1.0);
        auto semitone_method = base.semitone_shift(12);

        REQUIRE(octave_method.count() == semitone_method.count());
    }
}

TEST_CASE("frequency musical intervals", "[frequency][music]") {
    SECTION("equal temperament tuning") {
        hertz a4(440); // Standard concert pitch

        // Major scale intervals from A
        auto b = a4.semitone_shift(2);        // Major second
        auto c_sharp = a4.semitone_shift(4);  // Major third
        auto d = a4.semitone_shift(5);        // Perfect fourth
        auto e = a4.semitone_shift(7);        // Perfect fifth
        auto f_sharp = a4.semitone_shift(9);  // Major sixth
        auto g_sharp = a4.semitone_shift(11); // Major seventh
        auto a5 = a4.semitone_shift(12);      // Octave

        REQUIRE(b.count() == 494);
        REQUIRE(c_sharp.count() == 554);
        REQUIRE(d.count() == 587);
        REQUIRE(e.count() == 659);
        REQUIRE(f_sharp.count() == 740);
        REQUIRE(g_sharp.count() == 831);
        REQUIRE(a5.count() == 880);
    }
}

// =============================================================================
// Floating-point frequency tests
// =============================================================================

TEST_CASE("floating-point frequency construction", "[frequency][floating-point]") {
    using hertz_d = frequency<double, std::ratio<1>>;
    using kilohertz_d = frequency<double, std::kilo>;

    hertz_d f1{440.0};
    REQUIRE(f1.count() == 440.0);

    hertz_d f2{440.5};
    REQUIRE(f2.count() == 440.5);

    kilohertz_d f3{88.5};
    REQUIRE(f3.count() == 88.5);
}

TEST_CASE("floating-point frequency arithmetic", "[frequency][floating-point]") {
    using hertz_d = frequency<double, std::ratio<1>>;

    hertz_d f1{440.5};
    hertz_d f2{100.25};

    auto sum = f1 + f2;
    REQUIRE(sum.count() == Catch::Approx(540.75));

    auto diff = f1 - f2;
    REQUIRE(diff.count() == Catch::Approx(340.25));

    auto scaled = f1 * 2.0;
    REQUIRE(scaled.count() == Catch::Approx(881.0));

    auto divided = f1 / 2.0;
    REQUIRE(divided.count() == Catch::Approx(220.25));
}

TEST_CASE("floating-point frequency conversions", "[frequency][floating-point]") {
    using hertz_d = frequency<double, std::ratio<1>>;
    using kilohertz_d = frequency<double, std::kilo>;

    // Implicit lossless conversion (coarser to finer)
    kilohertz_d khz{1.5};
    hertz_d hz = khz;
    REQUIRE(hz.count() == Catch::Approx(1500.0));

    // Explicit lossy conversion (finer to coarser)
    hertz_d hz2{2750.5};
    kilohertz_d khz2 = frequency_cast<kilohertz_d>(hz2);
    REQUIRE(khz2.count() == Catch::Approx(2.7505));

    // Integer to floating-point conversion
    hertz int_freq{440};
    hertz_d float_freq{int_freq};
    REQUIRE(float_freq.count() == Catch::Approx(440.0));
}

TEST_CASE("floating-point frequency musical operations", "[frequency][floating-point][music]") {
    using hertz_d = frequency<double, std::ratio<1>>;

    SECTION("concert pitch and intervals") {
        hertz_d a4{440.0};

        // Perfect fifth (7 semitones) - should be exactly 1.5x in just intonation,
        // but equal temperament gives 2^(7/12)
        auto e5 = a4.semitone_shift(7.0);
        REQUIRE(e5.count() == Catch::Approx(659.255).epsilon(0.001));

        // Major third (4 semitones)
        auto c_sharp = a4.semitone_shift(4.0);
        REQUIRE(c_sharp.count() == Catch::Approx(554.365).epsilon(0.001));
    }

    SECTION("octave operations with fractional octaves") {
        hertz_d a4{440.0};

        // Half octave (tritone/augmented fourth)
        auto tritone = a4.octave_shift(0.5);
        REQUIRE(tritone.count() == Catch::Approx(622.254).epsilon(0.001));

        // Quarter octave
        auto quarter = a4.octave_shift(0.25);
        REQUIRE(quarter.count() == Catch::Approx(523.251).epsilon(0.001));

        // 1.5 octaves
        auto up_1_5 = a4.octave_shift(1.5);
        REQUIRE(up_1_5.count() == Catch::Approx(1244.508).epsilon(0.001));
    }

    SECTION("precise frequency ratios") {
        hertz_d fundamental{100.0};

        // Harmonic series with floating-point allows exact ratios
        auto second_harmonic = fundamental * 2.0;
        auto third_harmonic = fundamental * 3.0;
        auto fifth_harmonic = fundamental * 5.0;

        REQUIRE(second_harmonic.count() == Catch::Approx(200.0));
        REQUIRE(third_harmonic.count() == Catch::Approx(300.0));
        REQUIRE(fifth_harmonic.count() == Catch::Approx(500.0));

        // Just intonation intervals (exact frequency ratios)
        auto perfect_fifth = fundamental * 1.5; // 3:2 ratio
        auto major_third = fundamental * 1.25;  // 5:4 ratio

        REQUIRE(perfect_fifth.count() == Catch::Approx(150.0));
        REQUIRE(major_third.count() == Catch::Approx(125.0));
    }
}

TEST_CASE("floating-point frequency comparisons", "[frequency][floating-point]") {
    using hertz_d = frequency<double, std::ratio<1>>;

    hertz_d f1{440.0};
    hertz_d f2{440.0};
    hertz_d f3{440.5};

    REQUIRE(f1 == f2);
    REQUIRE(f1 != f3);
    REQUIRE(f1 < f3);
    REQUIRE(f3 > f1);
    REQUIRE(f1 <= f2);
    REQUIRE(f3 >= f1);
}

TEST_CASE("mixed integer and floating-point operations", "[frequency][floating-point]") {
    using hertz_d = frequency<double, std::ratio<1>>;

    hertz int_freq{440};
    hertz_d float_freq{440.0};

    // Comparison across types
    REQUIRE(hertz_d(int_freq) == float_freq);

    // Arithmetic with conversion
    hertz_d result = hertz_d(int_freq) * 1.5;
    REQUIRE(result.count() == Catch::Approx(660.0));
}

// =============================================================================
// Rounding function tests
// =============================================================================

TEST_CASE("frequency floor", "[frequency][rounding]") {
    SECTION("Hz to kHz") {
        REQUIRE(floor<kilohertz>(hertz(1500)).count() == 1);
        REQUIRE(floor<kilohertz>(hertz(1999)).count() == 1);
        REQUIRE(floor<kilohertz>(hertz(2000)).count() == 2);
        REQUIRE(floor<kilohertz>(hertz(500)).count() == 0);
    }

    SECTION("mHz to Hz") {
        REQUIRE(floor<hertz>(millihertz(1500)).count() == 1);
        REQUIRE(floor<hertz>(millihertz(999)).count() == 0);
        REQUIRE(floor<hertz>(millihertz(1000)).count() == 1);
    }

    SECTION("negative values") {
        REQUIRE(floor<kilohertz>(hertz(-1500)).count() == -2);
        REQUIRE(floor<kilohertz>(hertz(-1000)).count() == -1);
        REQUIRE(floor<kilohertz>(hertz(-999)).count() == -1);
    }

    SECTION("exact conversions") {
        REQUIRE(floor<kilohertz>(hertz(1000)) == kilohertz(1));
        REQUIRE(floor<kilohertz>(hertz(5000)) == kilohertz(5));
        REQUIRE(floor<hertz>(millihertz(3000)) == hertz(3));
    }

    SECTION("with literals") {
        REQUIRE(floor<kilohertz>(1500_Hz).count() == 1);
        REQUIRE(floor<hertz>(1500_mHz).count() == 1);
    }
}

TEST_CASE("frequency ceil", "[frequency][rounding]") {
    SECTION("Hz to kHz") {
        REQUIRE(ceil<kilohertz>(hertz(1500)).count() == 2);
        REQUIRE(ceil<kilohertz>(hertz(1001)).count() == 2);
        REQUIRE(ceil<kilohertz>(hertz(1000)).count() == 1);
        REQUIRE(ceil<kilohertz>(hertz(500)).count() == 1);
    }

    SECTION("mHz to Hz") {
        REQUIRE(ceil<hertz>(millihertz(1500)).count() == 2);
        REQUIRE(ceil<hertz>(millihertz(1001)).count() == 2);
        REQUIRE(ceil<hertz>(millihertz(1000)).count() == 1);
    }

    SECTION("negative values") {
        REQUIRE(ceil<kilohertz>(hertz(-1500)).count() == -1);
        REQUIRE(ceil<kilohertz>(hertz(-1000)).count() == -1);
        REQUIRE(ceil<kilohertz>(hertz(-1001)).count() == -1);
    }

    SECTION("exact conversions") {
        REQUIRE(ceil<kilohertz>(hertz(1000)) == kilohertz(1));
        REQUIRE(ceil<kilohertz>(hertz(5000)) == kilohertz(5));
        REQUIRE(ceil<hertz>(millihertz(3000)) == hertz(3));
    }

    SECTION("with literals") {
        REQUIRE(ceil<kilohertz>(1500_Hz).count() == 2);
        REQUIRE(ceil<hertz>(1001_mHz).count() == 2);
    }
}

TEST_CASE("frequency round", "[frequency][rounding]") {
    SECTION("Hz to kHz - basic rounding") {
        REQUIRE(round<kilohertz>(hertz(1499)).count() == 1);
        REQUIRE(round<kilohertz>(hertz(1501)).count() == 2);
        REQUIRE(round<kilohertz>(hertz(1000)).count() == 1);
        REQUIRE(round<kilohertz>(hertz(2000)).count() == 2);
    }

    SECTION("Hz to kHz - ties to even") {
        // 1500 Hz is exactly 1.5 kHz, should round to 2 (even)
        REQUIRE(round<kilohertz>(hertz(1500)).count() == 2);
        // 2500 Hz is exactly 2.5 kHz, should round to 2 (even)
        REQUIRE(round<kilohertz>(hertz(2500)).count() == 2);
        // 3500 Hz is exactly 3.5 kHz, should round to 4 (even)
        REQUIRE(round<kilohertz>(hertz(3500)).count() == 4);
        // 4500 Hz is exactly 4.5 kHz, should round to 4 (even)
        REQUIRE(round<kilohertz>(hertz(4500)).count() == 4);
    }

    SECTION("mHz to Hz") {
        REQUIRE(round<hertz>(millihertz(1499)).count() == 1);
        REQUIRE(round<hertz>(millihertz(1501)).count() == 2);
        REQUIRE(round<hertz>(millihertz(1500)).count() == 2); // ties to even
        REQUIRE(round<hertz>(millihertz(2500)).count() == 2); // ties to even
    }

    SECTION("negative values") {
        REQUIRE(round<kilohertz>(hertz(-1499)).count() == -1);
        REQUIRE(round<kilohertz>(hertz(-1501)).count() == -2);
        REQUIRE(round<kilohertz>(hertz(-1500)).count() == -2); // ties to even
        REQUIRE(round<kilohertz>(hertz(-2500)).count() == -2); // ties to even
    }

    SECTION("exact conversions") {
        REQUIRE(round<kilohertz>(hertz(1000)) == kilohertz(1));
        REQUIRE(round<kilohertz>(hertz(5000)) == kilohertz(5));
    }

    SECTION("with literals") {
        REQUIRE(round<kilohertz>(1499_Hz).count() == 1);
        REQUIRE(round<kilohertz>(1501_Hz).count() == 2);
        REQUIRE(round<kilohertz>(1500_Hz).count() == 2);
    }
}

TEST_CASE("frequency abs", "[frequency][rounding]") {
    SECTION("positive values") {
        REQUIRE(abs(hertz(100)).count() == 100);
        REQUIRE(abs(hertz(440)).count() == 440);
        REQUIRE(abs(kilohertz(5)).count() == 5);
    }

    SECTION("negative values") {
        REQUIRE(abs(hertz(-100)).count() == 100);
        REQUIRE(abs(hertz(-440)).count() == 440);
        REQUIRE(abs(kilohertz(-5)).count() == 5);
    }

    SECTION("zero") {
        REQUIRE(abs(hertz(0)).count() == 0);
        REQUIRE(abs(hertz::zero()).count() == 0);
    }

    SECTION("with different precisions") {
        REQUIRE(abs(millihertz(-1500)).count() == 1500);
        REQUIRE(abs(megahertz(-100)).count() == 100);
        REQUIRE(abs(gigahertz(-2)).count() == 2);
    }

    SECTION("with literals") {
        REQUIRE(abs(-440_Hz).count() == 440);
        REQUIRE(abs(-88_MHz).count() == 88);
    }

    SECTION("preserves type") {
        hertz h = abs(hertz(-100));
        kilohertz k = abs(kilohertz(-5));
        REQUIRE(h.count() == 100);
        REQUIRE(k.count() == 5);
    }
}

TEST_CASE("beat calculations", "[frequency][interference]") {
    SECTION("same precision - integer") {
        REQUIRE(beat(1000_Hz, 600_Hz) == 400_Hz);
        REQUIRE(beat(600_Hz, 1000_Hz) == 400_Hz); // commutative
    }

    SECTION("same frequency yields zero beat") {
        REQUIRE(beat(440_Hz, 440_Hz).count() == 0);
        REQUIRE(beat(1_kHz, 1_kHz).count() == 0);
    }

    SECTION("mixed precisions") {
        REQUIRE(beat(2_kHz, 500_Hz) == 1500_Hz);
        REQUIRE(beat(1000_Hz, 1_kHz).count() == 0);
        REQUIRE(beat(1_MHz, 500_kHz) == 500_kHz);
    }

    SECTION("negative frequencies") {
        REQUIRE(beat(hertz(-100), hertz(50)) == 150_Hz);
        REQUIRE(beat(hertz(100), hertz(-50)) == 150_Hz);
        REQUIRE(beat(hertz(-100), hertz(-50)) == 50_Hz);
    }

    SECTION("floating-point precision") {
        using hertz_d = frequency<double, std::ratio<1>>;
        auto b = beat(hertz_d{440.5}, hertz_d{438.2});
        REQUIRE(b.count() == Catch::Approx(2.3).epsilon(0.0001));
    }

    SECTION("with literals") {
        REQUIRE(beat(440_Hz, 442_Hz) == 2_Hz);
        REQUIRE(beat(1_kHz, 900_Hz) == 100_Hz);
    }

    SECTION("constexpr compatibility") {
        constexpr auto b = beat(100_Hz, 40_Hz);
        static_assert(b == 60_Hz);
    }
}

TEST_CASE("frequency rounding with floating-point", "[frequency][rounding][floating-point]") {
    using hertz_d = frequency<double, std::ratio<1>>;
    using kilohertz_d = frequency<double, std::kilo>;

    SECTION("floor with floating-point") {
        hertz_d h{1500.7};
        auto k = floor<kilohertz_d>(h);
        REQUIRE(k.count() == Catch::Approx(1.5007));
    }

    SECTION("ceil with floating-point") {
        hertz_d h{1500.3};
        auto k = ceil<kilohertz_d>(h);
        REQUIRE(k.count() == Catch::Approx(1.5003));
    }

    SECTION("round with floating-point") {
        hertz_d h{1500.5};
        auto k = round<kilohertz_d>(h);
        REQUIRE(k.count() == Catch::Approx(1.5005));
    }

    SECTION("abs with floating-point") {
        hertz_d h{-440.5};
        auto abs_h = abs(h);
        REQUIRE(abs_h.count() == Catch::Approx(440.5));
    }
}

TEST_CASE("frequency rounding edge cases", "[frequency][rounding]") {
    SECTION("floor with larger precision gaps") {
        REQUIRE(floor<megahertz>(hertz(1500000)).count() == 1);
        REQUIRE(floor<megahertz>(hertz(1999999)).count() == 1);
        REQUIRE(floor<megahertz>(hertz(2000000)).count() == 2);
    }

    SECTION("ceil with larger precision gaps") {
        REQUIRE(ceil<megahertz>(hertz(1000001)).count() == 2);
        REQUIRE(ceil<megahertz>(hertz(1000000)).count() == 1);
    }

    SECTION("round with larger precision gaps") {
        REQUIRE(round<megahertz>(hertz(1499999)).count() == 1);
        REQUIRE(round<megahertz>(hertz(1500000)).count() == 2); // ties to even
        REQUIRE(round<megahertz>(hertz(1500001)).count() == 2);
        REQUIRE(round<megahertz>(hertz(2500000)).count() == 2); // ties to even
    }

    SECTION("abs with min/max values") {
        // Test near boundaries (not exactly min/max to avoid overflow)
        hertz large_pos{1000000000};
        hertz large_neg{-1000000000};
        REQUIRE(abs(large_pos).count() == 1000000000);
        REQUIRE(abs(large_neg).count() == 1000000000);
    }
}
