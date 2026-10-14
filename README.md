# frequency

> **This library is no longer maintained.** It has been superseded by [quanta](https://github.com/cleishm/quanta-cpp), which includes it as `quanta::frequency` alongside temperature, electrical and spatial units, and is available as the ESP-IDF component [`cleishm/quanta`](https://components.espressif.com/components/cleishm/quanta). See [Migrating](https://github.com/cleishm/quanta-cpp#migrating-from-thermo-cpp-frequency-cpp-electro-cpp-and-spatial-cpp) for the changes. This is its final release.

A type-safe, header-only C++20 library for frequency handling, modeled after `std::chrono::duration`.

📚 **[Full API Documentation](https://cleishm.github.io/frequency-cpp/)**

## Features

- **Type-safe frequencies** with distinct types for different precisions
- **Standard SI units**: millihertz, hertz, kilohertz, megahertz, gigahertz, terahertz
- **Integer and floating-point representations** for exact or fractional precision
- **Automatic conversions** between precisions
- **Lossless implicit conversions** (lossy conversions require explicit casts)
- **User-defined literals** for concise notation
- **`std::format` support** (when available)
- **Fully constexpr** - all operations can be evaluated at compile time

## Requirements

- C++20 compiler (GCC 10+, Clang 10+, MSVC 19.26+)

## Installation

### CMake FetchContent

```cmake
include(FetchContent)
FetchContent_Declare(
    frequency
    GIT_REPOSITORY https://github.com/cleishm/frequency-cpp.git
    GIT_TAG v1.3.2
)
FetchContent_MakeAvailable(frequency)

target_link_libraries(your_target PRIVATE frequency::frequency)
```

### vcpkg

The library is available as the community-maintained `cleishm-frequency-cpp` port in
[microsoft/vcpkg](https://github.com/microsoft/vcpkg); new releases reach it after a short delay:

```bash
vcpkg install cleishm-frequency-cpp
```

```cmake
find_package(frequency CONFIG REQUIRED)
target_link_libraries(your_target PRIVATE frequency::frequency)
```

### Manual

Copy `include/frequency/frequency.hpp` to your project.

## Usage

```cpp
#include <frequency/frequency.hpp>

using namespace freq;
using namespace frequency_literals;

// Basic frequencies
hertz audio_sample_rate{44100};
kilohertz cpu_clock{3200};
megahertz radio_freq{88};
gigahertz wifi{2};

// Using literals
auto f1 = 1000_Hz;
auto f2 = 80_kHz;
auto f3 = 2400_MHz;
auto f4 = 5_GHz;

// Precision control
millihertz precise{44100000};  // 44100 Hz
hertz coarse = hertz(precise);  // explicit lossy conversion
millihertz back = coarse;  // implicit lossless conversion

// Arithmetic
auto sum = 1000_Hz + 500_Hz;     // 1500 Hz
auto diff = 5_GHz - 2400_MHz;    // common type conversion
auto scaled = 100_Hz * 3;        // 300 Hz
auto ratio = 1000_Hz / 100_Hz;   // 10 (scalar)

// Comparisons work across precisions
if (kilohertz{1} == hertz{1000}) {
    // true - same frequency, different precision
}

// String conversion
std::string s = to_string(kilohertz(80));  // "80kHz"

// Formatting: always in the type's own unit unless a marker asks otherwise
millihertz reading{1500};
std::string raw  = std::format("{}", reading);      // "1500mHz" (exact stored value)
std::string disp = std::format("{:.1f}", reading);  // "1500.0mHz"
std::string hz   = std::format("{:b.1f}", reading); // "1.5Hz" (base unit)
std::string sc   = std::format("{:s}", 2400_MHz);    // "2.4GHz" (scaled)
```

## Frequency Types

| Type | Unit | Precision |
|------|------|-----------|
| `millihertz` | mHz | 0.001 Hz |
| `hertz` | Hz | 1 Hz |
| `kilohertz` | kHz | 1,000 Hz |
| `megahertz` | MHz | 1,000,000 Hz |
| `gigahertz` | GHz | 1,000,000,000 Hz |
| `terahertz` | THz | 1,000,000,000,000 Hz |

All types use `int64_t` representation by default. For fractional precision, use `frequency<double, Precision>` (see Floating-Point Frequencies section).
### Literals

```cpp
using namespace frequency_literals;

1000_mHz   // millihertz(1000) = 1 Hz
1000_Hz    // hertz(1000)
80_kHz     // kilohertz(80)
2400_MHz   // megahertz(2400)
5_GHz      // gigahertz(5)
1_THz      // terahertz(1)
```

## Formatting

`to_string()` and `std::format` show a frequency in its own unit: `{}` prints the exact stored count
followed by the precision's SI-prefixed unit, and a format spec such as `{:.1f}` changes only how the
number is rendered (as a `double`), never the unit.

```cpp
to_string(kilohertz(433));                // "433kHz"
std::format("{}", millihertz(1500));      // "1500mHz"
std::format("{:.1f}", millihertz(1500));  // "1500.0mHz"
std::format("{:>8.1f}", hertz(50));       // "    50.0Hz"
```

To show a different unit, start the spec with a single marker letter; the rest of the spec is the
normal `std::format` spec for a `double`:

| Marker | Unit | Example |
|--------|------|---------|
| *(none)* | the type's own unit | `std::format("{:.1f}", millihertz(1500))` → `"1500.0mHz"` |
| `b` | base unit (hertz) | `std::format("{:b.1f}", millihertz(1500))` → `"1.5Hz"` |
| `s` | scaled: the largest SI prefix (fHz … THz) in which the magnitude is at least 1, else fHz; zero in Hz | `std::format("{:s}", megahertz(2400))` → `"2.4GHz"` |

```cpp
std::format("{:b}", kilohertz(433));       // "433000Hz"
std::format("{:s.1f}", millihertz(33));    // "33.0mHz"
std::format("{:s}", hertz(-2500));         // "-2.5kHz"
std::format("{:s*>8.2f}", hertz(1500));    // "****1.50kHz"

// The same forms without <format>:
to_string(millihertz(1500), base_unit);    // "1.5Hz"
to_string(megahertz(2400), scaled);        // "2.4GHz"
```

Notes:
- A spec that starts with `b` or `s` is always a marker, so those letters cannot be used as the fill
  character at the start of a spec (`{:s>8}` is the `s` marker followed by `>8`).
- Values are converted to `double` for the markers, so they work with integer representations too.
- With no spec (`{}` on a floating-point frequency, `{:b}`, `{:s}`, and every `to_string`), values are
  rendered in the shortest fixed notation that round-trips: `frequency<double>(1e6)` → `"1000000Hz"`,
  never `"1e+06Hz"`.
- A precision with no SI prefix (e.g. `frequency<int64_t, std::deci>`) has no unit of its own: `{}` and
  `to_string(f)` do not compile for it, and a spec without a marker renders the value in hertz
  (`{:.1f}` → `"22.5Hz"`). The `b` and `s` markers and tags work for every precision.
- To show a value in another unit, cast it, e.g. `std::format("{:.3f}", frequency<double, std::mega>(f))`.

## Period and Wavelength

```cpp
using namespace std::chrono;

hertz(1000).period<milliseconds>();               // 1 ms
megahertz(1).period<duration<double, std::micro>>(); // 1.0 µs

// Wavelength takes a length type: anything chrono-shaped (count(), rep, period
// relative to meters) other than a std::chrono::duration, such as the lengths
// of spatial (https://github.com/cleishm/spatial-cpp).
(2400_MHz).wavelength<spatial::millimeters>();      // 124 mm (speed of light)
hertz(440).wavelength<spatial::millimeters>(343.0); // 779 mm (sound in air)
```

`period<D>()` accepts only `std::chrono::duration` types, and `wavelength<L>()` rejects them.

## Conversion Rules

Conversions follow the same philosophy as `std::chrono`:

- **Implicit conversions** are allowed when lossless (e.g., `kilohertz` → `hertz`)
- **Explicit conversions** are required when lossy (e.g., `hertz` → `kilohertz`)

```cpp
// Implicit (lossless) - fine to coarser precision
hertz hz = kilohertz{1};          // OK: 1 kHz → 1000 Hz
millihertz mhz = hertz{1};        // OK: 1 Hz → 1000 mHz

// Explicit required (lossy) - coarse to finer precision with truncation
kilohertz khz = kilohertz(hertz{1500});  // 1500 Hz → 1 kHz (truncates)

// Use frequency_cast for explicit conversions
auto khz2 = frequency_cast<kilohertz>(hertz{2500});  // 2 kHz
```

## Floating-Point Frequencies

The library supports floating-point representations for fractional precision. This is useful for:
- Musical tuning and audio applications (e.g., concert pitch A = 440.0 Hz)
- Scientific measurements with sub-unit precision
- Calculations involving irrational ratios

```cpp
// Define floating-point frequency types
using hertz_d = frequency<double, std::ratio<1>>;
using kilohertz_d = frequency<double, std::kilo>;
using megahertz_d = frequency<double, std::mega>;

// Musical tuning - concert pitch
hertz_d concert_a{440.0};
hertz_d c_sharp = concert_a.semitone_shift(4);  // 554.37 Hz (major third)

// Scientific measurements
megahertz_d precise_radio{88.5};  // 88.5 MHz FM station
kilohertz_d exact_note{261.626};  // Middle C (C4) in Hz

// Fractional arithmetic
hertz_d f1{1000.5};
hertz_d f2{500.25};
hertz_d sum = f1 + f2;  // 1500.75 Hz

// Mixing integer and floating-point types
hertz int_freq{440};
hertz_d float_freq = hertz_d(int_freq);  // Convert to floating-point
float_freq = float_freq * 1.5;  // 660.0 Hz (perfect fifth interval)

// Octave operations (naturally work with floating-point)
hertz_d a4{440.0};
hertz_d a5 = a4.octave_shift(1.0);     // 880.0 Hz (one octave up)
hertz_d tritone = a4.octave_shift(0.5); // 622.25 Hz (half octave/tritone)

// Note: Modulo operations are disabled for floating-point types
// f1 % 3;  // Compile error - modulo not defined for floating-point
```

**Important considerations:**
- Floating-point frequencies allow fractional values but sacrifice exact arithmetic
- Implicit conversions between integer and floating-point frequencies follow the same lossless/lossy rules as precision conversions
- Integer types are preferred when exact values and modulo operations are needed
- Floating-point types are preferred for calculations involving division, musical intervals, or measurements with fractional precision

## Building Tests

```bash
cmake -B build -DFREQUENCY_BUILD_TESTS=ON
cmake --build build
ctest --test-dir build
```

## License

MIT License - see [LICENSE](LICENSE) for details.
