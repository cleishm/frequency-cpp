#pragma once

/**
 * @file frequency
 * @brief Frequency types and arithmetic, modeled after std::chrono.
 */

#include <cmath>
#include <compare>
#include <concepts>
#include <cstdint>
#include <limits>
#include <ratio>
#include <string>
#if __has_include(<format>) && defined(__cpp_lib_format)
#include <format>
#endif
#include <assert.h>

/**
 * @brief Frequency types and utilities.
 *
 * This library provides type-safe frequency handling with support for
 * multiple precisions (mHz, Hz, kHz, MHz, GHz, THz), following the design
 * of std::chrono::duration.
 *
 * ## Basic Usage
 *
 * The library provides standard integer-based frequency types:
 * @code
 * using namespace freq;
 * using namespace frequency_literals;
 *
 * hertz audio_rate{44100};
 * kilohertz cpu_clock{3200};
 * auto wifi = 2400_MHz;
 * auto optical = 193_THz;  // Near-infrared light
 * @endcode
 *
 * ## Floating-Point Frequencies
 *
 * For fractional precision (e.g., musical tuning, scientific measurements),
 * use floating-point representations:
 *
 * @code
 * // Define floating-point frequency types
 * using hertz_d = frequency<double, std::ratio<1>>;
 * using kilohertz_d = frequency<double, std::kilo>;
 * using megahertz_d = frequency<double, std::mega>;
 *
 * // Musical tuning - concert pitch
 * hertz_d concert_a{440.0};
 * hertz_d c_sharp = concert_a.semitone_shift(4);  // 554.37 Hz
 *
 * // Scientific measurements
 * megahertz_d fm_station{88.5};  // 88.5 MHz
 * kilohertz_d middle_c{261.626}; // Middle C in Hz
 *
 * // Fractional arithmetic
 * hertz_d f1{1000.5};
 * hertz_d f2{500.25};
 * hertz_d sum = f1 + f2;  // 1500.75 Hz
 *
 * // Octave operations with fractional values
 * hertz_d a4{440.0};
 * hertz_d tritone = a4.octave_shift(0.5);  // 622.25 Hz (half octave)
 * hertz_d perfect_fifth = a4 * 1.5;        // 660.0 Hz (3:2 ratio)
 * @endcode
 *
 * ## Integer vs Floating-Point
 *
 * - **Integer types** (default): Exact arithmetic, support modulo operations,
 *   preferred for digital systems and counting applications
 * - **Floating-point types**: Fractional precision, natural for calculations
 *   involving division and musical intervals, but no modulo operations
 *
 * Conversions between integer and floating-point follow the same rules as
 * precision conversions (implicit when lossless, explicit when lossy).
 */
namespace freq {

template<typename Rep, typename Precision = std::ratio<1>>
class frequency;

/**
 * @brief Trait to detect frequency specializations.
 * @tparam T Type to check.
 */
template<typename T>
struct is_frequency : std::false_type {};

template<typename Rep, typename Precision>
struct is_frequency<frequency<Rep, Precision>> : std::true_type {};

template<typename T>
inline constexpr bool is_frequency_v = is_frequency<T>::value;

/** @cond INTERNAL */
template<typename T, template<typename...> class Template>
struct _is_specialization_of : std::false_type {};

template<template<typename...> class Template, typename... Args>
struct _is_specialization_of<Template<Args...>, Template> : std::true_type {};

template<typename T, template<typename...> class Template>
inline constexpr bool _is_specialization_of_v = _is_specialization_of<T, Template>::value;

template<typename Rep>
concept not_frequency = !_is_specialization_of_v<Rep, frequency>;

/**
 * @brief Concept for duration-like types.
 *
 * A type satisfies duration_like if it has the same interface as std::chrono::duration:
 * - A count() method returning the tick count
 * - A nested period type (typically a std::ratio)
 * - A nested rep type (the representation type)
 */
template<typename T>
concept duration_like = requires(T t) {
    { t.count() };
    typename T::period;
    typename T::rep;
};

/**
 * @brief Concept for distance-like types.
 *
 * A type satisfies distance_like if it has the same interface as duration-like types,
 * but represents spatial distances rather than time durations:
 * - A count() method returning the tick count
 * - A nested period type (typically a std::ratio representing units relative to meters)
 * - A nested rep type (the representation type)
 */
template<typename T>
concept distance_like = requires(T t) {
    { t.count() };
    typename T::period;
    typename T::rep;
};

/**
 * @brief Provides special values for frequency representations.
 * @tparam Rep The representation type.
 */
template<typename Rep>
struct frequency_values {
    /** @brief Returns a zero frequency. */
    static constexpr Rep zero() noexcept { return Rep(0); }

    /** @brief Returns the maximum representable frequency. */
    static constexpr Rep max() noexcept { return std::numeric_limits<Rep>::max(); }

    /** @brief Returns the minimum representable frequency. */
    static constexpr Rep min() noexcept { return std::numeric_limits<Rep>::lowest(); }
};

template<typename T>
struct _is_ratio : std::false_type {};

template<std::intmax_t Num, std::intmax_t Denom>
struct _is_ratio<std::ratio<Num, Denom>> : std::true_type {};

/**
 * @brief Trait to indicate floating-point-like behavior.
 *
 * Specializations for floating-point types return true. User-defined types
 * may specialize this to enable implicit lossy conversions.
 *
 * This trait affects conversion rules and operation availability:
 * - Types with treat_as_inexact_v<T> == true allow implicit lossy conversions
 * - Modulo operations (%) are disabled for inexact types
 * - Affects rounding behavior in musical operations (octave_shift, semitone_shift)
 *
 * @code
 * // Enabled for standard floating-point types
 * static_assert(treat_as_inexact_v<float>);
 * static_assert(treat_as_inexact_v<double>);
 * static_assert(!treat_as_inexact_v<int>);
 * static_assert(!treat_as_inexact_v<int64_t>);
 * @endcode
 *
 * @tparam T The type to check.
 */
template<typename T>
struct treat_as_inexact : std::bool_constant<std::floating_point<T>> {};

template<typename T>
inline constexpr bool treat_as_inexact_v = treat_as_inexact<T>::value;

consteval intmax_t _gcd(intmax_t m, intmax_t n) noexcept {
    while (n != 0) {
        intmax_t rem = m % n;
        m = n;
        n = rem;
    }
    return m;
}

// Runtime GCD for integer types (using Euclidean algorithm)
template<typename T>
constexpr T _runtime_gcd(T m, T n) noexcept {
    if (m < 0) {
        m = -m;
    }
    if (n < 0) {
        n = -n;
    }
    while (n != 0) {
        T rem = m % n;
        m = n;
        n = rem;
    }
    return m;
}

template<typename R1, typename R2>
inline constexpr intmax_t _safe_ratio_divide_den = [] {
    constexpr intmax_t g1 = _gcd(R1::num, R2::num);
    constexpr intmax_t g2 = _gcd(R1::den, R2::den);
    return (R1::den / g2) * (R2::num / g1);
}();

template<typename From, typename To>
concept _harmonic_precision = _safe_ratio_divide_den<From, To> == 1;
/** @endcond */

/**
 * @brief A frequency value with a representation and precision.
 *
 * Similar to std::chrono::duration, frequency represents a quantity of
 * cycles per second. The precision is expressed as a std::ratio of one
 * Hertz.
 *
 * ## Basic Examples
 *
 * @code
 * // Integer frequencies (default)
 * hertz audio{44100};
 * kilohertz cpu{3200};
 * auto wifi = 2400_MHz;
 *
 * // Arithmetic
 * auto sum = 1000_Hz + 500_Hz;  // 1500 Hz
 * auto scaled = 100_Hz * 3;     // 300 Hz
 * auto ratio = 1000_Hz / 100_Hz; // 10 (scalar)
 * @endcode
 *
 * ## Floating-Point Examples
 *
 * @code
 * // Define floating-point types
 * using hertz_d = frequency<double>;
 * using kilohertz_d = frequency<double, std::kilo>;
 *
 * // Fractional values
 * hertz_d concert_pitch{440.0};
 * hertz_d a_flat = concert_pitch.semitone_shift(-1);  // 415.30 Hz
 *
 * // Exact ratios for just intonation
 * hertz_d fundamental{100.0};
 * hertz_d perfect_fifth = fundamental * 1.5;  // 3:2 ratio
 * hertz_d major_third = fundamental * 1.25;   // 5:4 ratio
 *
 * // Scientific measurements
 * kilohertz_d am_radio{540.5};  // 540.5 kHz
 * @endcode
 *
 * @tparam Rep       Arithmetic type for the tick count.
 * @tparam Precision A std::ratio representing the precision (default: 1 Hz).
 */
template<typename Rep, typename Precision>
class frequency {
    static_assert(!is_frequency<Rep>::value, "rep cannot be a frequency::frequency");
    static_assert(_is_ratio<Precision>::value, "precision must be a specialization of std::ratio");
    static_assert(Precision::num > 0, "precision must be positive");

public:
    /** @brief The representation type. */
    using rep = Rep;
    /** @brief The precision as a std::ratio. */
    using precision = typename Precision::type;

    /** @brief Constructs a zero frequency. */
    constexpr frequency() = default;
    frequency(const frequency&) = default;

    /**
     * @brief Constructs from a tick count.
     *
     * Implicit for inexact types, explicit otherwise to prevent accidental
     * precision loss.
     *
     * @tparam Rep2 The source representation type.
     * @param r     The tick count.
     */
    template<typename Rep2>
        requires std::convertible_to<const Rep2&, rep> && (treat_as_inexact_v<rep> || !treat_as_inexact_v<Rep2>)
    constexpr explicit frequency(const Rep2& r)
        : _r(static_cast<rep>(r)) {}

    /**
     * @brief Constructs from another frequency with different representation or precision.
     *
     * Implicit when the conversion is lossless (target precision evenly
     * divides source precision).
     *
     * @tparam Rep2       Source representation type.
     * @tparam Precision2 Source precision.
     * @param f           The source frequency.
     */
    template<typename Rep2, typename Precision2>
        requires std::convertible_to<const Rep2&, rep> &&
        (treat_as_inexact_v<rep> || (_harmonic_precision<Precision2, precision> && !treat_as_inexact_v<Rep2>))
    constexpr frequency(const frequency<Rep2, Precision2>& f)
        : _r(frequency_cast<frequency>(f).count()) {}

    /**
     * @brief Explicit constructor for lossy precision conversions.
     *
     * @tparam Rep2       Source representation type.
     * @tparam Precision2 Source precision.
     * @param f           The source frequency.
     */
    template<typename Rep2, typename Precision2>
        requires(!std::is_same_v<frequency, frequency<Rep2, Precision2>>) && (!treat_as_inexact_v<rep>) &&
        (!_harmonic_precision<Precision2, precision>)
    constexpr explicit frequency(const frequency<Rep2, Precision2>& f)
        : _r(frequency_cast<frequency>(f).count()) {}

    ~frequency() = default;
    frequency& operator=(const frequency&) = default;

    /** @brief Returns the tick count. */
    constexpr rep count() const { return _r; }

    constexpr frequency<typename std::common_type<rep>::type, precision> operator+() const {
        return frequency<typename std::common_type<rep>::type, precision>(_r);
    }

    constexpr frequency<typename std::common_type<rep>::type, precision> operator-() const {
        return frequency<typename std::common_type<rep>::type, precision>(-_r);
    }

    constexpr frequency& operator++() {
        ++_r;
        return *this;
    }

    constexpr frequency operator++(int) { return frequency(_r++); }

    constexpr frequency& operator--() {
        --_r;
        return *this;
    }

    constexpr frequency operator--(int) { return frequency(_r--); }

    constexpr frequency& operator+=(const frequency& f) {
        _r += f.count();
        return *this;
    }

    constexpr frequency& operator-=(const frequency& f) {
        _r -= f.count();
        return *this;
    }

    constexpr frequency& operator*=(const rep& r) {
        _r *= r;
        return *this;
    }

    constexpr frequency& operator/=(const rep& r) {
        _r /= r;
        return *this;
    }

    constexpr frequency& operator%=(const rep& r)
        requires(!treat_as_inexact_v<rep>)
    {
        _r %= r;
        return *this;
    }

    constexpr frequency& operator%=(const frequency& f)
        requires(!treat_as_inexact_v<rep>)
    {
        _r %= f.count();
        return *this;
    }

    /** @brief Returns a zero frequency. */
    static constexpr frequency zero() noexcept { return frequency(frequency_values<rep>::zero()); }

    /** @brief Returns the minimum representable frequency. */
    static constexpr frequency min() noexcept { return frequency(frequency_values<rep>::min()); }

    /** @brief Returns the maximum representable frequency. */
    static constexpr frequency max() noexcept { return frequency(frequency_values<rep>::max()); }

    /**
     * @brief Returns the period of this frequency as a duration.
     *
     * The period is the inverse of the frequency. For example, a frequency of
     * 1000 Hz has a period of 1 millisecond.
     *
     * For integer-to-integer conversions, this method uses exact integer arithmetic
     * with wider intermediate types (128-bit when available) to minimize overflow risk.
     * Common conversions like hertz{1000} to milliseconds will produce exact results.
     *
     * When either the frequency or duration uses floating-point representation,
     * the calculation falls back to floating-point arithmetic.
     *
     * @tparam Duration The target duration type. Must satisfy the duration_like concept.
     * @return The period as a duration.
     *
     * @note For integer duration types, be aware that the result may truncate if the
     *       exact period is not representable (e.g., gigahertz{2}.period<nanoseconds>()
     *       truncates to 0 since the true period is 0.5 ns).
     *
     * @code
     * // Integer-to-integer conversions (exact)
     * hertz{1000}.period<std::chrono::milliseconds>()   // exactly 1 ms
     * kilohertz{1}.period<std::chrono::milliseconds>()  // exactly 1 ms
     * hertz{100}.period<std::chrono::microseconds>()    // exactly 10 µs
     *
     * // Floating-point for fractional precision
     * using hertz_d = frequency<double>;
     * hertz_d{440.0}.period<std::chrono::duration<double, std::milli>>() // ~2.27 ms
     * @endcode
     */
    template<duration_like Duration>
    constexpr Duration period() const {
        // Period ratio is the inverse of frequency precision
        // For frequency<Rep, ratio<N,D>>, the period of 1 tick is ratio<D,N> seconds
        using period_ratio = std::ratio_divide<std::ratio<1>, precision>;
        using duration_period = typename Duration::period;
        using cf = std::ratio_divide<period_ratio, duration_period>;
        using duration_rep = typename Duration::rep;

        if (_r == rep(0)) {
            return Duration::max();
        }

        // Integer-only path when both types are integral
        if constexpr (std::is_integral_v<rep> && std::is_integral_v<duration_rep>) {
#ifdef __SIZEOF_INT128__
            using cr = std::common_type_t<duration_rep, rep, intmax_t>;
            using wider_t = std::conditional_t<std::is_signed_v<cr>, __int128, unsigned __int128>;
#else
            using wider_t = intmax_t;
#endif

            wider_t count = static_cast<wider_t>(_r);

            if constexpr (cf::den == 1 && cf::num == 1) {
                // period_ticks = 1 / freq_count (in the target duration units)
                return Duration(static_cast<duration_rep>(1 / count));
            } else if constexpr (cf::den == 1) {
                // period_ticks = num / freq_count
                // Use GCD: g = gcd(num, count), then (num/g) / (count/g)
                wider_t num = static_cast<wider_t>(cf::num);
                wider_t g = _runtime_gcd(num, count);
                wider_t reduced_num = num / g;
                wider_t reduced_count = count / g;
                return Duration(static_cast<duration_rep>(reduced_num / reduced_count));
            } else if constexpr (cf::num == 1) {
                // period_ticks = 1 / (freq_count * den)
                wider_t denom = count * static_cast<wider_t>(cf::den);
                return Duration(static_cast<duration_rep>(1 / denom));
            } else {
                // period_ticks = num / (freq_count * den)
                // Use GCD: g = gcd(num, count), then (num/g) / ((count/g) * den)
                wider_t num = static_cast<wider_t>(cf::num);
                wider_t g = _runtime_gcd(num, count);
                wider_t reduced_num = num / g;
                wider_t reduced_count = count / g;
                wider_t denom = reduced_count * static_cast<wider_t>(cf::den);
                return Duration(static_cast<duration_rep>(reduced_num / denom));
            }
        } else {
            // Floating-point path
            using cr = std::common_type_t<duration_rep, double>;

            if constexpr (cf::den == 1 && cf::num == 1) {
                return Duration(static_cast<duration_rep>(1.0 / static_cast<double>(_r)));
            } else if constexpr (cf::den == 1) {
                return Duration(static_cast<duration_rep>(static_cast<cr>(cf::num) / static_cast<double>(_r)));
            } else if constexpr (cf::num == 1) {
                return Duration(static_cast<duration_rep>(1.0 / (static_cast<double>(_r) * static_cast<cr>(cf::den))));
            } else {
                return Duration(
                    static_cast<duration_rep>(
                        static_cast<cr>(cf::num) / (static_cast<double>(_r) * static_cast<cr>(cf::den))
                    )
                );
            }
        }
    }

    /**
     * @brief Returns the nth harmonic of this frequency.
     *
     * The nth harmonic is n times the fundamental frequency.
     * For example, the 2nd harmonic of 440 Hz is 880 Hz.
     *
     * This method provides a semantically clear way to express harmonic
     * relationships, equivalent to `freq * n`.
     *
     * @param n The harmonic number (must be positive).
     * @return A frequency representing the nth harmonic.
     */
    constexpr frequency harmonic(unsigned int n) const { return *this * n; }

    /**
     * @brief Returns the nth subharmonic of this frequency.
     *
     * The nth subharmonic is the fundamental frequency divided by n.
     * For example, the 2nd subharmonic of 880 Hz is 440 Hz.
     *
     * This method provides a semantically clear way to express subharmonic
     * relationships, equivalent to `freq / n`.
     *
     * @param n The subharmonic divisor (must be positive).
     * @return A frequency representing the nth subharmonic.
     */
    constexpr frequency subharmonic(unsigned int n) const {
        assert(n > 0 && "subharmonic divisor must be positive");
        return *this / n;
    }

    /**
     * @brief Returns this frequency shifted by a number of octaves.
     *
     * Shifts the frequency up or down by the specified number of octaves.
     * Each octave represents a doubling (positive) or halving (negative) of frequency.
     * For example, shifting 440 Hz up by 1 octave yields 880 Hz.
     *
     * Fractional octaves are supported and particularly useful with floating-point types:
     * @code
     * // Integer types round to nearest
     * hertz a4{440};
     * auto tritone = a4.octave_shift(0.5);  // 622 Hz (rounded)
     *
     * // Floating-point types preserve fractional values
     * using hertz_d = frequency<double>;
     * hertz_d a4_d{440.0};
     * auto tritone_d = a4_d.octave_shift(0.5);  // 622.254 Hz (exact)
     * @endcode
     *
     * @tparam T The type of the octave value (default: double).
     * @param octaves The number of octaves to shift (positive = up, negative = down).
     * @return A frequency representing this frequency shifted by the specified octaves.
     */
    template<typename T = double>
    frequency octave_shift(T octaves) const {
        double multiplier = std::pow(2.0, static_cast<double>(octaves));
        if constexpr (std::is_integral_v<rep>) {
            return frequency(static_cast<rep>(std::round(static_cast<double>(_r) * multiplier)));
        } else {
            return frequency(static_cast<rep>(static_cast<double>(_r) * multiplier));
        }
    }

    /**
     * @brief Returns this frequency shifted by a number of semitones.
     *
     * Shifts the frequency up or down by the specified number of semitones.
     * There are 12 semitones per octave in equal temperament tuning.
     * For example, shifting 440 Hz up by 12 semitones yields 880 Hz (one octave).
     *
     * Fractional semitones enable precise microtonal adjustments:
     * @code
     * using hertz_d = frequency<double>;
     * hertz_d a4{440.0};
     *
     * // Equal temperament intervals
     * auto major_third = a4.semitone_shift(4);   // 554.37 Hz (C#)
     * auto perfect_fifth = a4.semitone_shift(7); // 659.26 Hz (E)
     *
     * // Microtonal adjustment (50 cents = 0.5 semitones)
     * auto slightly_sharp = a4.semitone_shift(0.5);  // 452.89 Hz
     * @endcode
     *
     * @tparam T The type of the semitone value (default: double).
     * @param semitones The number of semitones to shift (positive = up, negative = down).
     * @return A frequency representing this frequency shifted by the specified semitones.
     */
    template<typename T = double>
    frequency semitone_shift(T semitones) const {
        double multiplier = std::pow(2.0, static_cast<double>(semitones) / 12.0);
        if constexpr (std::is_integral_v<rep>) {
            return frequency(static_cast<rep>(std::round(static_cast<double>(_r) * multiplier)));
        } else {
            return frequency(static_cast<rep>(static_cast<double>(_r) * multiplier));
        }
    }

    /**
     * @brief Calculates the interval in octaves between this frequency and another.
     *
     * Returns the number of octaves between this frequency and the other frequency.
     * Positive values indicate this frequency is higher, negative values indicate lower.
     * For example, octaves_from(440 Hz) called on 880 Hz returns 1.0 (one octave higher).
     *
     * @tparam T The return type for the octave count (default: double).
     * @param other The reference frequency to compare against.
     * @return The number of octaves between the two frequencies.
     */
    template<typename T = double>
    T octaves_from(const frequency& other) const {
        return static_cast<T>(std::log2(static_cast<double>(_r) / static_cast<double>(other._r)));
    }

    /**
     * @brief Calculates the interval in semitones between this frequency and another.
     *
     * Returns the number of semitones between this frequency and the other frequency.
     * There are 12 semitones per octave in equal temperament tuning.
     * Positive values indicate this frequency is higher, negative values indicate lower.
     *
     * @tparam T The return type for the semitone count (default: double).
     * @param other The reference frequency to compare against.
     * @return The number of semitones between the two frequencies.
     */
    template<typename T = double>
    T semitones_from(const frequency& other) const {
        return static_cast<T>(12.0 * std::log2(static_cast<double>(_r) / static_cast<double>(other._r)));
    }

    /**
     * @brief Calculates the wavelength for this frequency.
     *
     * Returns the wavelength for this frequency given the time it takes for a wave to
     * travel one unit of distance. The relationship is: wavelength = period / time_per_unit_distance
     *
     * For electromagnetic waves in vacuum, use std::chrono::nanoseconds(3) for approximately
     * 1 nanosecond per 0.3 meters (since light travels ~0.3 meters per nanosecond).
     *
     * @tparam Distance The target distance type. Must satisfy the distance_like concept.
     * @tparam Duration The duration type representing time per unit distance. Must satisfy duration_like.
     * @param time_per_unit_distance The time it takes to travel one unit of the target distance.
     * @return The wavelength as a distance-like value.
     *
     * @note This method uses the period() method internally and performs ratio arithmetic
     *       to convert between time and distance domains.
     */
    template<distance_like Distance, duration_like Duration>
    constexpr Distance wavelength(const Duration& time_per_unit_distance) const {
        // Get the period of this frequency
        using period_rep = double;
        using period_ratio = std::ratio_divide<std::ratio<1>, precision>;

        if (_r == rep(0)) {
            return Distance::max();
        }

        // Calculate period in our internal representation (1/frequency)
        period_rep wave_period = 1.0 / static_cast<double>(_r);

        // Convert to common time representation
        using duration_period = typename Duration::period;
        using time_cf = std::ratio_divide<period_ratio, duration_period>;
        using time_cr = std::common_type_t<period_rep, typename Duration::rep, double>;

        // Calculate period in time_per_unit_distance units
        time_cr period_in_duration_units;
        if constexpr (time_cf::den == 1 && time_cf::num == 1) {
            period_in_duration_units = static_cast<time_cr>(wave_period);
        } else if constexpr (time_cf::den == 1) {
            period_in_duration_units = static_cast<time_cr>(wave_period) * static_cast<time_cr>(time_cf::num);
        } else if constexpr (time_cf::num == 1) {
            period_in_duration_units = static_cast<time_cr>(wave_period) / static_cast<time_cr>(time_cf::den);
        } else {
            period_in_duration_units = static_cast<time_cr>(wave_period) * static_cast<time_cr>(time_cf::num) /
                static_cast<time_cr>(time_cf::den);
        }

        // Wavelength = period / time_per_unit_distance
        double wavelength_count = period_in_duration_units / static_cast<double>(time_per_unit_distance.count());

        return Distance(static_cast<typename Distance::rep>(wavelength_count));
    }

    /**
     * @brief Calculates the wavelength for this frequency given a propagation velocity.
     *
     * Returns the wavelength for this frequency given the propagation velocity.
     * The relationship is: wavelength = velocity / frequency
     *
     * @tparam Distance The target distance type. Must satisfy the distance_like concept.
     * @param velocity The propagation velocity in meters per second.
     *                 Defaults to the speed of light (299,792,458 m/s).
     * @return The wavelength as a distance-like value.
     *
     * @note For electromagnetic waves in vacuum, use the default speed of light.
     *       For other media or wave types, specify the appropriate propagation velocity.
     *       For sound in air at 20°C, use approximately 343 m/s.
     */
    template<distance_like Distance>
    constexpr Distance wavelength(double velocity = 299792458.0) const {
        if (_r == rep(0)) {
            return Distance::max();
        }

        // Convert frequency to Hz
        double freq_hz =
            static_cast<double>(_r) * static_cast<double>(precision::num) / static_cast<double>(precision::den);

        // Calculate wavelength in meters: wavelength = velocity / frequency
        double wavelength_meters = velocity / freq_hz;

        // Convert from meters to the target distance type's units
        // Distance::period represents the ratio of the distance unit to meters
        using distance_period = typename Distance::period;
        double wavelength_in_units =
            wavelength_meters * static_cast<double>(distance_period::den) / static_cast<double>(distance_period::num);

        return Distance(static_cast<typename Distance::rep>(wavelength_in_units));
    }

private:
    rep _r{};
};

/**
 * @brief Converts a frequency to a different precision or representation.
 *
 * For integer-to-integer conversions, this function uses wider intermediate types
 * (128-bit when available) to minimize overflow risk during ratio arithmetic.
 *
 * @tparam ToFreq The target frequency type.
 * @tparam Rep     Source representation type.
 * @tparam Precision Source precision.
 * @param f        The frequency to convert.
 *
 * @return The converted frequency.
 */
template<typename ToFreq, typename Rep, typename Precision>
constexpr ToFreq frequency_cast(const frequency<Rep, Precision>& f) {
    if constexpr (std::is_same_v<ToFreq, frequency<Rep, Precision>>) {
        return f;
    } else {
        using to_rep = typename ToFreq::rep;
        using to_precision = typename ToFreq::precision;
        using cf = std::ratio_divide<Precision, to_precision>;

        // Use wider intermediate type for integer-to-integer conversions
        if constexpr (std::is_integral_v<Rep> && std::is_integral_v<to_rep>) {
#ifdef __SIZEOF_INT128__
            using cr = std::common_type_t<to_rep, Rep, intmax_t>;
            using wider_t = std::conditional_t<std::is_signed_v<cr>, __int128, unsigned __int128>;
#else
            using wider_t = intmax_t;
#endif

            if constexpr (cf::den == 1 && cf::num == 1) {
                return ToFreq(static_cast<to_rep>(f.count()));
            } else if constexpr (cf::den == 1) {
                wider_t result = static_cast<wider_t>(f.count()) * static_cast<wider_t>(cf::num);
                return ToFreq(static_cast<to_rep>(result));
            } else if constexpr (cf::num == 1) {
                wider_t result = static_cast<wider_t>(f.count()) / static_cast<wider_t>(cf::den);
                return ToFreq(static_cast<to_rep>(result));
            } else {
                // Use GCD to reduce operands: count * num / den
                // Compute g = gcd(count, den), then (count/g) * num / (den/g)
                wider_t count = static_cast<wider_t>(f.count());
                wider_t den = static_cast<wider_t>(cf::den);
                wider_t g = _runtime_gcd(count, den);
                wider_t reduced_count = count / g;
                wider_t reduced_den = den / g;
                wider_t result = reduced_count * static_cast<wider_t>(cf::num) / reduced_den;
                return ToFreq(static_cast<to_rep>(result));
            }
        } else {
            // Floating-point path
            using cr = std::common_type_t<to_rep, Rep, intmax_t>;
            if constexpr (cf::den == 1 && cf::num == 1) {
                return ToFreq(static_cast<to_rep>(f.count()));
            } else if constexpr (cf::den == 1) {
                return ToFreq(static_cast<to_rep>(static_cast<cr>(f.count()) * static_cast<cr>(cf::num)));
            } else if constexpr (cf::num == 1) {
                return ToFreq(static_cast<to_rep>(static_cast<cr>(f.count()) / static_cast<cr>(cf::den)));
            } else {
                return ToFreq(
                    static_cast<to_rep>(
                        static_cast<cr>(f.count()) * static_cast<cr>(cf::num) / static_cast<cr>(cf::den)
                    )
                );
            }
        }
    }
}

/**
 * @brief Converts a frequency to the target type, rounding toward negative infinity.
 *
 * This function performs a frequency conversion with floor rounding semantics.
 * When converting to a coarser precision, values are rounded down.
 *
 * @tparam ToFreq The target frequency type.
 * @tparam Rep     Source representation type.
 * @tparam Precision Source precision.
 * @param f        The frequency to convert.
 *
 * @return The converted frequency, rounded toward negative infinity.
 *
 * @code
 * using namespace freq;
 * hertz h{1500};
 * auto kh = floor<kilohertz>(h);  // 1 kHz (rounded down from 1.5)
 * @endcode
 */
template<typename ToFreq, typename Rep, typename Precision>
constexpr ToFreq floor(const frequency<Rep, Precision>& f) {
    using to_rep = typename ToFreq::rep;
    ToFreq result = frequency_cast<ToFreq>(f);

    if constexpr (std::is_integral_v<Rep> && std::is_integral_v<to_rep>) {
        if (result > f) {
            return ToFreq(result.count() - to_rep(1));
        }
    }

    return result;
}

/**
 * @brief Converts a frequency to the target type, rounding toward positive infinity.
 *
 * This function performs a frequency conversion with ceiling rounding semantics.
 * When converting to a coarser precision, values are rounded up.
 *
 * @tparam ToFreq The target frequency type.
 * @tparam Rep     Source representation type.
 * @tparam Precision Source precision.
 * @param f        The frequency to convert.
 *
 * @return The converted frequency, rounded toward positive infinity.
 *
 * @code
 * using namespace freq;
 * hertz h{1500};
 * auto kh = ceil<kilohertz>(h);  // 2 kHz (rounded up from 1.5)
 * @endcode
 */
template<typename ToFreq, typename Rep, typename Precision>
constexpr ToFreq ceil(const frequency<Rep, Precision>& f) {
    using to_rep = typename ToFreq::rep;
    ToFreq result = frequency_cast<ToFreq>(f);

    if constexpr (std::is_integral_v<Rep> && std::is_integral_v<to_rep>) {
        if (result < f) {
            return ToFreq(result.count() + to_rep(1));
        }
    }

    return result;
}

/**
 * @brief Converts a frequency to the target type, rounding to nearest (ties to even).
 *
 * This function performs a frequency conversion with round-to-nearest rounding semantics.
 * When converting to a coarser precision, values are rounded to the nearest representable
 * value, with ties rounded to even.
 *
 * @tparam ToFreq The target frequency type.
 * @tparam Rep     Source representation type.
 * @tparam Precision Source precision.
 * @param f        The frequency to convert.
 *
 * @return The converted frequency, rounded to nearest.
 *
 * @code
 * using namespace freq;
 * hertz h1{1500};
 * hertz h2{2500};
 * auto kh1 = round<kilohertz>(h1);  // 2 kHz (rounded to even)
 * auto kh2 = round<kilohertz>(h2);  // 2 kHz (rounded to even)
 * @endcode
 */
template<typename ToFreq, typename Rep, typename Precision>
constexpr ToFreq round(const frequency<Rep, Precision>& f) {
    using to_rep = typename ToFreq::rep;

    if constexpr (std::is_integral_v<Rep> && std::is_integral_v<to_rep>) {
        ToFreq result = frequency_cast<ToFreq>(f);
        ToFreq lower = floor<ToFreq>(f);
        ToFreq upper = lower + ToFreq(to_rep(1));

        auto diff_lower = f - lower;
        auto diff_upper = upper - f;

        if (diff_lower < diff_upper) {
            return lower;
        } else if (diff_lower > diff_upper) {
            return upper;
        } else {
            // Tie: round to even
            return (lower.count() % to_rep(2) == to_rep(0)) ? lower : upper;
        }
    } else {
        return frequency_cast<ToFreq>(f);
    }
}

/**
 * @brief Calculates the beat frequency between two frequencies.
 *
 * In acoustics and signal processing, the beat frequency is the absolute
 * difference between two frequencies. When two sound waves of slightly
 * different frequencies interfere, they produce a beating pattern at this
 * frequency.
 *
 * This is a convenience function equivalent to abs(f1 - f2).
 *
 * @tparam Rep1 First frequency representation type.
 * @tparam Precision1 First frequency precision.
 * @tparam Rep2 Second frequency representation type.
 * @tparam Precision2 Second frequency precision.
 * @param f1 The first frequency.
 * @param f2 The second frequency.
 * @return The beat frequency as the absolute difference.
 *
 * @code
 * using namespace freq;
 * using namespace frequency_literals;
 *
 * // Acoustic beats between two tuning forks
 * auto f1 = 440_Hz;  // A4 concert pitch
 * auto f2 = 442_Hz;  // Slightly sharp A4
 * auto beat = beat(f1, f2);  // 2 Hz beat
 *
 * // Mixed precisions handled automatically
 * auto carrier = 1000_Hz;
 * auto modulator = 1_kHz + 10_Hz;
 * auto beat2 = beat(carrier, modulator);  // 10 Hz
 * @endcode
 *
 * @see abs(), operator-()
 */
template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
constexpr auto beat(const frequency<Rep1, Precision1>& f1, const frequency<Rep2, Precision2>& f2)
    -> std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>> {
    using cf = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return abs(cf(f1) - cf(f2));
}

/**
 * @brief Returns the absolute value of a frequency.
 *
 * @tparam Rep       Representation type.
 * @tparam Precision Precision type.
 * @param f          The frequency.
 *
 * @return The absolute value of the frequency.
 *
 * @code
 * using namespace freq;
 * hertz h{-440};
 * auto abs_h = abs(h);  // 440 Hz
 * @endcode
 */
template<typename Rep, typename Precision>
constexpr frequency<Rep, Precision> abs(const frequency<Rep, Precision>& f) {
    return f >= frequency<Rep, Precision>::zero() ? f : -f;
}

/** @brief Returns the sum of two frequencies. */
template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
constexpr auto operator+(const frequency<Rep1, Precision1>& lhs, const frequency<Rep2, Precision2>& rhs)
    -> std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>> {
    using cf = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return cf(cf(lhs).count() + cf(rhs).count());
}

/** @brief Returns the difference of two frequencies. */
template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
constexpr auto operator-(const frequency<Rep1, Precision1>& lhs, const frequency<Rep2, Precision2>& rhs)
    -> std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>> {
    using cf = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return cf(cf(lhs).count() - cf(rhs).count());
}

/** @brief Multiplies a frequency by a scalar. */
template<typename Rep1, typename Precision, typename Rep2>
    requires not_frequency<Rep2> && std::convertible_to<const Rep2&, std::common_type_t<Rep1, Rep2>>
constexpr auto operator*(const frequency<Rep1, Precision>& f, const Rep2& r)
    -> frequency<std::common_type_t<Rep1, Rep2>, Precision> {
    using cf = frequency<std::common_type_t<Rep1, Rep2>, Precision>;
    return cf(cf(f).count() * r);
}

/** @brief Multiplies a scalar by a frequency. */
template<typename Rep1, typename Rep2, typename Precision>
    requires not_frequency<Rep1> && std::convertible_to<const Rep1&, std::common_type_t<Rep1, Rep2>>
constexpr auto operator*(const Rep1& r, const frequency<Rep2, Precision>& f)
    -> frequency<std::common_type_t<Rep1, Rep2>, Precision> {
    return f * r;
}

/** @brief Divides a frequency by a scalar. */
template<typename Rep1, typename Precision, typename Rep2>
    requires not_frequency<Rep2> && std::convertible_to<const Rep2&, std::common_type_t<Rep1, Rep2>>
constexpr auto operator/(const frequency<Rep1, Precision>& f, const Rep2& s)
    -> frequency<std::common_type_t<Rep1, Rep2>, Precision> {
    using cf = frequency<std::common_type_t<Rep1, Rep2>, Precision>;
    return cf(cf(f).count() / s);
}

/** @brief Divides two frequencies, returning a scalar. */
template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
constexpr auto operator/(const frequency<Rep1, Precision1>& lhs, const frequency<Rep2, Precision2>& rhs)
    -> std::common_type_t<Rep1, Rep2> {
    using cf = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return cf(lhs).count() / cf(rhs).count();
}

/** @brief Returns the remainder of dividing a frequency by a scalar. */
template<typename Rep1, typename Precision, typename Rep2>
    requires not_frequency<Rep2> && std::convertible_to<const Rep2&, std::common_type_t<Rep1, Rep2>> &&
    (!treat_as_inexact_v<Rep1> && !treat_as_inexact_v<Rep2>)
constexpr auto operator%(const frequency<Rep1, Precision>& f, const Rep2& s)
    -> frequency<std::common_type_t<Rep1, Rep2>, Precision> {
    using cf = frequency<std::common_type_t<Rep1, Rep2>, Precision>;
    return cf(cf(f).count() % s);
}

/** @brief Returns the remainder of dividing two frequencies. */
template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
    requires(!treat_as_inexact_v<Rep1> && !treat_as_inexact_v<Rep2>)
constexpr auto operator%(const frequency<Rep1, Precision1>& lhs, const frequency<Rep2, Precision2>& rhs)
    -> std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>> {
    using cf = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return cf(cf(lhs).count() % cf(rhs).count());
}

template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
constexpr bool operator==(const frequency<Rep1, Precision1>& lhs, const frequency<Rep2, Precision2>& rhs) {
    using ct = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return ct(lhs).count() == ct(rhs).count();
}

template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
    requires std::three_way_comparable<std::common_type_t<Rep1, Rep2>>
constexpr auto operator<=>(const frequency<Rep1, Precision1>& lhs, const frequency<Rep2, Precision2>& rhs) {
    using ct = std::common_type_t<frequency<Rep1, Precision1>, frequency<Rep2, Precision2>>;
    return ct(lhs).count() <=> ct(rhs).count();
}

/**
 * @defgroup FrequencyTypes Standard Frequency Type Aliases
 * @{
 *
 * Standard integer-based frequency types using int64_t representation.
 * For floating-point representations, use frequency<double, Precision>.
 *
 * @code
 * // Integer types (default)
 * hertz audio{44100};
 * kilohertz cpu{3200};
 * megahertz fm{88};
 * gigahertz wifi{2};
 * terahertz optical{193};  // Near-infrared light
 *
 * // Floating-point types (custom)
 * using hertz_d = frequency<double>;
 * using kilohertz_d = frequency<double, std::kilo>;
 * using megahertz_d = frequency<double, std::mega>;
 *
 * hertz_d concert_a{440.0};
 * megahertz_d fm_precise{88.5};
 * @endcode
 */

/** @brief Frequency with 0.001 Hz (millihertz) precision. */
using millihertz = frequency<int64_t, std::milli>;
/** @brief Frequency with 1 Hz precision. */
using hertz = frequency<int64_t>;
/** @brief Frequency with 1000 Hz (kilohertz) precision. */
using kilohertz = frequency<int64_t, std::kilo>;
/** @brief Frequency with 1,000,000 Hz (megahertz) precision. */
using megahertz = frequency<int64_t, std::mega>;
/** @brief Frequency with 1,000,000,000 Hz (gigahertz) precision. */
using gigahertz = frequency<int64_t, std::giga>;
/** @brief Frequency with 1,000,000,000,000 Hz (terahertz) precision. */
using terahertz = frequency<int64_t, std::tera>;

/** @} */ // end of FrequencyTypes group

inline std::string to_string(millihertz f) {
    return std::to_string(f.count()) + "mHz";
}

inline std::string to_string(hertz f) {
    return std::to_string(f.count()) + "Hz";
}

inline std::string to_string(kilohertz f) {
    return std::to_string(f.count()) + "kHz";
}

inline std::string to_string(megahertz f) {
    return std::to_string(f.count()) + "MHz";
}

inline std::string to_string(gigahertz f) {
    return std::to_string(f.count()) + "GHz";
}

inline std::string to_string(terahertz f) {
    return std::to_string(f.count()) + "THz";
}

} // namespace freq

namespace std {

template<typename Rep1, typename Precision1, typename Rep2, typename Precision2>
struct common_type<freq::frequency<Rep1, Precision1>, freq::frequency<Rep2, Precision2>> {
private:
    using common_precision = std::ratio<
        freq::_gcd(Precision1::num, Precision2::num),
        (Precision1::den / freq::_gcd(Precision1::den, Precision2::den)) * Precision2::den>;

public:
    using type = freq::frequency<std::common_type_t<Rep1, Rep2>, common_precision>;
};

#if __has_include(<format>) && defined(__cpp_lib_format)
template<>
struct formatter<freq::millihertz> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    auto format(freq::millihertz f, format_context& ctx) const { return std::format_to(ctx.out(), "{}mHz", f.count()); }
};

template<>
struct formatter<freq::hertz> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    auto format(freq::hertz f, format_context& ctx) const { return std::format_to(ctx.out(), "{}Hz", f.count()); }
};

template<>
struct formatter<freq::kilohertz> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    auto format(freq::kilohertz f, format_context& ctx) const { return std::format_to(ctx.out(), "{}kHz", f.count()); }
};

template<>
struct formatter<freq::megahertz> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    auto format(freq::megahertz f, format_context& ctx) const { return std::format_to(ctx.out(), "{}MHz", f.count()); }
};

template<>
struct formatter<freq::gigahertz> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    auto format(freq::gigahertz f, format_context& ctx) const { return std::format_to(ctx.out(), "{}GHz", f.count()); }
};

template<>
struct formatter<freq::terahertz> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

    auto format(freq::terahertz f, format_context& ctx) const { return std::format_to(ctx.out(), "{}THz", f.count()); }
};
#endif

} // namespace std

/**
 * @brief User-defined literals for frequency types.
 */
namespace frequency_literals {
/** @cond INTERNAL */
namespace detail {

template<unsigned long long Value, unsigned long long Power>
struct pow10 {
    static constexpr unsigned long long value = 10 * pow10<Value, Power - 1>::value;
};

template<unsigned long long Value>
struct pow10<Value, 0> {
    static constexpr unsigned long long value = Value;
};

template<char... Digits>
struct parse_int;

template<char D, char... Rest>
struct parse_int<D, Rest...> {
    static_assert(D >= '0' && D <= '9', "invalid digit");
    static constexpr unsigned long long value = pow10<D - '0', sizeof...(Rest)>::value + parse_int<Rest...>::value;
};

template<char D>
struct parse_int<D> {
    static_assert(D >= '0' && D <= '9', "invalid digit");
    static constexpr unsigned long long value = D - '0';
};

template<typename Freq, char... Digits>
constexpr Freq check_overflow() {
    using parsed = parse_int<Digits...>;
    constexpr typename Freq::rep repval = parsed::value;
    static_assert(
        repval >= 0 && static_cast<unsigned long long>(repval) == parsed::value,
        "literal value cannot be represented by frequency type"
    );
    return Freq(repval);
}

} // namespace detail
/** @endcond */

/** @brief Literal for millihertz (e.g., 1000_mHz). */
template<char... Digits>
constexpr freq::millihertz operator""_mHz() {
    return detail::check_overflow<freq::millihertz, Digits...>();
}

/** @brief Literal for hertz (e.g., 1000_Hz). */
template<char... Digits>
constexpr freq::hertz operator""_Hz() {
    return detail::check_overflow<freq::hertz, Digits...>();
}

/** @brief Literal for kilohertz (e.g., 80_kHz). */
template<char... Digits>
constexpr freq::kilohertz operator""_kHz() {
    return detail::check_overflow<freq::kilohertz, Digits...>();
}

/** @brief Literal for megahertz (e.g., 80_MHz). */
template<char... Digits>
constexpr freq::megahertz operator""_MHz() {
    return detail::check_overflow<freq::megahertz, Digits...>();
}

/** @brief Literal for gigahertz (e.g., 2_GHz). */
template<char... Digits>
constexpr freq::gigahertz operator""_GHz() {
    return detail::check_overflow<freq::gigahertz, Digits...>();
}

/** @brief Literal for terahertz (e.g., 1_THz). */
template<char... Digits>
constexpr freq::terahertz operator""_THz() {
    return detail::check_overflow<freq::terahertz, Digits...>();
}

} // namespace frequency_literals
