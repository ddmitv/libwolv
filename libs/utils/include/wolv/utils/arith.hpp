#pragma once

#include <concepts>
#include <limits>
#include <optional>
#include <type_traits>

namespace wolv::util {

    template<std::integral T>
    [[nodiscard]] constexpr std::optional<T> checked_add(const T a, const T b) noexcept {
        if (b >= 0 && a > std::numeric_limits<T>::max() - b) { return std::nullopt; }
        if (b < 0 && a < std::numeric_limits<T>::min() - b) { return std::nullopt; }
        return static_cast<T>(a + b);
    }

    template<std::integral T>
    [[nodiscard]] constexpr std::optional<T> checked_sub(const T a, const T b) noexcept {
        if (b >= 0 && a < std::numeric_limits<T>::min() + b) { return std::nullopt; }
        if (b < 0 && a > std::numeric_limits<T>::max() + b) { return std::nullopt; }
        return static_cast<T>(a - b);
    }

    template<std::integral T>
    [[nodiscard]] constexpr std::optional<T> checked_mul(const T a, const T b) noexcept {
        if (a == 0 || b == 0) { return T{0}; }

        if constexpr (std::is_unsigned_v<T>) {
            if (a > std::numeric_limits<T>::max() / b) { return std::nullopt; }
            return static_cast<T>(a * b);
        } else {
            constexpr T limitMax = std::numeric_limits<T>::max();
            constexpr T limitMin = std::numeric_limits<T>::min();

            if (a > 0) {
                if (b > 0) {
                    if (a > limitMax / b) { return std::nullopt; }
                } else {
                    if (b < limitMin / a) { return std::nullopt; }
                }
            } else {
                if (b > 0) {
                    if (a < limitMin / b) { return std::nullopt; }
                } else {
                    if (b < limitMax / a) { return std::nullopt; }
                }
            }
            return static_cast<T>(a * b);
        }
    }
    // reference implementation of saturating_{add,sub,mul,div} - https://github.com/llvm/llvm-project/blob/main/libcxx/include/__numeric/saturation_arithmetic.h
    template <std::integral T>
    [[nodiscard]] constexpr T saturating_add(const T a, const T b) noexcept {
        if (const auto sum = checked_add(a, b)) {
            return *sum;
        }
        if constexpr (std::is_unsigned_v<T>) {
            return std::numeric_limits<T>::max();
        } else {
            return a > 0 ? std::numeric_limits<T>::max() : std::numeric_limits<T>::min();
        }
    }
    template <std::integral T>
    [[nodiscard]] constexpr T saturating_sub(const T a, const T b) noexcept {
        if (const auto sub = checked_sub(a, b)) {
            return *sub;
        }
        if constexpr (std::is_unsigned_v<T>) {
            return std::numeric_limits<T>::min();
        } else {
            return a >= 0 ? std::numeric_limits<T>::max() : std::numeric_limits<T>::min();
        }
    }
    template <std::integral T>
    [[nodiscard]] constexpr T saturating_mul(const T a, const T b) noexcept {
        if (const auto mul = checked_mul(a, b)) {
            return *mul;
        }
        if constexpr (std::is_unsigned_v<T>) {
            return std::numeric_limits<T>::max();
        } else {
            return (a > 0 && b > 0) || (a < 0 && b < 0) ? std::numeric_limits<T>::max() : std::numeric_limits<T>::min();
        }
    }
    template <std::integral T>
    [[nodiscard]] constexpr T saturating_div(const T a, const T b) noexcept {
        // note: division by zero saturates to +max (or -min if numerator (a) < 0)
        if constexpr (std::is_unsigned_v<T>) {
            if (b == 0) {
                return std::numeric_limits<T>::max();
            }
            return a / b;
        } else {
            if (b == 0) {
                return a < 0 ? std::numeric_limits<T>::min() : std::numeric_limits<T>::max();
            }
            if (a == std::numeric_limits<T>::min() && b == T{-1}) {
                return std::numeric_limits<T>::max();
            }
            return a / b;
        }
    }

}
