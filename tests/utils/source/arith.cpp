
#include <wolv/test/tests.hpp>

#include <wolv/utils/arith.hpp>

#include <wolv/types.hpp>

using namespace wolv::util;
using wolv::u32, wolv::i32;

namespace {
    template<class T>
    constexpr T Max = std::numeric_limits<T>::max();
    template<class T>
    constexpr T Min = std::numeric_limits<T>::min();
}

TEST_SEQUENCE("Arith_Checked") {
    TEST_ASSERT(checked_add<u32>(Max<u32>, 0) == Max<u32>);
    TEST_ASSERT(checked_add<u32>(Max<u32>, 1) == std::nullopt);
    TEST_ASSERT(checked_add<i32>(Max<i32>, 1) == std::nullopt);
    TEST_ASSERT(checked_add<i32>(Min<i32>, -1) == std::nullopt);
    TEST_ASSERT(checked_add<i32>(Min<i32>, -1) == std::nullopt);

    TEST_ASSERT(checked_sub<u32>(0, 0) == 0);
    TEST_ASSERT(checked_sub<u32>(0, 1) == std::nullopt);
    TEST_ASSERT(checked_sub<i32>(Min<i32>, 1) == std::nullopt);
    TEST_ASSERT(checked_sub<i32>(Max<i32>, -1) == std::nullopt);

    TEST_ASSERT(checked_mul<u32>(0, Max<u32>) == 0);
    TEST_ASSERT(checked_mul<u32>(Max<u32>, 1) == Max<u32>);
    TEST_ASSERT(checked_mul<u32>(Max<u32>, 2) == std::nullopt);
    TEST_ASSERT(checked_mul<i32>(-2, -3) == 6);
    TEST_ASSERT(checked_mul<i32>(Max<i32>, 2) == std::nullopt);
    TEST_ASSERT(checked_mul<i32>(Min<i32>, 2) == std::nullopt);
    TEST_ASSERT(checked_mul<i32>(Min<i32>, -1) == std::nullopt);
    TEST_ASSERT(checked_mul<i32>(0, 7) == 0);
    TEST_ASSERT(checked_mul<i32>(7, 0) == 0);

    TEST_SUCCESS();
};

TEST_SEQUENCE("Arith_Saturating") {
    TEST_ASSERT(saturating_add<u32>(Max<u32>, 1) == Max<u32>);
    TEST_ASSERT(saturating_add<u32>(3, 4) == 7);
    TEST_ASSERT(saturating_add<i32>(Max<i32>, 1) == Max<i32>);
    TEST_ASSERT(saturating_add<i32>(Min<i32>, -1) == Min<i32>);
    TEST_ASSERT(saturating_add<i32>(Max<i32>, Min<i32>) == -1);
    TEST_ASSERT(saturating_add<i32>(1, 2) == 3);

    TEST_ASSERT(saturating_sub<u32>(0, 1) == 0);
    TEST_ASSERT(saturating_sub<u32>(10, 3) == 7);
    TEST_ASSERT(saturating_sub<i32>(Min<i32>, 1) == Min<i32>);
    TEST_ASSERT(saturating_sub<i32>(Max<i32>, -1) == Max<i32>);
    TEST_ASSERT(saturating_sub<i32>(Min<i32>, Min<i32>) == 0);
    TEST_ASSERT(saturating_sub<i32>(5, 3) == 2);

    TEST_ASSERT(saturating_mul<u32>(Max<u32>, 2) == Max<u32>);
    TEST_ASSERT(saturating_mul<u32>(5, 8) == 40);
    TEST_ASSERT(saturating_mul<i32>(Max<i32>, 2) == Max<i32>);
    TEST_ASSERT(saturating_mul<i32>(Max<i32>, -2) == Min<i32>);
    TEST_ASSERT(saturating_mul<i32>(Min<i32>, 2) == Min<i32>);
    TEST_ASSERT(saturating_mul<i32>(Min<i32>, -1) == Max<i32>);
    TEST_ASSERT(saturating_mul<i32>(3, 4) == 12);

    TEST_ASSERT(saturating_div<u32>(7, 2) == 3);
    TEST_ASSERT(saturating_div<u32>(Max<u32>, 1) == Max<u32>);
    TEST_ASSERT(saturating_div<i32>(-7, 2) == -3);
    TEST_ASSERT(saturating_div<i32>(Min<i32>, -1) == Max<i32>);

    TEST_ASSERT(saturating_div<u32>(7, 0) == Max<u32>);
    TEST_ASSERT(saturating_div<i32>(7, 0) == Max<i32>);
    TEST_ASSERT(saturating_div<i32>(-7, 0) == Min<i32>);

    TEST_SUCCESS();
};
