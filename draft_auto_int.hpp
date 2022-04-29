#pragma once

#include <cstdlib>
#include <limits>
#include <type_traits>

#include <boost/integer.hpp>
#include <boost/multiprecision/cpp_int.hpp>

namespace r7
{
    using f32 = float;
    using f63 = double;

    using i8 = boost::int_t<8>::exact;
    using i16 = boost::int_t<16>::exact;
    using i32 = boost::int_t<32>::exact;
    using i64 = boost::int_t<64>::exact;
    using i128 = boost::multiprecision::int128_t;
    using i256 = boost::multiprecision::int256_t;
    using i512 = boost::multiprecision::int512_t;
    using i1024 = boost::multiprecision::int1024_t;

    using u8 = boost::uint_t<8>::exact;
    using u16 = boost::uint_t<16>::exact;
    using u32 = boost::uint_t<32>::exact;
    using u64 = boost::uint_t<64>::exact;
    using u128 = boost::multiprecision::uint128_t;
    using u256 = boost::multiprecision::uint256_t;
    using u512 = boost::multiprecision::uint512_t;
    using u1024 = boost::multiprecision::uint1024_t;

    // Convinient wrappers for boost::multiprecision numbers.
    template <std::size_t Bits, boost::multiprecision::cpp_integer_type SignType>
    using __LongInteger = boost::multiprecision::number<
        boost::multiprecision::cpp_int_backend<
            Bits,
            Bits,
            SignType,
            boost::multiprecision::unchecked,
            void
        >
    >;
    template <std::size_t Bits>
    using LongSignedInteger = __LongInteger<Bits, boost::multiprecision::signed_magnitude>;
    template <std::size_t Bits>
    using LongUnsignedInteger = __LongInteger<Bits, boost::multiprecision::unsigned_magnitude>;

    // MakeSigned.
    template <typename T> struct MakeSigned final { using Type = std::make_signed_t<T>; };
    template <std::size_t Bits, boost::multiprecision::cpp_integer_type SignType>
    struct MakeSigned<__LongInteger<Bits, SignType>> final
    {
        using Type = LongSignedInteger<Bits>;
    };
    template <typename T> using MakeSignedT = typename MakeSigned<T>::Type;
    static_assert(std::is_same_v<MakeSignedT<int>, int>);
    static_assert(std::is_same_v<MakeSignedT<unsigned int>, int>);
    static_assert(std::is_same_v<MakeSignedT<boost::multiprecision::int256_t>, boost::multiprecision::int256_t>);
    static_assert(std::is_same_v<MakeSignedT<boost::multiprecision::uint256_t>, boost::multiprecision::int256_t>);

    // MakeUnsigned.
    template <typename T> struct MakeUnsigned final { using Type = std::make_unsigned_t<T>; };
    template <std::size_t Bits, boost::multiprecision::cpp_integer_type SignType>
    struct MakeUnsigned<__LongInteger<Bits, SignType>> final
    {
        using Type = LongUnsignedInteger<Bits>;
    };
    template <typename T> using MakeUnsignedT = typename MakeUnsigned<T>::Type;
    static_assert(std::is_same_v<MakeUnsignedT<int>, unsigned int>);
    static_assert(std::is_same_v<MakeUnsignedT<unsigned int>, unsigned int>);
    static_assert(std::is_same_v<MakeUnsignedT<boost::multiprecision::int256_t>, boost::multiprecision::uint256_t>);
    static_assert(std::is_same_v<MakeUnsignedT<boost::multiprecision::uint256_t>, boost::multiprecision::uint256_t>);

    // ExtendedInteger.
    template <typename T> struct ExtendedInteger final { using Type = void; };
    template <> struct ExtendedInteger<i8> final { using Type = i16; };
    template <> struct ExtendedInteger<i16> final { using Type = i32; };
    template <> struct ExtendedInteger<i32> final { using Type = i64; };
    template <> struct ExtendedInteger<i64> final { using Type = i128; };
    template <> struct ExtendedInteger<u8> final { using Type = u16; };
    template <> struct ExtendedInteger<u16> final { using Type = u32; };
    template <> struct ExtendedInteger<u32> final { using Type = u64; };
    template <> struct ExtendedInteger<u64> final { using Type = u128; };
    template <std::size_t Bits, boost::multiprecision::cpp_integer_type SignType>
    struct ExtendedInteger<__LongInteger<Bits, SignType>> final
    {
        using Type =__LongInteger<2 * Bits, SignType>;
    };
    template <typename T> using ExtendedIntegerT = typename ExtendedInteger<T>::Type;
    static_assert(std::is_same_v<ExtendedIntegerT<LongSignedInteger<128>>, LongSignedInteger<256>>);
    static_assert(std::is_same_v<ExtendedIntegerT<LongUnsignedInteger<128>>, LongUnsignedInteger<256>>);

    // ExtendedSignedInteger.
    template <typename T> using ExtendedSignedIntegerT = ExtendedIntegerT<MakeSignedT<T>>;
    static_assert(std::is_same_v<ExtendedSignedIntegerT<LongSignedInteger<128>>, LongSignedInteger<256>>);
    static_assert(std::is_same_v<ExtendedSignedIntegerT<LongUnsignedInteger<128>>, LongSignedInteger<256>>);

    // ExtendedUnsignedInteger.
    template <typename T> using ExtendedUnsignedIntegerT = ExtendedIntegerT<MakeUnsignedT<T>>;
    static_assert(std::is_same_v<ExtendedUnsignedIntegerT<LongSignedInteger<128>>, LongUnsignedInteger<256>>);
    static_assert(std::is_same_v<ExtendedUnsignedIntegerT<LongUnsignedInteger<128>>, LongUnsignedInteger<256>>);

    // Check that integer type is extendable.
    template <typename T> constexpr bool extendable_integer = !std::is_same_v<ExtendedIntegerT<T>, void>;
    static_assert(extendable_integer<LongSignedInteger<2048>>);

    template <typename DestinationType, typename SourceType>
    [[nodiscard]] constexpr bool can_be_casted(const SourceType & value) noexcept
    {
        constexpr auto min = std::numeric_limits<DestinationType>::min();
        constexpr auto max = std::numeric_limits<DestinationType>::max();
        return min <= value && value <= max;
    }

    template <typename T>
    [[nodiscard]] constexpr auto extend_cast(const T & value) noexcept
    {
        static_assert(extendable_integer<T>, "Type is not extendable");
        return static_cast<ExtendedIntegerT<T>>(value);
    }

    template <typename T>
    [[nodiscard]] constexpr auto extend_signed_cast(const T & value)
    {
        return static_cast<ExtendedSignedIntegerT<T>>(extend_cast(value));
    }

    template <typename T>
    [[nodiscard]] constexpr auto sum_extended(const T & a, const T & b) noexcept
    {
        return extend_cast(a) + extend_cast(b);
    }

    template <typename T>
    [[nodiscard]] constexpr auto difference_extended(const T & a, const T & b) noexcept
    {
        return extend_signed_cast(a) - extend_signed_cast(b);
    }

    template <typename T>
    [[nodiscard]] constexpr auto multiply_extended(const T & a, const T & b) noexcept
    {
        return extend_cast(a) * extend_cast(b);
    }

    template <typename T>
    [[nodiscard]] constexpr auto square_extended(const T & a) noexcept
    {
        return static_cast<ExtendedUnsignedIntegerT<T>>(multiply_extended(a, a));
    }

    template <i64 min_value, i64 max_value>
    struct __TestRange
    {
        constexpr static auto min = min_value;
        constexpr static auto max = max_value;
    };
    template <i64 min, i64 max>
    struct __TestStructWithRange
    {
        using ValueRange = __TestRange<min, max>;
    };

    template <typename Range>
    struct __IntegerForRange
    {
        static_assert(Range::min < Range::max);
        static_assert(Range::max > 0);

        template <typename CandidateType, typename = void>
        struct FindInteger final
        {
            using Type = typename FindInteger<ExtendedIntegerT<CandidateType>>::Type;
        };
        template <typename CandidateType>
        struct FindInteger<
            CandidateType,
            std::enable_if_t<can_be_casted<CandidateType>(Range::min) && can_be_casted<CandidateType>(Range::max)>
        > final
        {
            using Type = CandidateType;
        };

        using Type = typename FindInteger<std::conditional_t<Range::min < 0, i8, u8>>::Type;
    };
    template <typename Range> using IntegerForRangeT = typename __IntegerForRange<Range>::Type;
    static_assert(std::is_same_v<i8, IntegerForRangeT<__TestRange<-10, 10>>>);
    static_assert(std::is_same_v<u8, IntegerForRangeT<__TestRange<10, 100>>>);
    static_assert(std::is_same_v<i16, IntegerForRangeT<__TestRange<-129, 10>>>);
    static_assert(std::is_same_v<u16, IntegerForRangeT<__TestRange<10, 256>>>);

    // RangeOf.
    template <typename T, typename = void>
    struct RangeOf final
    {
        constexpr static auto min = std::numeric_limits<T>::min();
        constexpr static auto max = std::numeric_limits<T>::max();
    };
    // TODO: use C++20.
    template <typename T>
    struct RangeOf<T, std::void_t<typename T::ValueRange>> final
    {
        constexpr static auto min = T::ValueRange::min;
        constexpr static auto max = T::ValueRange::max;
    };
    static_assert(RangeOf<i8>::min == -128);
    static_assert(RangeOf<i8>::max == 127);
    static_assert(RangeOf<u8>::min == 0);
    static_assert(RangeOf<u8>::max == 255);
    static_assert(RangeOf<__TestStructWithRange<-100, -23>>::min == -100);
    static_assert(RangeOf<__TestStructWithRange<-100, -23>>::max == -23);
    static_assert(RangeOf<__TestStructWithRange<10, 23>>::min == 10);
    static_assert(RangeOf<__TestStructWithRange<10, 23>>::max == 23);

    // ExtendedCommonType.
    template <
        typename Lhs,
        typename Rhs,
        typename LhsSigned = ExtendedSignedIntegerT<Lhs>,
        typename RhsSigned = ExtendedSignedIntegerT<Rhs>,
        bool CanCastRhsToLhs =
            can_be_casted<LhsSigned>(std::numeric_limits<RhsSigned>::min())
            && can_be_casted<LhsSigned>(std::numeric_limits<RhsSigned>::max()),
        typename CommonType = std::conditional_t<CanCastRhsToLhs, LhsSigned, RhsSigned>
    >
    using __ExtendedCommonType = ExtendedIntegerT<CommonType>;
    template <typename Lhs, typename Rhs> using ExtendedCommonType = __ExtendedCommonType<Lhs, Rhs>;
    static_assert(std::is_same_v<i64, ExtendedCommonType<u16, u8>>);
    static_assert(std::is_same_v<i64, ExtendedCommonType<u8, u16>>);

    namespace operations
    {
        struct Sum final
        {
            template <typename T>
            [[nodiscard]] constexpr static auto execute(const T & lhs, const T & rhs)
            {
                return lhs + rhs;
            }
        };

        struct Difference final
        {
            template <typename T>
            [[nodiscard]] constexpr static auto execute(const T & lhs, const T & rhs)
            {
                return lhs - rhs;
            }
        };

        struct Product final
        {
            template <typename T>
            [[nodiscard]] constexpr static auto execute(const T & lhs, const T & rhs)
            {
                return lhs * rhs;
            }
        };
    }

    // Operation executiton.
    template <typename Operation, typename T, typename Lhs, typename Rhs>
    [[nodiscard]] constexpr static auto execute(const Lhs & lhs, const Rhs & rhs)
    {
        return Operation::execute(static_cast<T>(lhs), static_cast<T>(rhs));
    }
    // TODO: C++20 consteval.
    template <typename Operation, typename Lhs, typename Rhs>
    [[nodiscard]] constexpr static auto execute_in_extended_common_type(const Lhs & lhs, const Rhs & rhs)
    {
        return execute<Operation, ExtendedCommonType<Lhs, Rhs>>(lhs, rhs);
    }

    template <typename Operation, typename Lhs, typename Rhs>
    struct OperationOutputType final
    {
        using ValueRange = typename Operation::template OutputRange<RangeOf<Lhs>, RangeOf<Rhs>>;
        using ValueType = IntegerForRangeT<ValueRange>;

        ValueType value;
    };
    template <typename T, typename = void> struct HasValue final : std::false_type {};
    template <typename T> struct HasValue<T, std::void_t<decltype(T::value)>> final : std::true_type {};
    template <typename T>
    [[nodiscard]] constexpr auto value_of(const T & value) noexcept
    {
        if constexpr(HasValue<T>::value)
        {
            return value.value;
        }
        else
        {
            return value;
        }
    }

    template <typename Operation, typename Lhs, typename Rhs>
    [[nodiscard]] constexpr auto auto_int(const Lhs & lhs, const Rhs & rhs) noexcept
    {
        using OutputType = OperationOutputType<Operation, Lhs, Rhs>;
        return OutputType { execute<typename Operation::Operation, typename OutputType::ValueType>(value_of(lhs), value_of(rhs)) };
    }

    template <typename OperationType>
    struct __BinaryOperationWithBruteforcedRange final
    {
        using Operation = OperationType;

        template <typename LhsRange, typename RhsRange>
        struct OutputRange final
        {
            constexpr static auto min = std::min(
                std::min(
                    execute_in_extended_common_type<Operation>(LhsRange::min, RhsRange::min),
                    execute_in_extended_common_type<Operation>(LhsRange::min, RhsRange::max)),
                std::min(
                    execute_in_extended_common_type<Operation>(LhsRange::max, RhsRange::min),
                    execute_in_extended_common_type<Operation>(LhsRange::max, RhsRange::max)));
            constexpr static auto max = std::max(
                std::max(
                    execute_in_extended_common_type<Operation>(LhsRange::min, RhsRange::min),
                    execute_in_extended_common_type<Operation>(LhsRange::min, RhsRange::max)),
                std::max(
                    execute_in_extended_common_type<Operation>(LhsRange::max, RhsRange::min),
                    execute_in_extended_common_type<Operation>(LhsRange::max, RhsRange::max)));
        };
    };

    using Sum = __BinaryOperationWithBruteforcedRange<operations::Sum>;
    using Difference = __BinaryOperationWithBruteforcedRange<operations::Difference>;
    using Product = __BinaryOperationWithBruteforcedRange<operations::Product>;
    
    static_assert(auto_int<Product>(30, 10).value == 300);
    static_assert(std::is_same_v<decltype(auto_int<Product>(30, 10).value), i64>);

    static_assert(std::is_same_v<
        decltype(auto_int<Sum>(auto_int<Product, i32, i32>(1, 2), auto_int<Product, i32, i32>(3, 4)).value),
        i128
    >);

    static_assert(std::is_same_v<
        decltype(
            auto_int<Sum>(
                auto_int<Product>(auto_int<Difference, i32, i32>(1, 2), auto_int<Difference, i32, i32>(3, 4)),
                auto_int<Product>(auto_int<Difference, i32, i32>(5, 6), auto_int<Difference, i32, i32>(7, 8))).value),
        i128
    >);
}
