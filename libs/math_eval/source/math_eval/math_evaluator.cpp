#include <wolv/math_eval/math_evaluator.hpp>

#include <wolv/utils/core.hpp>

#include <bit>
#include <charconv>
#include <string>
#include <queue>
#include <stack>
#include <cmath>
#include <optional>
#include <numbers>
#include <concepts>
#include <algorithm>
#include <cctype>
#include <limits>
#include <string_view>

namespace
{
    using wolv::u8;

    template<typename T, typename U>
    [[nodiscard]] auto powi(T base, U exp) {
        using ResultType = decltype(T{} * U{});

        if (exp < 0)
            return ResultType(0);

        ResultType result = 1;

        while (exp != 0) {
            if ((exp & 0b1) == 0b1)
                result *= base;
            exp >>= 1;
            base *= base;
        }
        return result;
    }

    // could be in wolv/utils/string.hpp
    template<typename T = char>
    [[nodiscard]] constexpr std::basic_string_view<T> trim_ascii_start(std::basic_string_view<T> s) noexcept {
        s.remove_prefix(std::min(s.find_first_not_of(" \t\n\r\f\v"), s.size()));
        return s;
    }

    // for MSVC and ClangCL compilers that do not support __int128_t or std::from_chars with __int128_t
#if !defined(LIBWOLV_BUILTIN_UINT128)
    [[nodiscard]] constexpr u8 digitValue(const char c) noexcept {
        if (c >= '0' && c <= '9') { return c - '0'; }
        if (c >= 'a' && c <= 'z') { return c - 'a' + 10; }
        if (c >= 'A' && c <= 'Z') { return c - 'A' + 10; }
        return 255;
    }

    template<typename T> requires std::same_as<T, wolv::i128> || std::same_as<T, wolv::u128>
    [[nodiscard]] std::from_chars_result parseWideInteger(const char* start, const char* const end, T& value, const int base) {
        using U = wolv::u128;
        constexpr U umax = std::numeric_limits<U>::max();

        U riskyVal;
        U maxDigit;
        if constexpr (std::numeric_limits<T>::is_signed) {
            constexpr U imax = umax >> 1; // 2^127 - 1
            riskyVal = imax / static_cast<U>(base);
            maxDigit = imax % static_cast<U>(base);
        } else {
            riskyVal = umax / static_cast<U>(base);
            maxDigit = umax % static_cast<U>(base);
        }
        U acc = 0;
        bool overflowed = false;
        const char* current = start;

        for (; current != end; ++current) {
            const u8 digit = digitValue(*current);
            if (digit >= base) { break; }

            if (acc < riskyVal || (acc == riskyVal && static_cast<U>(digit) <= maxDigit)) {
                acc = acc * static_cast<U>(base) + static_cast<U>(digit);
            } else {
                overflowed = true; // keep going, current must point to first char not matching the pattern
            }
        }
        if (current == start) { return {.ptr = start, .ec = std::errc::invalid_argument}; }
        if (overflowed) { return {.ptr = current, .ec = std::errc::result_out_of_range}; }
        value = static_cast<T>(acc);
        return {.ptr = current, .ec = std::errc{}};
    }
#endif
    // cannot use std::expected because libstdc++ version that Github Action runner is using doesn't provide the header <expected> (ubuntu-22.04, GCC 11.4.0)

    template<typename T> requires std::integral<T> || std::same_as<T, wolv::i128> || std::same_as<T, wolv::u128>
    [[nodiscard]] std::optional<T> parseNumber(const char** const str_ptr, const char* const end_pos, std::errc& out_err) noexcept {
        auto str = trim_ascii_start(std::string_view{*str_ptr, end_pos});
        int base = 10;

        // minus/plus signs are handled in minus/plus unary operations
        if (str.starts_with('+') || str.starts_with('-')) {
            out_err = std::errc::invalid_argument;
            return std::nullopt;
        }

        if (str.starts_with("0x") || str.starts_with("0X")) {
            base = 16;
            str.remove_prefix(2);
        } else if (str.starts_with("0o") || str.starts_with("0O")) {
            base = 8;
            str.remove_prefix(2);
        }  else if (str.starts_with("0b") || str.starts_with("0B")) {
            base = 2;
            str.remove_prefix(2);
        }
        // 0x-123 and 0x+123 are invalid
        if (str.starts_with('+') || str.starts_with('-')) {
            out_err = std::errc::invalid_argument;
            return std::nullopt;
        }
        T value{};
#if defined(LIBWOLV_BUILTIN_UINT128)
        const auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), value, base);
#else
        const auto [ptr, ec] = [&] {
            if constexpr (std::same_as<T, wolv::i128> || std::same_as<T, wolv::u128>) {
                return parseWideInteger(str.data(), str.data() + str.size(), value, base);
            } else {
                return std::from_chars(str.data(), str.data() + str.size(), value, base);
            }
        }();
#endif
        if (ec != std::errc()) {
            out_err = ec;
            return std::nullopt;
        }
        *str_ptr = ptr;
        return value;
    }
    template<std::floating_point T>
    [[nodiscard]] std::optional<T> parseNumber(const char** const str_ptr, const char* const end_pos, std::errc& out_err) noexcept {
        auto str = trim_ascii_start(std::string_view{*str_ptr, end_pos});
        std::chars_format fmt = std::chars_format::general;

        // minus/plus signs are handled in minus/plus unary operations
        if (str.starts_with('+') || str.starts_with('-')) {
            out_err = std::errc::invalid_argument;
            return std::nullopt;
        }

        if (str.starts_with("0x") || str.starts_with("0X")) {
            fmt = std::chars_format::hex;
            str = str.substr(2);
        }
        // 0x-123p0 and 0x+123p0 are invalid
        if (str.starts_with('+') || str.starts_with('-')) {
            out_err = std::errc::invalid_argument;
            return std::nullopt;
        }
#if defined(_LIBCPP_VERSION)
        using FixedT = std::conditional_t<std::same_as<T, long double>, double, T>;
        FixedT value{}; // libc++ std::from_chars doesn't support T=long double right now
#else
        T value{};
#endif
        const auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), value, fmt);
        if (ec != std::errc()) {
            out_err = ec;
            return std::nullopt;
        }
        *str_ptr = ptr;
        return value;
    }
}

namespace wolv::math_eval {
    template<typename T>
    i16 MathEvaluator<T>::comparePrecedence(const Operator &a, const Operator &b) {
        return (static_cast<i8>(a) & 0x0F0) - (static_cast<i8>(b) & 0x0F0);
    }

    template<typename T>
    bool MathEvaluator<T>::isLeftAssociative(const Operator &op) {
        return (static_cast<u32>(op) & 0x100) == 0;
    }

    template<typename T>
    bool MathEvaluator<T>::isUnary(const Operator &op) {
        return (static_cast<u32>(op) & 0x200) != 0;
    }

    template<typename T>
    std::pair<typename MathEvaluator<T>::Operator, size_t> MathEvaluator<T>::toOperator(const std::string &input) {
        if (input.starts_with("##")) return { Operator::Combine,                2 };
        if (input.starts_with("==")) return { Operator::Equals,                 2 };
        if (input.starts_with("!=")) return { Operator::NotEquals,              2 };
        if (input.starts_with(">=")) return { Operator::GreaterThanOrEquals,    2 };
        if (input.starts_with("<=")) return { Operator::LessThanOrEquals,       2 };
        if (input.starts_with(">>")) return { Operator::ShiftRight,             2 };
        if (input.starts_with("<<")) return { Operator::ShiftLeft,              2 };
        if (input.starts_with("||")) return { Operator::Or,                     2 };
        if (input.starts_with("^^")) return { Operator::Xor,                    2 };
        if (input.starts_with("&&")) return { Operator::And,                    2 };
        if (input.starts_with("**")) return { Operator::Exponentiation,         2 };
        if (input.starts_with(">"))  return { Operator::GreaterThan,            1 };
        if (input.starts_with("<"))  return { Operator::LessThan,               1 };
        if (input.starts_with("!"))  return { Operator::Not,                    1 };
        if (input.starts_with("|"))  return { Operator::BitwiseOr,              1 };
        if (input.starts_with("^"))  return { Operator::BitwiseXor,             1 };
        if (input.starts_with("&"))  return { Operator::BitwiseAnd,             1 };
        if (input.starts_with("~"))  return { Operator::BitwiseNot,             1 };
        if (input.starts_with("+"))  return { Operator::Addition,               1 };
        if (input.starts_with("-"))  return { Operator::Subtraction,            1 };
        if (input.starts_with("*"))  return { Operator::Multiplication,         1 };
        if (input.starts_with("/"))  return { Operator::Division,               1 };
        if (input.starts_with("%"))  return { Operator::Modulus,                1 };
        if (input.starts_with("="))  return { Operator::Assign,                 1 };

        return { Operator::Invalid, 0 };
    }

    template<typename T>
    std::optional<std::queue<typename MathEvaluator<T>::Token>> MathEvaluator<T>::toPostfix(std::queue<Token> inputQueue) {
        std::queue<Token> outputQueue;
        std::stack<Token> operatorStack;

        while (!inputQueue.empty()) {
            Token currToken = inputQueue.front();
            inputQueue.pop();

            if (currToken.type == TokenType::Number || currToken.type == TokenType::Variable || currToken.type == TokenType::Function)
                outputQueue.push(currToken);
            else if (currToken.type == TokenType::Operator) {
                while ((!operatorStack.empty()) && ((operatorStack.top().type == TokenType::Operator && currToken.type == TokenType::Operator && (comparePrecedence(operatorStack.top().op, currToken.op) > 0)) || (comparePrecedence(operatorStack.top().op, currToken.op) == 0 && isLeftAssociative(currToken.op))) && operatorStack.top().type != TokenType::Bracket) {
                    outputQueue.push(operatorStack.top());
                    operatorStack.pop();
                }
                operatorStack.push(currToken);
            } else if (currToken.type == TokenType::Bracket) {
                if (currToken.bracketType == BracketType::Left)
                    operatorStack.push(currToken);
                else {
                    while (!operatorStack.empty() && (operatorStack.top().type != TokenType::Bracket || (operatorStack.top().type == TokenType::Bracket && operatorStack.top().bracketType != BracketType::Left))) {
                        outputQueue.push(operatorStack.top());
                        operatorStack.pop();
                    }

                    if (operatorStack.empty()) {
                        this->setError("Mismatching parenthesis!");
                        return std::nullopt;
                    }

                    operatorStack.pop();
                }
            }
        }

        while (!operatorStack.empty()) {
            auto top = operatorStack.top();

            if (top.type == TokenType::Bracket) {
                this->setError("Mismatching parenthesis!");
                return std::nullopt;
            }

            outputQueue.push(top);
            operatorStack.pop();
        }

        return outputQueue;
    }

    template<typename T>
    std::optional<std::queue<typename MathEvaluator<T>::Token>> MathEvaluator<T>::parseInput(std::string input) {
        std::queue<Token> inputQueue;

        static constexpr auto isValue = [](const Token& t) {
            return t.type == TokenType::Number ||
                   t.type == TokenType::Variable ||
                   t.type == TokenType::Function ||
                   (t.type == TokenType::Bracket && t.bracketType == BracketType::Right);
        };

        const char *prevPos = input.data();
        for (const char *pos = prevPos; *pos != '\0';) {
            std::errc parse_err{};
            if (const auto number = parseNumber<T>(&pos, input.data() + input.size(), parse_err)) {
                if (!inputQueue.empty() && isValue(inputQueue.back())) {
                    this->setError("Invalid syntax!");
                    return std::nullopt;
                }
                inputQueue.push(Token { .type = TokenType::Number, .number = *number, .name = "", .arguments = { } });
            } else if (parse_err == std::errc::result_out_of_range) {
                this->setError("Number out of range!");
                return std::nullopt;
            } else if (*pos == '(') {
                if (!inputQueue.empty() && isValue(inputQueue.back())) {
                    this->setError("Invalid syntax!");
                    return std::nullopt;
                }
                inputQueue.push(Token { .type = TokenType::Bracket, .bracketType = BracketType::Left, .name = "", .arguments = { } });
                pos++;
            } else if (*pos == ')') {
                inputQueue.push(Token { .type = TokenType::Bracket, .bracketType = BracketType::Right, .name = "", .arguments = { } });
                pos++;
            } else if (std::isspace(*pos)) {
                pos++;
            } else {
                auto [op, width] = toOperator(pos);

                if (inputQueue.empty() || inputQueue.back().type == TokenType::Operator || (inputQueue.back().type == TokenType::Bracket && inputQueue.back().bracketType == BracketType::Left)) {
                    if (op == Operator::Addition)
                        op = Operator::Plus;
                    else if (op == Operator::Subtraction)
                        op = Operator::Minus;
                }

                if (op != Operator::Invalid) {
                    inputQueue.push(Token { .type = TokenType::Operator, .op = op, .name = "", .arguments = { } });
                    pos += width;
                } else {
                    Token token = {};

                    while (std::isalpha(*pos) || *pos == '_') {
                        token.name += *pos;
                        pos++;
                    }

                    if (*pos == '(') {
                        pos++;

                        u32 depth = 1;
                        std::vector<std::string> expressions;
                        expressions.emplace_back();

                        while (*pos != '\0') {
                            if (*pos == '(') depth++;
                            else if (*pos == ')') depth--;

                            if (depth == 0)
                                break;

                            if (depth == 1 && *pos == ',') {
                                expressions.emplace_back();
                                pos++;
                            }

                            expressions.back() += *pos;

                            pos++;
                        }
                        if (*pos == '\0') {
                            this->setError("Mismatching parenthesis!");
                            return std::nullopt;
                        }
                        pos++;

                        for (const auto &expression : expressions) {
                            if (expression.empty() && expressions.size() > 1) {
                                this->setError("Invalid function call syntax!");
                                return std::nullopt;
                            }
                            else if (expression.empty())
                                break;

                            auto newInputQueue = parseInput(expression);
                            if (!newInputQueue.has_value())
                                return std::nullopt;

                            auto postfixTokens = toPostfix(*newInputQueue);
                            if (!postfixTokens.has_value())
                                return std::nullopt;

                            auto result = evaluate(*postfixTokens);
                            if (!result.has_value()) {
                                this->setError("Invalid argument for function!");
                                return std::nullopt;
                            }

                            token.arguments.push_back(result.value());
                        }

                        token.type = TokenType::Function;

                        if (!inputQueue.empty() && isValue(inputQueue.back())) {
                            this->setError("Invalid syntax!");
                            return std::nullopt;
                        }
                        inputQueue.push(token);

                    } else {
                        token.type = TokenType::Variable;

                        if (!inputQueue.empty() && isValue(inputQueue.back())) {
                            this->setError("Invalid syntax!");
                            return std::nullopt;
                        }
                        inputQueue.push(token);
                    }
                }
            }

            if (prevPos == pos) {
                this->setError("Invalid syntax!");
                return std::nullopt;
            }

            prevPos = pos;
        }

        return inputQueue;
    }

    template<typename T>
    std::optional<T> MathEvaluator<T>::evaluate(std::queue<Token> postfixTokens) {
        std::stack<T> evaluationStack;

        while (!postfixTokens.empty()) {
            auto front = postfixTokens.front();
            postfixTokens.pop();

            if (front.type == TokenType::Number)
                evaluationStack.push(front.number);
            else if (front.type == TokenType::Operator) {
                T rightOperand, leftOperand;
                if (isUnary(front.op)) {
                    if (evaluationStack.empty()) {
                        this->setError("Not enough operands for operator!");
                        return std::nullopt;
                    } else {
                        rightOperand = evaluationStack.top();
                        evaluationStack.pop();
                        leftOperand = 0;
                    }
                } else {
                    if (evaluationStack.size() < 2) {
                        this->setError("Not enough operands for operator!");
                        return std::nullopt;
                    } else {
                        rightOperand = evaluationStack.top();
                        evaluationStack.pop();
                        leftOperand = evaluationStack.top();
                        evaluationStack.pop();
                    }
                }

                T result = [] {
                   if constexpr (std::numeric_limits<T>::has_quiet_NaN)
                       return std::numeric_limits<T>::quiet_NaN();
                   else
                       return 0;
                }();
                switch (front.op) {
                    default:
                    case Operator::Invalid:
                        this->setError("Invalid operator!");
                        return std::nullopt;
                    case Operator::And:
                        result = static_cast<i64>(leftOperand) && static_cast<i64>(rightOperand);
                        break;
                    case Operator::Or:
                        result = static_cast<i64>(leftOperand) || static_cast<i64>(rightOperand);
                        break;
                    case Operator::Xor:
                        result = (static_cast<i64>(leftOperand) ^ static_cast<i64>(rightOperand)) > 0;
                        break;
                    case Operator::GreaterThan:
                        result = leftOperand > rightOperand;
                        break;
                    case Operator::LessThan:
                        result = leftOperand < rightOperand;
                        break;
                    case Operator::GreaterThanOrEquals:
                        result = leftOperand >= rightOperand;
                        break;
                    case Operator::LessThanOrEquals:
                        result = leftOperand <= rightOperand;
                        break;
                    case Operator::Equals:
                        result = leftOperand == rightOperand;
                        break;
                    case Operator::NotEquals:
                        result = leftOperand != rightOperand;
                        break;
                    case Operator::Not:
                        result = !static_cast<i64>(rightOperand);
                        break;
                    case Operator::BitwiseOr:
                        result = static_cast<i64>(leftOperand) | static_cast<i64>(rightOperand);
                        break;
                    case Operator::BitwiseXor:
                        result = static_cast<i64>(leftOperand) ^ static_cast<i64>(rightOperand);
                        break;
                    case Operator::BitwiseAnd:
                        result = static_cast<i64>(leftOperand) & static_cast<i64>(rightOperand);
                        break;
                    case Operator::BitwiseNot:
                        result = ~static_cast<i64>(rightOperand);
                        break;
                    case Operator::ShiftLeft:
                        result = static_cast<i64>(leftOperand) << static_cast<i64>(rightOperand);
                        break;
                    case Operator::ShiftRight:
                        result = static_cast<i64>(leftOperand) >> static_cast<i64>(rightOperand);
                        break;
                    case Operator::Addition:
                        result = leftOperand + rightOperand;
                        break;
                    case Operator::Subtraction:
                        result = leftOperand - rightOperand;
                        break;
                    case Operator::Multiplication:
                        result = leftOperand * rightOperand;
                        break;
                    case Operator::Division:
                        if (rightOperand == 0) {
                            this->setError("Division by Zero!");
                            return std::nullopt;
                        }

                        result = leftOperand / rightOperand;
                        break;
                    case Operator::Modulus:
                        if (rightOperand == 0) {
                            this->setError("Division by Zero!");
                            return std::nullopt;
                        }

                        if constexpr (std::floating_point<T>)
                            result = std::fmod(leftOperand, rightOperand);
                        else
                            result = leftOperand % rightOperand;
                        break;
                    case Operator::Exponentiation:
                        if constexpr (std::floating_point<T>)
                            result = std::pow(leftOperand, rightOperand);
                        else
                            result = powi(leftOperand, rightOperand);
                        break;
                    case Operator::Combine:
                        result = (static_cast<u64>(leftOperand) << (64 - std::countl_zero(static_cast<u64>(rightOperand)))) | static_cast<u64>(rightOperand);
                        break;
                    case Operator::Plus:
                        result = +rightOperand;
                        break;
                    case Operator::Minus:
                        result = -rightOperand;
                        break;
                }

                evaluationStack.push(result);
            } else if (front.type == TokenType::Variable) {
                if (this->m_variables.contains(front.name))
                    evaluationStack.push(this->m_variables.at(front.name).value);
                else {
                    this->setError("Unknown variable!");
                    return std::nullopt;
                }
            } else if (front.type == TokenType::Function) {
                const auto it = this->m_functions.find(front.name);
                if (it == this->m_functions.end()) {
                    this->setError("Unknown function \"" + front.name + "\"!");
                    return std::nullopt;
                }
                const auto result = it->second(front.arguments);

                if (!result.has_value()) {
                    this->setError("Invalid argument for function \"" + front.name + "\"!");
                    return std::nullopt;
                }
                evaluationStack.push(*result);
            } else {
                this->setError("Parenthesis in postfix expression!");
                return std::nullopt;
            }
        }

        if (evaluationStack.empty()) {
            return std::nullopt;
        }
        else if (evaluationStack.size() > 1) {
            this->setError("Undigested input left!");
            return std::nullopt;
        }
        else {
            return evaluationStack.top();
        }
    }


    template<typename T>
    std::optional<T> MathEvaluator<T>::evaluate(const std::string &input) {
        auto inputQueue = parseInput(input);
        if (!inputQueue.has_value() || inputQueue->empty())
            return std::nullopt;

        std::string resultVariable = "ans";

        {
            auto queueCopy = *inputQueue;
            if (queueCopy.front().type == TokenType::Variable && queueCopy.size() > 2) {
                resultVariable = queueCopy.front().name;
                queueCopy.pop();
                if (queueCopy.front().type != TokenType::Operator || queueCopy.front().op != Operator::Assign)
                    resultVariable = "ans";
                else {
                    inputQueue->pop();
                    inputQueue->pop();
                }
            }
        }

        auto postfixTokens = toPostfix(*inputQueue);
        if (!postfixTokens.has_value())
            return std::nullopt;

        auto result = evaluate(*postfixTokens);

        if (result.has_value() && !this->getVariables()[resultVariable].constant)
            this->setVariable(resultVariable, result.value());

        return result;
    }

    template<typename T>
    void MathEvaluator<T>::setVariable(const std::string &name, T value, bool constant) {
        this->m_variables[name] = { value, constant };
    }

    template<typename T>
    void MathEvaluator<T>::setFunction(const std::string &name, const std::function<std::optional<T>(std::vector<T>)> &function, size_t minNumArgs, size_t maxNumArgs) {
        this->m_functions[name] = [this, minNumArgs, maxNumArgs, function](auto args) -> std::optional<T> {
            if (args.size() < minNumArgs || args.size() > maxNumArgs) {
                this->setError("Invalid number of function arguments!");
                return std::nullopt;
            }

            return function(args);
        };
    }


    template<typename T>
    void MathEvaluator<T>::registerStandardVariables() {
        this->setVariable("ans", 0);
        this->setVariable("pi", std::numbers::pi, true);
        this->setVariable("e", std::numbers::e, true);
        this->setVariable("phi", std::numbers::phi, true);
    }

    template<typename T>
    void MathEvaluator<T>::registerStandardFunctions() {
        if constexpr (std::floating_point<T>) {
            this->setFunction(
                "sin", [](auto args) { return std::sin(args[0]); }, 1, 1);
            this->setFunction(
                "cos", [](auto args) { return std::cos(args[0]); }, 1, 1);
            this->setFunction(
                "tan", [](auto args) { return std::tan(args[0]); }, 1, 1);
            this->setFunction(
                "sqrt", [](auto args) { return std::sqrt(args[0]); }, 1, 1);
            this->setFunction(
                "ceil", [](auto args) { return std::ceil(args[0]); }, 1, 1);
            this->setFunction(
                "floor", [](auto args) { return std::floor(args[0]); }, 1, 1);
            this->setFunction(
                "sign", [](auto args) { return (args[0] > 0) ? 1 : (args[0] == 0) ? 0 : -1; }, 1, 1);
            this->setFunction(
                "abs", [](auto args) { return std::abs(args[0]); }, 1, 1);
            this->setFunction(
                "ln", [](auto args) { return std::log(args[0]); }, 1, 1);
            this->setFunction(
                "lb", [](auto args) { return std::log2(args[0]); }, 1, 1);
            this->setFunction(
                "log", [](auto args) { return args.size() == 1 ? std::log10(args[0]) : std::log(args[1]) / std::log(args[0]); }, 1, 2);
        }
    }

    template class MathEvaluator<long double>;
    template class MathEvaluator<i64>;
    template class MathEvaluator<u64>;
    template class MathEvaluator<i128>;
    template class MathEvaluator<u128>;
}
