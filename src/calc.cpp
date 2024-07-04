#include "calc.hpp"

#include <cctype>
#include <cmath>
#include <iostream>

namespace {

const std::size_t max_decimal_digits = 10;

enum class Op
{
    ERR,
    SET,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    NEG,
    POW,
    SQRT
};

std::size_t arity(const Op op)
{
    switch (op) {
    // error
    case Op::ERR:
        return 0;
        // unary
    case Op::NEG: return 1;
    case Op::SQRT:
        return 1;
        // binary
    case Op::SET: return 2;
    case Op::ADD: return 2;
    case Op::SUB: return 2;
    case Op::MUL: return 2;
    case Op::DIV: return 2;
    case Op::REM: return 2;
    case Op::POW: return 2;
    }
    return 0;
}

Op parse_op(std::string_view line, std::size_t & i)
{
    const auto rollback = [&i, &line](const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    switch (line[i++]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        --i; // a first digit is a part of op's argument
        return Op::SET;
    case '+':
        return Op::ADD;
    case '-':
        return Op::SUB;
    case '*':
        return Op::MUL;
    case '/':
        return Op::DIV;
    case '%':
        return Op::REM;
    case '_':
        return Op::NEG;
    case '^':
        return Op::POW;
    case 'S':
        switch (line[i++]) {
        case 'Q':
            switch (line[i++]) {
            case 'R':
                switch (line[i++]) {
                case 'T':
                    return Op::SQRT;
                default:
                    return rollback(4);
                }
            default:
                return rollback(3);
            }
        default:
            return rollback(2);
        }
    case '(':
        switch (line[i++]) {
        case '+':
            switch (line[i++]) {
            case ')':
                return Op::ADD;
            default:
                return rollback(2);
            }
        case '-':
            switch (line[i++]) {
            case ')':
                return Op::SUB;
            default:
                return rollback(2);
            }
        case '*':
            switch (line[i++]) {
            case ')':
                return Op::MUL;
            default:
                return rollback(2);
            }
        case '/':
            switch (line[i++]) {
            case ')':
                return Op::DIV;
            default:
                return rollback(2);
            }
        case '%':
            switch (line[i++]) {
            case ')':
                return Op::REM;
            default:
                return rollback(2);
            }
        case '^':
            switch (line[i++]) {
            case ')':
                return Op::POW;
            default:
                return rollback(2);
            }
        }
    default:
        return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

double parse_arg(const std::string & line, std::size_t & i, bool flag)
{
    double res = 0;
    std::size_t count = 0;
    bool good = true;
    bool integer = true;
    double fraction = 1;
    while (good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            if (integer) {
                res *= 10;
                res += line[i] - '0';
            }
            else {
                fraction /= 10;
                res += (line[i] - '0') * fraction;
            }
            ++i;
            ++count;
            break;
        case '.':
            integer = false;
            ++i;
            break;
        case ' ':
            if (flag) {
                return res;
            }
            else {
                good = false;
                break;
            }
        default:
            good = false;
            break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        res = 0;
    }
    else if (i < line.size() || count > max_decimal_digits) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
        res = 0;
    }
    return res;
}

double unary(const double current, const Op op)
{
    switch (op) {
    case Op::NEG:
        return -current;
    case Op::SQRT:
        if (current > 0) {
            return std::sqrt(current);
        }
        else {
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            [[fallthrough]];
        }
    default:
        return current;
    }
}

double binary(const Op op, const double left, const double right)
{
    switch (op) {
    case Op::SET:
        return right;
    case Op::ADD:
        return left + right;
    case Op::SUB:
        return left - right;
    case Op::MUL:
        return left * right;
    case Op::DIV:
        if (right != 0) {
            return left / right;
        }
        else {
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return left;
        }
    case Op::REM:
        if (right != 0) {
            return std::fmod(left, right);
        }
        else {
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return left;
        }
    case Op::POW:
        return std::pow(left, right);
    default:
        return left;
    }
}

} // anonymous namespace

double process_line(double currentValue,
                    std::string const & line)
{
    double tempValue = currentValue;
    std::size_t i = 0;
    bool isFold = false;
    if (line[0] == '(' && line[2] == ')') {
        isFold = true;
    }
    const auto op = parse_op(line, i);
    switch (arity(op)) {
    case 2: {
        if ((line.size() == 1 && !std::isdigit(line[i])) || (line.size() > 1 && i == line.size())) {
            std::cerr << "No argument for a binary operation" << std::endl;
            break;
        }
        bool isCorrectLine = true;
        std::size_t countOfArgs = 0;
        while (i < line.size()) {
            i = skip_ws(line, i);
            if (countOfArgs != 0 && i >= line.size()) {
                break;
            }
            if ((!std::isdigit(line[i]) && !std::isspace(line[i]) && isFold && line[i] != '.') || (op == Op::REM && line[i] == '0')) {
                isCorrectLine = false;
            }
            const auto old_i = i;
            const auto arg = parse_arg(line, i, isFold);
            if (i == old_i) {
                std::cerr << "No argument for a binary operation" << std::endl;
                return tempValue;
            }
            else {
                countOfArgs++;
            }
            if (!isFold && i < line.size()) {
                break;
            }
            if (isCorrectLine) {
                currentValue = binary(op, currentValue, arg);
            }
            else {
                currentValue = binary(op, currentValue, arg);
                return tempValue;
            }
        }
        return currentValue;
    }
    case 1: {
        if (i < line.size()) {
            std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
            break;
        }
        return unary(currentValue, op);
    }
    default: break;
    }
    return currentValue;
}

double process_line(double currentValue, bool &, const std::string & line)
{
    return process_line(currentValue, line);
}