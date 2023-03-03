--
-- Bigint functions that operate directly on strings.
--
-- 2019, Chris Pressey, Cat's Eye Technologies.
-- I hereby place this work into the public domain.  Use entirely at your own risk.
--
-- NOTE: This module is incomplete.  For practical purposes, you probably want to
-- use something like https://github.com/empyreuma/bigint.lua instead.
--
-- Arguments are assumed to be strings consisting solely of ASCII digit characters.
-- Under this assumption, the return value will also be a string of ASCII digits.
--


function add_strings(a, b)
    local pos = 1
    local len_a = string.len(a)
    local len_b = string.len(b)
    local acc = ""
    local carry = 0
    local charval_a = 0
    local charval_b = 0
    local sum

    while true do
        charval_a = 0
        if pos <= len_a then
            charval_a = string.byte(a, -pos) - 48
        end

        charval_b = 0
        if pos <= len_b then
            charval_b = string.byte(b, -pos) - 48
        end

        if pos > len_a and pos > len_b and carry == 0 then
            return acc
        end

        sum = charval_a + charval_b + carry

        acc = tostring(sum % 10) .. acc
        carry = math.floor(sum / 10)

        pos = pos + 1
    end
end


function test_add_strings(a, b, c)
    local r = add_strings(a, b)
    if r ~= c then
        print("FAILURE: ", a, b, c, r)
    end
end


test_add_strings("1", "1", "2")
test_add_strings("9", "9", "18")
test_add_strings("1000", "1001", "2001")
test_add_strings("9999", "0", "9999")
test_add_strings("1", "9999", "10000")
test_add_strings("9999", "9999", "19998")
test_add_strings("0001", "00000005", "00000006")
test_add_strings("0", "0", "0")
test_add_strings(
    "123456789123456789123456789123456789123456789123456789123456789123456789",
    "123456789123456789123456789123456789123456789123456789123456789123456789",
    "246913578246913578246913578246913578246913578246913578246913578246913578"
)
test_add_strings(
    "123456789123456789123456789123456789123456789123456789123456789123456789",
    "999999999999999999999999999999999999999999999999999999999999999999999999",
    "1123456789123456789123456789123456789123456789123456789123456789123456788"
)
