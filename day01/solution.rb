DIGITS = %w[zero one two three four five six seven eight nine]
input = ARGF.to_a

puts "Part 1:"
puts input.sum { |line|
    nums = line.scan(/\d/)
    nums.first.to_i * 10 + nums.last.to_i
}

puts "Part 2:"
puts input.sum { |line|
    digit_regex = Regexp.union(*DIGITS, /\d/)
    first_digit = line[digit_regex]
    _, last_digit, _ = line.rpartition(digit_regex)
    tens = DIGITS.index(first_digit) || first_digit.to_i
    ones = DIGITS.index(last_digit) || last_digit.to_i
    10 * tens + ones
}
