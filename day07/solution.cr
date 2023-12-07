hands = ARGF.each_line.map do |line|
  cards, bid = line.split
  {cards.chars, bid.to_i}
end.to_a

part1 = hands.sort_by do |cards, bid|
  counts = cards.tally.values.sort.reverse
  values = cards.map { |c| "23456789TJQKA".index!(c) }
  {counts, values}
end

part2 = hands.sort_by do |cards, bid|
  jokers = cards.count('J')
  counts = cards.reject('J').tally.values.sort.reverse
  if counts.empty?
    counts.push(jokers)
  else
    counts[0] += jokers
  end
  values = cards.map { |c| "J23456789TQKA".index!(c) }
  {counts, values}
end

puts "Part 1:", part1.zip(1..).sum{|(_,bid), idx| bid*idx}
puts "Part 2:", part2.zip(1..).sum{|(_,bid), idx| bid*idx}
