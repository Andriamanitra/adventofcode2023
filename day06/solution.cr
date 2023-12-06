times = gets.not_nil!.split[1..].map(&.to_i)
dists = gets.not_nil!.split[1..].map(&.to_i)

def count_wins(time, record)
  (time//time...time).count { |c| c * (time - c) > record }
end

puts "Part 1:", times.zip(dists).product { |t,d| count_wins(t, d) }

time = times.join.to_i64
dist = dists.join.to_i64
puts "Part 2:", count_wins(time, dist)
puts "Part 2 (math):",  Math.isqrt(time * time - dist * 4)
