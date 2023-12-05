s = File.read("input.txt")
sections = s.split("\n\n")[1..]

# Part 1
seeds = s.lines[0].split[1..].map(&:to_i)

sections.map do |sect|
  xs = sect.lines[1..].map{_1.split.map(&:to_i)}
  seeds.map! do |seed|
    case xs.find{|dest,src,len| (src...src+len) === seed}
    in nil then seed
    in [dest,src,len] then dest + seed - src
    end
  end
end

puts "Part 1:", seeds.min


# Part 2
seeds = s.lines[0].split[1..].map(&:to_i).each_slice(2).map{[_1, _1+_2-1]}

sections.map do |sect|
  xs = sect.lines[1..].map{_1.split.map(&:to_i)}.sort_by{_2}
  new_seeds = []

  xs.each do |dest,src,len|
    next_seeds = []
    a = src
    b = src + len - 1
    seeds.each do |c, d|
      # A-C-D-B
      if a <= c && d <= b # full overlap
        new_seeds << [c - src + dest, d - src + dest]
      # C-A-B-D
      elsif c < a && b < d # inside
        next_seeds << [c, a-1]
        new_seeds << [a - src + dest, b - src + dest]
        next_seeds << [b+1, d]
      # A-C-B-D
      elsif a < c && c < b && b < d # partial overlap
        new_seeds << [c - src + dest, b - src + dest]
        next_seeds << [b+1, d]
      # C-A-D-B 
      elsif c < a && a < d && d < b # partial overlap
        next_seeds << [c, a-1]
        new_seeds << [a - src + dest, d - src + dest]
      # A-B-C-D or C-D-A-B
      else # no overlap
        next_seeds << [c, d]
      end
    end
    seeds = next_seeds
  end

  seeds += new_seeds
end

puts "Part 2:", seeds.map{|a,b| a }.min
