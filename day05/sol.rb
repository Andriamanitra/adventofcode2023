# Part 1
s = File.read("input.txt")
seeds = s.lines[0].split[1..].map(&:to_i)
sections = s.split("\n\n")[1..]
sections.map do |sect|
  xs = sect.lines[1..].map{_1.split.map(&:to_i)}
  seeds.map! do |seed|
    case xs.find{|dest,src,len| (src...src+len) === seed}
    in nil then seed
    in [dest,src,len] then dest + seed - src
    end
  end
end
p seeds.min
