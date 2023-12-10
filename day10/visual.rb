ARGV << "input.txt" if ARGV.empty?

PIPES = '║═╔╗╚╝'
up_pipes = '║╚╝'
def pipify(char) = char.tr('|\-F7LJ', '║═╔╗╚╝')

def red(s) = "\x1b[31m#{s}\x1b[0m"
def green(s) = "\x1b[32m#{s}\x1b[0m"
def grey(s) = "\x1b[90m#{s}\x1b[0m"

def step(grid, ch, r, c, d)
  case [ch, d]
  in ['|', :up] then [r-1, c, :up]
  in ['|', :down] then [r+1, c, :down]
  in ['F', :up] then [r, c+1, :right]
  in ['F', :left] then [r+1, c, :down]
  in ['L', :down] then [r, c+1, :right]
  in ['L', :left] then [r-1, c, :up]
  in ['-', :left] then [r, c-1, :left]
  in ['-', :right] then [r, c+1, :right]
  in ['7', :up] then [r, c-1, :left]
  in ['7', :right] then [r+1, c, :down]
  in ['J', :down] then [r, c-1, :left]
  in ['J', :right] then [r-1, c, :up]
  end
end

C_LEFT = %w(- 7 J)
C_RIGHT = %w(- F L)
C_UP = %w(| L J)
C_DOWN = %w(| 7 F)

grid = ARGF.read.lines(chomp: true).map(&:chars)
sR = nil
sC = nil
grid.each_with_index do |row, i|
  if j = row.index('S')
    sR, sC = i, j
  end
end

can_go_up = sR > 0 && C_DOWN.include?(grid[sR-1][sC])
can_go_left = sC > 0 && C_RIGHT.include?(grid[sR][sC-1])
can_go_right = C_LEFT.include?(grid[sR][sC+1])
can_go_down = C_UP.include?(grid[sR+1][sC])

if can_go_up
  fR, fC, fD = sR-1, sC, :up
  up_pipes += 'S'
elsif can_go_right
  fR, fC, fD = sR, sC+1, :right
else
  fR, fC, fD = sR+1, sC, :down
end

r, c, d = fR, fC, fD
steps = 1
while grid[r][c] != 'S'
  r, c, d = step(grid, grid[r][c], r, c, d)
  steps += 1
end
furthest = steps / 2

r, c, d = fR, fC, fD
x = 1
loop do
  ch = grid[r][c]
  break if ch == 'S'
  if x == furthest
    grid[r][c] = red(pipify(ch))
  else
    grid[r][c] = pipify(ch)
  end
  r, c, d = step(grid, ch, r, c, d)
  x += 1
end

n_inside = 0
grid.each do |row|
  n_left = 0
  row.each_index do |i|
    if up_pipes.include?(row[i])
      n_left += 1
    elsif !PIPES.include?(row[i])
      if n_left.odd?
        n_inside += 1
        row[i] = green(row[i])
      else
        row[i] = grey(row[i])
      end
    end
    row[i] = red(row[i]) if row[i] == "S"
  end
  puts row.join
end

puts "#{steps} steps (furthest point #{red(furthest)} steps away), #{green(n_inside)} tiles inside loop"
