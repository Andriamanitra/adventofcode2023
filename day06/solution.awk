/Time/ { split($0, times, " ") }
/Dist/ { split($0, dists, " ") }
END {
  answer1 = 1
  for (i = 2; i <= length(times); i += 1) {
    wins = 0
    time = times[i]
    record = dists[i]

    for (charge = 1; charge < time; charge += 1) {
      dist = charge * (time - charge)
      if (dist > record) wins += 1
    }
    answer1 *= wins
  }
  print "Part 1: " answer1
}
