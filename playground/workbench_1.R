# Create matrix
win <- c(25, 10, 5)
e_dies <- numeric(12)
p <- 1/36
for (die_1 in 1:6){
  for (die_2 in 1:6){
    max_dice <- max(die_1, die_2)
    min_dice <- min(die_1, die_2)
    sum_dice <- die_1 + die_2
    e_dies[sum_dice] <- (e_dies[sum_dice] + win[1])
    if (die_1 == die_2){
      e_dies[die_1] = (e_dies[die_1] + win[2]) # Only get paid for second, not minimum
    }
    else{
      e_dies[min_dice] <- (e_dies[min_dice] + win[3])
      e_dies[max_dice] <- (e_dies[max_dice] + win[2])
    }
  }
}
e_dies <- e_dies*p
barplot(e_dies, names.arg = seq_along(e_dies), main="Store obligation")
grid(lwd=1, col="blue")

# Small win
win_small <- c(10, 5, 3)
e_dies <- numeric(12)
p <- 1/36
for (die_1 in 1:6){
  for (die_2 in 1:6){
    max_dice <- max(die_1, die_2)
    min_dice <- min(die_1, die_2)
    sum_dice <- die_1 + die_2
    e_dies[sum_dice] <- (e_dies[sum_dice] + win_small[1])
    if (die_1 == die_2){
      e_dies[die_1] = (e_dies[die_1] + win_small[2]) # Only get paid for second, not minimum
    }
    else{
      e_dies[min_dice] <- (e_dies[min_dice] + win_small[3])
      e_dies[max_dice] <- (e_dies[max_dice] + win_small[2])
    }
  }
}
e_dies <- e_dies*p
barplot(e_dies, names.arg = seq_along(e_dies), main="Lille obligation")
grid(lwd=1, col="blue")
