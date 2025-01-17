library(MASS)
n_states <- 53
P <- matrix(0, ncol=n_states, nrow=n_states)
for (i in 1:(n_states-6)){
  P[i, (i+1):(i+6)] <- P[i, (i+1):(i+6)] + 1/6
}
# row 48
P[48, 49:53] <- P[48, 49:53] + 1/6
P[48, 1] <- 1/6

# row 49
P[49, 50:53] <- P[49, 50:53] + 1/6
P[49, 1:2] <- 1/6

# row 50
P[50, 51:53] <- P[50, 51:53] + 1/6
P[50, 1:3] <- 1/6

# row 51
P[51, 52:53] <- P[51, 52:53] + 1/6
P[51, 1:4] <- 1/6

# row 52
P[52, 53] <- P[52, 53] + 1/6
P[52, 1:5] <- 1/6

# row 53
P[53, 1:6] <- P[53, 1:6] + 1/6

# Editing special states
# Special states: 2, 4, 8, 10, 34, 43, 44, 46, 51
# Absorbing ish: 39, 1

# state 2
P[2,] <- 0
P[2, 18] <- 1

# state 4, do nothing actually

# state 8
P[8,] <- 0
P[8, 47] <- 1

# state 10
P[10,] <- 0
P[10, 1] <- 1

# state 34
P[34,] <- 0
P[34, 28] <- 1

# state 43, do nothing actually

# state 44
P[44,] <- 0
P[44, 53] <- 1

# state 46
P[46,] <- 0
P[46, 52] <- 1

# state 51
P[51,] <- 0
P[51, 36] <- 1

# Special states
# state 1
for (i in 49:53){
  if (i != 51){
    P[i, 1:10] <- 0
    P[i, 1] <- 1 - sum(P[i,])
  }
}

# state 39
for (i in 35:38){
  P[i, 39:49]  <- 0
  P[i, 39] <- 1 - sum(P[i,])
}

# Finding null-space
A <- t(P) - diag(length(P[1,]))
nullspace_R <- Null(t(A))
nullspace_R_dist <- nullspace_R/sum(nullspace_R)
nullspace_R_dist <- as.vector(nullspace_R_dist)
print(rowSums(P)) # Check that they're all 1


############### Simulation ###############
states_visits <- numeric(53)
current_state <- 1
states_visits[current_state] <- states_visits[current_state] + 1

mcmc_finans <- function(M){
  for (i in 1:M){
    current_state <- sample(1:length(P[current_state,]), size=1, prob = P[current_state,])
    states_visits[current_state] <- states_visits[current_state] + 1
  }
  states_visits <- states_visits/sum(states_visits)
  df_states <- data.frame((states_visits))
  barplot(states_visits, names.arg = seq_along(states_visits), main="Distribution using MCMC")
}
barplot(nullspace_R_dist, names.arg = seq_along(nullspace_R_dist), main="Distribution by calculating nullspace")
# Now lets study the relevant states
# Irrelevant states, ie. states where you instantly get redirected:
# 2, 4, 8, 10, 34, 43, 44, 46, 51
to_remove <- c(2, 4, 8, 10, 34, 43, 44, 46, 51)
to_keep <- setdiff(1:53, to_remove)
dist_keep <- nullspace_R_dist[to_keep] / sum(nullspace_R_dist[to_keep])
df_relevant_states <- data.frame(relevant_states = to_keep, probability = dist_keep)
df_sorted <- df_relevant_states[order(df_relevant_states$probability, decreasing=TRUE),]
houses <- c(5, 6, 13, 14, 16, 18, 19, 21, 22, 25 ,26, 27, 30, 31, 32, 36, 37, 38)
colors <- ifelse(df_sorted$relevant_states %in% houses, "blueviolet", "lightblue")
barplot(df_sorted$probability, names.arg= df_sorted$relevant_states, main="Bar plot of relevant states", xlab="State", ylab="Probability", cex.names=1, las=2, col=colors)
df_houses <- df_sorted[df_sorted$relevant_states %in% houses,]
barplot(df_houses$probability, names.arg=df_houses$relevant_states, main="Bar plot of states with building rights", xlab="state", ylab="Probability", cex.names=1, las=2, col="blueviolet")