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
# Special states: 2, 4, 8, 10, 34, 41, 43, 44, 46, 51
# Stopping: 39, 1

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

# state 41
P[41,] <- 0
P[41, 23] <- 1

# state 43, extra die roll -> do nothing

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
# Now lets study the relevant states
# Irrelevant states, ie. states where you instantly get redirected:
# 2, 4, 8, 10, 34, 43, 44, 46, 51
to_remove <- c(2, 4, 8, 10, 34, 43, 44, 46, 51)
to_keep <- setdiff(1:53, to_remove)
dist_keep <- nullspace_R_dist[to_keep] / sum(nullspace_R_dist[to_keep])
df_relevant_states <- data.frame(relevant_states = to_keep, probability = dist_keep)
df_sorted <- df_relevant_states[order(df_relevant_states$probability, decreasing=TRUE),]
houses <- c(5, 6, 13, 14, 16, 18, 19, 21, 22, 25 ,26, 27, 30, 31, 32, 36, 37, 38)

# Calculating return on investment (ROI)
name_of_grounds <- replicate(38, NA)
name_of_grounds[5] <- "Rådhuspladsen"
name_of_grounds[6] <- "Vesterbrogade"
name_of_grounds[13] <- "Strandvejen"
name_of_grounds[14] <- "Østerbrogade"
name_of_grounds[16] <- "Skræderi"
name_of_grounds[18] <- "Malerværksted"
name_of_grounds[19] <- "Snedkerværksted"
name_of_grounds[21] <- "Hotel"
name_of_grounds[22] <- "Bilværksted"
name_of_grounds[25] <- "Havngade"
name_of_grounds[26] <- "Dr.Tværgade"
name_of_grounds[27] <- "Kongensgade"
name_of_grounds[28] <- "Parkeringshus"
name_of_grounds[30] <- "Møbelforretning"
name_of_grounds[31] <- "Købmandsbutik"
name_of_grounds[32] <- "Boghandel"
name_of_grounds[36] <- "Møbelfabrik"
name_of_grounds[37] <- "Chokoladefabrik"
name_of_grounds[38] <- "Bryggeri"

return_of_grounds <- replicate(38, NA)
return_of_grounds[5] <- 500
return_of_grounds[6] <- 500
return_of_grounds[13] <- 2000
return_of_grounds[14] <- 2000
return_of_grounds[16] <- 1000
return_of_grounds[18] <- 1000
return_of_grounds[19] <- 1000
return_of_grounds[21] <- 0
return_of_grounds[22] <- 0
return_of_grounds[25] <- 1000
return_of_grounds[26] <- 1000
return_of_grounds[27] <- 1000
return_of_grounds[28] <- 0
return_of_grounds[30] <- 2000
return_of_grounds[31] <- 1000
return_of_grounds[32] <- 2000
return_of_grounds[36] <- 4000
return_of_grounds[37] <- 4000
return_of_grounds[38] <- 6000

# Calculating cost of grounds
price_of_grounds <- replicate(38, NA)
price_of_grounds[5] <- 2000
price_of_grounds[6] <- 2000
price_of_grounds[13] <- 4000
price_of_grounds[14] <- 4000
price_of_grounds[16] <- 3000
price_of_grounds[18] <- 4000
price_of_grounds[19] <- 3000
price_of_grounds[21] <- 5000
price_of_grounds[22] <- 4000
price_of_grounds[25] <- 5000
price_of_grounds[26] <- 5000
price_of_grounds[27] <- 5000
price_of_grounds[28] <- 8000
price_of_grounds[30] <- 6000
price_of_grounds[31] <- 5000
price_of_grounds[32] <- 6000
price_of_grounds[36] <- 10000
price_of_grounds[37] <- 10000
price_of_grounds[38] <- 15000


# Calculating cost of building house
price_of_house <- replicate(38, NA)
price_of_house[5] <- 2000
price_of_house[6] <- 2000
price_of_house[13] <- 4000
price_of_house[14] <- 4000
price_of_house[16] <- 3000
price_of_house[18] <- 8000
price_of_house[19] <- 3000
price_of_house[21] <- 7000
price_of_house[22] <- 4000
price_of_house[25] <- 5000
price_of_house[26] <- 5000
price_of_house[27] <- 5000
price_of_house[28] <- 0
price_of_house[30] <- 6000
price_of_house[31] <- 5000
price_of_house[32] <- 6000
price_of_house[36] <- 40000
price_of_house[37] <- 20000
price_of_house[38] <- 30000

# Calculating return after building house
return_with_house <- replicate(38, NA)
return_with_house[5] <- 5000
return_with_house[6] <- 5000
return_with_house[13] <- 10000
return_with_house[14] <- 10000
return_with_house[16] <- 5000
return_with_house[18] <- 10000
return_with_house[19] <- 5000
return_with_house[21] <- 15000
return_with_house[22] <- 10000
return_with_house[25] <- 10000
return_with_house[26] <- 10000
return_with_house[27] <- 10000
return_with_house[28] <- 0
return_with_house[30] <- 12000
return_with_house[31] <- 10000
return_with_house[32] <- 12000
return_with_house[36] <- 40000
return_with_house[37] <- 40000
return_with_house[38] <- 60000

df_grounds <- data.frame(name_of_grounds = name_of_grounds, prob = df_relevant_states$probability[1:38], price_of_grounds = price_of_grounds, return_of_grounds = return_of_grounds, price_of_house, return_with_house)
df_grounds$expected_return_no_house <- df_grounds$prob*df_grounds$return_of_grounds
df_grounds$expected_return_with_house <- df_grounds$prob*df_grounds$return_with_house

df_grounds$roi_no_house <- df_grounds$return_of_grounds/df_grounds$price_of_grounds
df_grounds$roi_with_house <- df_grounds$return_with_house/(price_of_grounds + price_of_house)
df_grounds$expected_roi_no_house <- df_grounds$prob*df_grounds$roi_no_house
df_grounds$expected_roi_with_house <- df_grounds$prob*df_grounds$roi_with_house

par(mar = c(7.5, 4, 4, 2) + 0)
indices <- which(!is.na(df_grounds$return_with_house))
colors <- ifelse(df_sorted$relevant_states %in% houses, "blueviolet", "lightblue")
barplot(df_sorted$probability, names.arg= df_sorted$relevant_states, main="Probability of all states", xlab="State", ylab="Probability", cex.names=0.6, las=2, col=colors)
df_houses <- df_sorted[df_sorted$relevant_states %in% houses,]
barplot(df_grounds$prob[indices], names.arg=df_grounds$name_of_grounds[indices], main="Probability of grounds with building rights", ylab="Probability", cex.names=0.9, las=2, col="blueviolet")

barplot(df_grounds$expected_roi_no_house[indices], names.arg = df_grounds$name_of_grounds[indices], cex.names=0.9, las=2, main = "Expected ROI with no house", col = 'cyan4')
barplot(df_grounds$expected_roi_with_house[indices], names.arg = df_grounds$name_of_grounds[indices], cex.names=0.9, las=2, main = "Expected ROI with house", col = 'cyan4')
barplot(df_grounds$expected_return_no_house[indices], names.arg = df_grounds$name_of_grounds[indices], cex.names=0.9, las=2, main = "Expected return with no house", col = 'chartreuse3')
barplot(df_grounds$expected_return_with_house[indices], names.arg = df_grounds$name_of_grounds[indices], cex.names=0.9, las=2, main = "Expected return with house", col = 'chartreuse3')
