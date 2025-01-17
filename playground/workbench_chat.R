# Example vector with indices
values <- c(5, 10, 15, 20, 25)  # Values
keys <- c("A", "B", "C", "D", "E")  # Indices or keys

# Create a named vector
named_vector <- setNames(values, keys)

# Plot the bar plot with keys as x-axis labels
barplot(named_vector, main = "Bar Plot with Custom X Labels", xlab = "Keys", ylab = "Values", col = "skyblue")

# Optional: Add grid lines for better visualization
grid(nx = NA, ny = NULL)


# Start with an empty data frame
df <- data.frame()

# Insert columns one by one
df$Name <- c("Alice", "Bob", "Charlie")  # Insert Name column
df$Age <- c(25, 30, 35)                  # Insert Age column
df$Score <- c(88, 91, 84)                # Insert Score column

# View the data frame
print(df)