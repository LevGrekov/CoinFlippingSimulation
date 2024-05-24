simulate_coin_flips <- function(n) {
  flips <- sample(c("О", "Р"), size = n, replace = TRUE)
  pairs <- paste0(flips[-n], flips[-1]) 
  alice_count <- sum(pairs == "ОО")
  bob_count <- sum(pairs == "ОР")

  return(c(alice_count, bob_count))
}

simulate_experiments <- function(n, num_experiments) {
  results <- replicate(num_experiments, simulate_coin_flips(n))
  alice_wins <- sum(results[1, ] > results[2, ])
  bob_wins <- sum(results[2, ] > results[1, ])
  draws <- num_experiments - alice_wins - bob_wins

  return(list(alice_wins = alice_wins, bob_wins = bob_wins, draws = draws))
}


# Проведение экспериментов для разных значений n
ns <- c(10, 50, 100, 500, 1000)
num_experiments <- 1000

results <- lapply(ns, function(n) {
  simulate_experiments(n, num_experiments)
})
str
# Вывод результатов
results <- do.call(rbind, lapply(seq_along(ns), function(i) {
  c(n = ns[i], results[[i]])
}))

print(results)