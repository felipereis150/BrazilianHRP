medidas <- function(x, rf = 0.5) {
  # Annualized Average
  AV <- mean(x)
  # Annualized SD
  SD <- sd(x)
  # Information (or Sharpe) Ratio
  SR <- (mean(x) - rf)/sd(x)
  # Adjusted Sharpe Ratio
  ASR <- SR*(1 + (moments::skewness(x)/6)*SR - ((moments::kurtosis(x) - 3)/24)*SR^2)
  # Sortino Ratio
  SO <- (mean(x) - rf)/sqrt(mean(ifelse(x - rf < 0, 0, (x - rf)^2)))
  output <- c(12*AV, sqrt(12)*SD, sqrt(12)*SR, sqrt(12)*ASR, sqrt(12)*SO)
  return(output)
}

calculate_to <- function(previous_weights, desired_weights, oos_returns) {
    num <- previous_weights * (1 + oos_returns/100)
    den <- sum(num)
    updated_weights <- num/den
    to <- sum(abs(desired_weights - updated_weights))
  return(to)
}