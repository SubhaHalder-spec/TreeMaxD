#' Max-D test for testing equality means against tree-ordered alternatives in one-way ANOVA
#' @export
#' @param sample_data list
#' @param significance_level numeric
#' @return Critical value numeric
#' @return Test statistic value numeric
#' @return Result Character
#' @details Testing of H_0:mu_0 = mu_1 = ... = mu_k vs H_1:mu_0 <= mu_i (at least one strict inequality), forall i >= 1, where mu_i represents the population means of the i-th treatment. The input consists of two variables: sample_data and significance_level. The output consists of the critical value, the Max-D test statistic value, and the result, which indicates whether to reject or not reject the null hypothesis.
#' @import stats
#' @author Subha Halder
TreeMaxD <- function(sample_data, significance_level){
  set.seed(456)
  sample_data <- lapply(sample_data, function(x) x[!is.na(x)])
  num_samples = 20000
  num_datasets <- length(sample_data)
  n <- sapply(sample_data, length)
  D_star_max <- numeric(num_samples)
  for (i in 1:num_samples) {
    bootstrap_samples <- lapply(sample_data, function(x) rnorm(n = length(x), mean = 0, sd = sqrt(var(x))))
    D_star_max[i] <- max(sapply(2:num_datasets, function(j) {
      (mean(bootstrap_samples[[j]]) - mean(bootstrap_samples[[1]])) /
        sqrt((var(bootstrap_samples[[j]]) / length(bootstrap_samples[[j]])) +
               (var(bootstrap_samples[[1]]) / length(bootstrap_samples[[1]])))
    }))
  }
  sort_D_star_max <- sort(D_star_max)
  quantile_value <- quantile(sort_D_star_max, probs = 1 - significance_level)
  D_Max <- max(sapply(2:num_datasets, function(i) {
    (mean(sample_data[[i]]) - mean(sample_data[[1]])) /
      sqrt(
        (var(sample_data[[i]]) / length(sample_data[[i]])) +
          (var(sample_data[[1]]) / length(sample_data[[1]]))
      )
  }))
  if (D_Max > quantile_value) {
    result <- "Reject null hypothesis"
  } else {
    result <- "Do not reject null hypothesis"
  }
  return(paste("Critical value:", quantile_value, "; D_Max Test statistic:", D_Max, "; Result:", result))
}
