# Pooled standard deviation for 2 samples

spool2 <- function(s1 = NULL, s2 = NULL, n1 = NULL, n2 = NULL) {
  stopifnot(
    "s1 is missing" = !is.null(s1),
    "s2 is missing" = !is.null(s2),
    "n1 is missing" = !is.null(n1),
    "n2 is missing" = !is.null(n2),
    "s1 must be a single number" = is.numeric(s1) && length(s1) == 1,
    "s2 must be a single number" = is.numeric(s2) && length(s2) == 1,
    "n1 must be a single number" = is.numeric(n1) && length(n1) == 1,
    "n2 must be a single number" = is.numeric(n2) && length(n2) == 1,
    "s1 and s2 must be non-negative" = s1 >= 0 && s2 >= 0,
    "n1 and n2 must each be at least 2" = n1 >= 2 && n2 >= 2
  )
  
  numerator   <- (n1 - 1) * s1^2 + (n2 - 1) * s2^2
  denominator <- n1 + n2 - 2
  
  sqrt(numerator / denominator)
}