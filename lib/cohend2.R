# Cohen's d for two samples

cohend2 <- function(x1 = NULL, x2 = NULL, sp = NULL) {
  stopifnot(
    "x1 is missing" = !is.null(x1),
    "x2 is missing" = !is.null(x2),
    "sp is missing" = !is.null(sp),
    "x1 must be a single number" = is.numeric(x1) && length(x1) == 1,
    "x2 must be a single number" = is.numeric(x2) && length(x2) == 1,
    "sp must be a single number" = is.numeric(sp) && length(sp) == 1,
    "sp must be greater than zero" = sp > 0
  )
  
  (x1 - x2) / sp
}