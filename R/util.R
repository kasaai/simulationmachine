maybe_set_seed <- function(seed) {
  if (length(seed))
    set.seed(seed)
  else
    set.seed(NULL)
}