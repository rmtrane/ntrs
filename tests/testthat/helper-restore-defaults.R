# Shared helper: snapshot current default for scores_class and restore it after
# the test (removing the key entirely if it did not exist before).
local_restore_default <- function(scores_class, env = parent.frame()) {
  defaults_env <- get(
    ".std_defaults",
    envir = asNamespace("NpsychBatteryNormsS3")
  )

  prior_exists <- exists(scores_class, envir = defaults_env, inherits = FALSE)
  prior_value <- if (prior_exists) defaults_env[[scores_class]] else NULL

  # Also remove the key NOW so the test starts clean
  if (prior_exists) {
    rm(list = scores_class, envir = defaults_env)
  }

  withr::defer(
    {
      if (prior_exists) {
        defaults_env[[scores_class]] <- prior_value
      } else if (exists(scores_class, envir = defaults_env, inherits = FALSE)) {
        rm(list = scores_class, envir = defaults_env)
      }
    },
    envir = env
  )
}
