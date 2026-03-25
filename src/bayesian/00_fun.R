timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

log_message <- function(message, model_name = NULL) {
  suffix <- if (is.null(model_name) || is.na(model_name) || !nzchar(model_name)) {
    ""
  } else {
    paste0(" for ", model_name)
  }

  cat("[", timestamp(), "] ", message, suffix, "\n", sep = "")
}
