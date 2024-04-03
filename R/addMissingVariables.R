#' add variables that are missing based on a list of formulas
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param variables the list of requested variables
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom quitte as.quitte calc_addVariable_
#' @importFrom tibble as_tibble
#' @return quitte object with adapted mif data
#' @export

addMissingVariables <- function(mifdata, variables, logFile = NULL) {
  mifdata <- as.quitte(mifdata)

  toadd <- unique(setdiff(variables, levels(mifdata$variable)))
  formulas <- system.file("addMissingVariables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#") %>%
    as_tibble() %>%
    filter(.data$piam_variable %in% toadd)
  # we need to loop because the second calculation might need data from the first
  for (i in seq_len(nrow(formulas))) {
    liste <- list(c(formulas$formula[[i]], formulas$piam_unit[[i]]))
    names(liste) <- formulas$piam_variable[[i]]
    mifdata <- as.quitte(calc_addVariable_(mifdata, liste, only.new = FALSE, skip.missing.rhs = TRUE))
  }

  added <- intersect(levels(mifdata$variable), toadd)
  if (length(added) > 0 && ! is.null(logFile) && ! isFALSE(logFile)) {
    logtext <- c("Automatically added variables: ", paste(added, collapse = ", "))
    write(logtext, file = logFile, append = TRUE)
  }

  return(mifdata)
}
