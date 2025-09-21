#' check the yaml doc 
#'
#' @export
check_yaml <- function(f,eval.expr=FALSE){
  x <- readLines(f, warn = FALSE)
  if (length(x) >= 2 && x[1] == "---") {
    end <- which(x[-1] == "---")
    if (length(end)) {
      end <- end[1] + 1
      y <- paste(x[2:(end-1)], collapse = "\n")
      tryCatch({ yaml::yaml.load(y,eval.expr=eval_expr); NULL },
               error = function(e) data.frame(file=f, error=conditionMessage(e)))
    } else data.frame(file=f, error="No closing --- for YAML front matter")
  } else NULL
}
