#' Make Regex to Locate Strings Containing Co-ocuring Substrings
#'
#' Make a regex to locate strings that contain >= 2 substrings with optional
#' negation.
#'
#' @param \ldots Terms that cooccur/collocate
#' @param not A substring to exclude from consideration.
#' @return Returns a regular expression.  If Windows attempts to copy to
#' clipboard as well.
#' @keywords collocate cooccur
#' @export
#' @examples
#' colo('dog', 'cat')
colo <- function(..., not=NULL) {
    if (is.null(not)){
          if (length(substitute(...())) == 1) {
              return(substitute(...())[[1]])
          }
    	  cooc(...)
    } else {
    	  cooc_not(..., not=not)
    }
}


cooc <- function(...){

    x <- substitute(...())

    if (length(x) == 2) {
        z <- sprintf("((%s.*%s)|(%s.*%s))", x[1], x[2], x[2], x[1])
    } else {
        z <- paste0("^", paste(sprintf("(?=.*%s)", x), collapse=""))
    }
    z
}



cooc_not <- function(..., not){
    x <- substitute(...())
    z <- paste0(sprintf("^(?!.*(%s))", not), paste(sprintf("(?=.*(%s))", x), collapse=""))
    z
}




