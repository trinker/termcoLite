#' Search For and Count Terms
#'
#' \code{term_count} - Search a string by any number of grouping variables for
#' categories (themes) of grouped root terms/substrings.
#'
#' @param text.var The text string variable.
#' @param grouping.var The grouping variable(s).  Default \code{NULL} generates
#' one word list for all text.  Also takes a single grouping variable or a list
#' of 1 or more grouping variables.  If \code{TRUE} an \code{id} variable is
#' used with a \code{seq_along} the \code{text.var}.
#' @param term.list A list of named character vectors.  `code{term_count} can
#' be used in a hierarchical fashion as well; that is a list of regexes can be
#' passed and counted and then a second (or more) pass can be taken wit a new
#' set of regexes on only those rows/text elements that were left untagged
#' (count \code{\link[base]{rowSums}} is zero).  This is accomplished by passing
#' a \code{\link[base]{list}} of \code{\link[base]{list}}s of regexes.
#' See \bold{Examples} for the \strong{hierarchical terms} section for a
#' demonstration.
#' @param ignore.case logical.  If \code{FALSE}, the pattern matching is case
#' sensitive and if \code{TRUE}, case is ignored during matching.
#' @param pretty logical.  If \code{TRUE} pretty printing is used.  Pretty
#' printing can be turned off globally by setting
#' \code{options(termco_pretty = FALSE)}.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots ignored.
#' @return Returns a \code{\link[dplyr]{tibble}} object of term counts by
#' grouping variable.
#' @keywords term substring
#' @rdname term_count
#' @importFrom data.table := .SD
#' @export
term_count <- function(text.var, grouping.var = NULL, term.list,
    ignore.case = TRUE, pretty = ifelse(isTRUE(grouping.var), FALSE, TRUE),
     group.names, ...){

    amodel <- FALSE
    auto_map <- FALSE

    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                x[length(x)]
            }
            )
        } else {
            if (isTRUE(grouping.var)) {
                G <- "id"
                amodel <- TRUE
            } else {
                G <- as.character(substitute(grouping.var))
                G <- G[length(G)]
            }
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (isTRUE(grouping.var)) {
            grouping <- seq_along(text.var)
        } else {
            if (is.list(grouping.var) & length(grouping.var)>1) {
                grouping <- grouping.var
            } else {
                grouping <- unlist(grouping.var)
            }
        }
    }

    if(!missing(group.names)) {
        G <- group.names
    }

    DF <- data.frame(text.var, check.names = FALSE, stringsAsFactors = FALSE)
    DF[G] <- grouping
    DF['n.words'] <- stringi::stri_count_words(text.var)

    DF <- data.table::setkeyv(data.table::data.table(DF), G)

    ## check for hierarchical terms
    list_list <- FALSE
    if (is.list(term.list[[1]]) && length(term.list) > 1 && all(sapply(term.list, is.list))) {


        ## make sure for hierarchical terms that each observation is also a group
        if(nrow(DF) != nrow(unique(DF[,G, with=FALSE]))) {
            stop("In order to run nested `term.list` then `grouping.var` must place every observation in its own group.")
        }

        list_list <- TRUE

        #out_list <- vector(mode = "list", length = length(term.list))
        #inds  <- vector(mode = "list", length = length(term.list))
        term.list <- lapply(term.list, term_lister_check, G)

        ## Auto create a map for same named term lists and
        ## add ending number to distinguish
        term.nms <- lapply(term.list, names)
        term.lens <- sapply(term.nms, length)
        term.nms <- unlist(term.nms)

        if (any(duplicated(term.nms))){

            map <- as.list(unique(term.nms))
            names(map) <- unique(term.nms)

            for(i in names(map)){
                suffix <- seq_len(sum(term.nms == i))
                if (length(suffix) == 1) {
                    replacements <- i
                    map[i] <- NULL
                } else {
                    replacements <- paste(i, seq_len(sum(term.nms == i)), sep = "__")
                    map[[i]] <- paste(i, seq_len(sum(term.nms == i)), sep = "__")
                }
                term.nms[term.nms == i] <- replacements
            }

            term.list <- Map(function(x, y) {
                names(x) <- y
                x
            }, term.list, split(term.nms, rep(seq_along(term.lens), term.lens)))

            auto_map <- TRUE

        }



        inds <- seq_along(text.var)

        for (i in seq_along(term.list)){

            if (i == 1){
                counts <- data.table::setkeyv(
                    data.table::copy(data.table::setDT(DF))[inds, ][,
                        names(term.list[[i]]):= lapply(term.list[[i]], countfun,
                        text.var, ignore.case = ignore.case), ][, 'text.var':=NULL],
                    G
                )
            } else {

                counts <- merge(
                    counts,
                    data.table::setkeyv(data.table::copy(data.table::setDT(DF))[inds, ][,
                        names(term.list[[i]]):= lapply(term.list[[i]], countfun,
                        text.var, ignore.case = ignore.case), ][, 'text.var':=NULL][,
                        'n.words' := NULL], G),
                    all=TRUE
                )

            }

            terminds <- (1 + which(colnames(counts) == "n.words")):ncol(counts)
            inds <- which(rowSums(counts[, terminds, with = FALSE]) == 0)
        }


        term.cols <- colnames(counts)[(1 + which(colnames(counts) == "n.words")):ncol(counts)]
        for (i in term.cols) eval(parse(text=paste("counts[,",i,":=na.replace(",i,")]")))
        out <- counts[,lapply(.SD, sum, na.rm = TRUE), keyby = G]


    } else {
        term.list <- term_lister_check(term.list, G)

        counts <- data.table::setDT(DF)[, names(term.list):= lapply(term.list, countfun,
            text.var, ignore.case = ignore.case), ][, text.var:=NULL]

        out <- counts[,lapply(.SD, sum, na.rm = TRUE), keyby = G]
    }

    text <- new.env(hash=FALSE)
    text[["text.var"]] <- text.var

    count <- new.env(hash=FALSE)
    count[["count"]] <- counts

    regex <- new.env(hash=FALSE)
    regex[["term.list"]] <- term.list

    out <- dplyr::tibble(out)
    class(out) <- c("term_count", class(out))

    if(isTRUE(list_list)) class(out) <- c("hierarchical_term_count", class(out))

    attributes(out)[["group.vars"]] <- G
    if (isTRUE(list_list)) {
        attributes(out)[["term.vars"]] <- unlist(lapply(term.list, names))
    } else {
        attributes(out)[["term.vars"]] <- names(term.list)
    }

    attributes(out)[["weight"]] <- "count"
    attributes(out)[["pretty"]] <- pretty
    attributes(out)[["counts"]] <- count
    attributes(out)[["text.var"]] <- text
    attributes(out)[["model"]] <- amodel
    attributes(out)[["regex"]] <- regex

    if(isTRUE(list_list)) attributes(out)[["hierarchical_terms"]] <- lapply(term.list, names)

    if (isTRUE(auto_map)){
        message("Collapsing duplicate `term.list` columns.")
        out <- collapse_tags(out, map, ...)
    }

    out
}


na.replace <- function(v, value=0) { v[is.na(v)] <- value; v }
mymerge <-  function(x, y) merge(x, y, all=TRUE)


term_lister_check <- function(term.list, G){
    if(any(G %in% names(term.list))) stop("`grouping` names cannot be used as `term.list` names")

    nms <- names(term.list)
    names(term.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms,
        identical, "")])))

    if (!is.list(term.list)) {
        warning("Expecting a named list for `term.list`; coercing to list.")
        term.list <- as.list(term.list)
        if (is.null(names(term.list))) term.list <- stats::setNames(term.list, term.list)
    } else {
        term.list <- lapply(term.list, function(x) paste(paste0("(", x, ")"), collapse = "|"))
    }
    term.list
}

na.replace <- function(v, value=0) { v[is.na(v)] <- value; v }
mymerge <-  function(x, y) merge(x, y, all=TRUE)


term_lister_check <- function(term.list, G){
    if(any(G %in% names(term.list))) stop("`grouping` names cannot be used as `term.list` names")

    nms <- names(term.list)
    names(term.list)[sapply(nms, identical, "")] <- make.names(seq_len(length(nms[sapply(nms,
        identical, "")])))

    if (!is.list(term.list)) {
        warning("Expecting a named list for `term.list`; coercing to list.")
        term.list <- as.list(term.list)
        if (is.null(names(term.list))) term.list <- stats::setNames(term.list, term.list)
    } else {
        term.list <- lapply(term.list, function(x) paste(paste0("(", x, ")"), collapse = "|"))
    }
    term.list
}



