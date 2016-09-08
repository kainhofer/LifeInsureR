## ----echo = FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(pander)
library(LifeInsuranceContracts)


## Modified pandoc.list function that also works with NULL entries in the lists:
pandoc.listRK.return <- function(elements, style = c('bullet', 'ordered', 'roman'), loose = FALSE, add.line.breaks = TRUE, add.end.of.list = TRUE, indent.level = 0, missing = panderOptions('missing')) { #nolint

    ## checks
    if (!is.logical(loose)) {
        stop('Wrong argument provided: loose')
    }

    ## default values
    if (missing(style)) {
        style <- panderOptions('list.style')
    } else {
        style <- match.arg(style)
    }

    ## replace missing values
    w <- which(is.na(elements))
    if (length(w) > 0) {
        elements[w] <- missing
    }

    ## helpers
    elements.l <- length(elements)
    marker     <- switch(style,
                         'bullet'  = rep('* ', elements.l),
                         'ordered' = paste0(1:elements.l, '. '),
                         'roman'   = paste0(as.roman(1:elements.l), '. '))

    ## number of elements should be more than one
    if (elements.l == 0) {
        return('')
    }

    ## recursive call
    i.lag <- 0
    res <- ifelse(add.line.breaks, '\n', '')
    nms = names(elements)
    for (i in 1:elements.l) {
        res <- paste0(res, paste(rep(' ', indent.level * 4), collapse = ''), marker[i - i.lag])
        if (nms[[i]] != "") {
            res <- paste0(res, nms[[i]], ': ')
        }

        if (length(elements[[i]]) <=1 && !is.list(elements[[i]])) {
            res <- paste0(res, elements[[i]], '\n')
        } else {
            i.lag <<- i.lag + 1
            res <- paste0(res, '\n', pandoc.listRK.return(elements[[i]], style, loose, FALSE, FALSE, indent.level + 1))
        }
        res <- paste0(res, ifelse(loose, '\n', ''))
    }

    # res <- paste(sapply(1:elements.l, function(i) {
    #     if (length(elements[[i]]) <= 1 && !is.list(elements[[i]])) {
    #         paste0(paste(rep(' ', indent.level * 4), collapse = ''), marker[i - i.lag], elements[[i]])
    #     } else {
    #         i.lag <<- i.lag + 1
    #         pandoc.listRK.return(elements[[i]], style, loose, FALSE, FALSE, indent.level + 1)
    #     }}),
    #     collapse = '\n', ifelse(loose, '\n', ''))

    ## closing tag
    if (add.end.of.list) {
        res <- paste0(res, ifelse(loose, '', '\n\n'), '<!-- end of list -->\n')
    }
    if (add.line.breaks) {
        res <- add.blank.lines(res)
    }

    return(res)

}

#' @export
pandoc.listRK <- function(...)
    cat(pandoc.listRK.return(...))





## ------------------------------------------------------------------------
str(InsuranceContract.ParameterDefaults)

## ---- results="asis"-----------------------------------------------------

#pandoc.listRK(InsuranceContract.ParameterDefaults)

