#' Configure LifeInsureR debug options interactively
#'
#' Uses a Shiny checkbox GUI if {shiny} is available (e.g., in RStudio),
#' otherwise falls back to a text-based multi-select list.
#'
#' - Preselects options that are currently TRUE.
#' - Provides Select all / Select none controls (Shiny UI).
#' - Provides a Cancel button that exits without changes.
#'
#' The selected options will be set to TRUE, all un-selected options will be set to FALSE.
#'
#' @param title Dialog title.
#' @return Invisibly, a named logical vector of all keys and their values after setting;
#'   returns NULL if cancelled.
#' @export
LIC_debug_configure <- function(title = "LifeInsureR debug options") {
  if (!interactive()) stop("LIC_debug_configure() is interactive.")

  keys <- get0("LIC_debug_options", envir = asNamespace("LifeInsureR"), inherits = FALSE)
  if (is.null(keys) || !length(keys)) stop("No debug options index found (LIC_debug_options).")

  # current values (preselect TRUE)
  cur_vals <- vapply(keys, function(k) isTRUE(getOption(k, FALSE)), logical(1))
  preselect <- keys[cur_vals]

  set_all_options <- function(selected_keys) {
    vals <- setNames(rep(FALSE, length(keys)), keys)
    vals[selected_keys] <- TRUE
    do.call(options, as.list(vals))
    invisible(vals)
  }

  # Prefer Shiny if available
  if (requireNamespace("shiny", quietly = TRUE)) {
    selected = c()
    cancelled = TRUE

    app <- shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::h3(title),

        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::actionButton("all", "Select all"),
            shiny::actionButton("none", "Select none"),
            shiny::tags$span(style = "display:inline-block; width: 12px;"),
            shiny::actionButton("ok", "OK"),
            shiny::actionButton("cancel", "Cancel")
          )
        ),

        shiny::tags$hr(),

        shiny::checkboxGroupInput(
          "opts",
          label = NULL,
          choices = keys,
          selected = preselect
        )
      ),
      server = function(input, output, session) {
        shiny::observeEvent(input$all, {
          shiny::updateCheckboxGroupInput(session, "opts", selected = keys)
        })

        shiny::observeEvent(input$none, {
          shiny::updateCheckboxGroupInput(session, "opts", selected = character())
        })

        shiny::observeEvent(input$ok, {
          selected <<- input$opts
          cancelled <<- FALSE
          shiny::stopApp()
        })

        shiny::observeEvent(input$cancel, {
          cancelled <<- TRUE
          shiny::stopApp()
        })
      }
    )

    shiny::runApp(app)

    if (cancelled) return(invisible(NULL))
    return(set_all_options(selected))
  }

  # Fallback: text-based multi-select (no cancel => treat empty selection as OK)
  chosen <- utils::select.list(
    choices = keys,
    preselect = preselect,
    multiple = TRUE,
    title = paste0(title, " (text mode)")
  )
  set_all_options(chosen)
}




# NOTE: This function was written with the help of ChatGPT
#' Extract LIC debug option keys used in this package
#'
#' Scans the package's R/ sources for "LIC.debug...." option string literals (getOption called on the option).
#' Intended for use in data-raw scripts and tests.
#'
#' @param pkg_path Path to the package root (contains DESCRIPTION and R/).
#' @return Character vector of unique option keys, sorted.
#' @keywords internal
extract_lic_debug_options = function(pkg_path = ".") {
  r_dir <- file.path(pkg_path, "R")
  if (!dir.exists(r_dir)) stop("Cannot find R/ directory under: ", normalizePath(pkg_path))

  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  if (!length(r_files)) return(character())

  # Match getOption with "LIC.debug.xxx" or 'LIC.debug.xxx'
  rx <- "getOption\\s*\\(\\s*(['\"])(LIC\\.debug\\.[^'\"]+)\\1"

  hits <- character()
  for (f in r_files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- gregexpr(rx, txt, perl = TRUE)
    if (m[[1]][1] == -1) next
    found <- regmatches(txt, m)[[1]]
    opt <- sub(rx, "\\2", found, perl = TRUE)
    hits <- c(hits, opt)
  }

  sort(unique(hits))
}
