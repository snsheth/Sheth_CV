## cv_helpers.R
## Tidyverse helper functions that read data/*.csv and emit ready-to-paste
## LaTeX for the Sheth_CV.Rnw document. Each function returns a single string;
## call it inside a knitr chunk with cat() and chunk option results='asis'.
##
## The design principle: every "list of similar things" section of the CV
## (publications, grants, presentations, service, etc.) lives in its own CSV
## in data/. To add, remove, or edit an entry, edit the CSV -- never the .Rnw.

## ---- Auto-install any missing R packages -----------------------------
## This is the #1 cause of "works on my computer, fails on another": a
## bare library(dplyr) just stops with an error if dplyr isn't installed
## on whatever machine/R version you're currently on. This checks for each
## required package and installs it from CRAN automatically if missing,
## so knitting on a new computer "just works" the first time (aside from
## needing an internet connection for that one-time install).
required_packages <- c("dplyr", "readr", "glue", "stringr", "tidyr", "tinytex", "rstudioapi")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  message("Installing missing R packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

## ---- Auto-install TinyTeX and every LaTeX package this document needs ----
## Same idea, one level down: this used to be something you had to remember
## to run separately (compile_cv.R). Now it happens automatically every time
## you knit -- normally instantly, since it's just checking a list -- so
## just opening the .Rnw and hitting Knit is the robust path, not a
## secondary option you have to remember to use instead.
##
## Wrapped in tryCatch so that if this specific check can't run (e.g. no
## internet connection right now), knitting still proceeds instead of
## failing harder than it would have without this whole block -- LaTeX will
## give its own normal error later if a package genuinely is missing.
tryCatch({
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    install.packages("tinytex", repos = "https://cloud.r-project.org")
  }
  if (!tinytex::is_tinytex()) {
    message("TinyTeX not found -- installing (first time only, can take a few minutes)...")
    tinytex::install_tinytex()
  }
  required_latex_packages <- c(
    "fontenc", "xcharter", "fontaxes", "microtype", "needspace", "inputenc",
    "geometry", "array", "fancyhdr", "lastpage", "titlesec", "caption",
    "hanging", "babel", "hyphen-english", "hyperref", "xcolor", "tabularx",
    "xltabular", "ltxcmds", "etoolbox", "kvoptions", "pdftexcmds"
  )
  already_installed <- tinytex::check_installed(required_latex_packages)
  missing_latex <- required_latex_packages[!already_installed]
  if (length(missing_latex) > 0) {
    message("Installing missing LaTeX packages: ", paste(missing_latex, collapse = ", "))
    tinytex::tlmgr_install(missing_latex)
  }
}, error = function(e) {
  warning("Could not verify/install LaTeX packages automatically (",
          conditionMessage(e), "). Continuing to knit anyway -- if the PDF ",
          "compile step fails afterward, this is the first thing to check.")
})

library(dplyr)
library(readr)
library(glue)
library(stringr)

## ---- Simple numbered list (Publications) ----------------------------------
## data/publications.csv has columns: number, authors, year, title, journal,
## volume_pages, doi, note
##
## `authors` is plain text -- just type names the way you'd write them in any
## citation, separated by commas with "and" before the last one, exactly as
## you're used to. The ONLY markup is for yourself and any trainee co-authors:
## wrap the name in [square brackets] and put the role right after in
## (parentheses):
##   [Sheth, S. N.](self)        -- bolds your own name
##   [L. J. Albano](p)           -- underlines + adds the postdoc-associate
##                                   superscript (u/g/p/t, or combine like
##                                   (u,g) for someone who was both)
## Everything else in `authors` (co-authors with no role) is typed completely
## plain -- no backslashes, no braces.
##
## `journal` is typed plain (no italics needed -- added automatically).
## `volume_pages` is whatever comes after the journal name, e.g. "53: 87-111"
## or "16: e70197" -- leave blank if not applicable (e.g. an in-press paper).
## `doi` is just the full https://doi.org/... URL, or blank.
## `note` is an optional trailing remark, e.g. "(Runner-up for X Award)" --
## it's bolded automatically. Leave blank if there isn't one.
##
## If a title needs an italicized species name, you can still use
## \emph{...} directly inside the title field for that one word -- that's
## the one case this format doesn't try to automate, since it's rare.
## ---- Shared: [Name](role) author markup -----------------------------
## Converts the plain-text convention used across pub/talk CSVs:
##   [Sheth, S. N.](self)  -> \textbf{Sheth, S. N.}
##   [L. J. Albano](p)     -> \underline{L. J. Albano}\textsuperscript{p}
## (role can combine multiple codes, e.g. (u,g))
format_author_markup <- function(a) {
  a <- str_replace_all(a, "\\[([^\\[\\]]+)\\]\\(self\\)", "\\\\textbf{\\1}")
  a <- str_replace_all(a, "\\[([^\\[\\]]+)\\]\\(([a-z,]+)\\)",
                       "\\\\underline{\\1}\\\\textsuperscript{\\2}")
  a
}

format_publications <- function(csv_path = "data/publications.csv") {
  pubs <- read_csv(csv_path, show_col_types = FALSE) |>
    arrange(desc(number)) |>
    mutate(
      volume_pages = tidyr::replace_na(volume_pages, ""),
      doi = tidyr::replace_na(doi, ""),
      note = tidyr::replace_na(note, "")
    )

  n <- nrow(pubs)

  citations <- glue(
    "{format_author_markup(pubs$authors)}. {pubs$year}. {pubs$title}. ",
    "\\emph{{{pubs$journal}}}",
    "{ifelse(nzchar(pubs$volume_pages), paste0(' ', pubs$volume_pages, '.'), '.')}",
    "{ifelse(nzchar(pubs$doi), paste0(' \\\\url{', pubs$doi, '}.'), '')}",
    "{ifelse(nzchar(pubs$note), paste0(' \\\\textbf{', pubs$note, '}'), '')}"
  )

  items <- glue("\\item {citations}")

  glue(
    "\\begin{{hangparas}}{{.5in}}{{1}}\n",
    "\\begin{{enumerate}}\n",
    "\\reverselabel{{{n}}} %set to total # of publications -- now automatic\n\n",
    "{paste(items, collapse = '\n\n')}\n\n",
    "\\end{{enumerate}}\n",
    "\\end{{hangparas}}\n",
    "\\vspace{{-1.95ex}} % hangparas adds its own closing padding; cancel it for consistent heading spacing\n"
  )
}

## ---- Contributed conference presentations ---------------------------------
## data/contributed_presentations.csv has columns:
##   authors, title, venue, year, note
## `authors` uses the same [Name](role) convention as publications.
## `title` is plain text (quotes are added automatically) -- may contain
## \emph{...} directly for an italicized species name, same exception as
## publications.
## `venue` is everything after the title: conference name, location, and
## any "(presented by ...)" note -- all plain text.
## `note` is optional -- used for things like "Winner of Best Graduate
## Student Poster" -- leave blank if there isn't one.
format_contributed_presentations <- function(csv_path = "data/contributed_presentations.csv",
                                              col_width = "5.25in") {
  entries <- read_csv(csv_path, show_col_types = FALSE) |>
    mutate(note = tidyr::replace_na(note, ""))

  main_rows <- glue(
    "\\hangindent=5ex \\ {format_author_markup(entries$authors)}. ",
    "``{entries$title}'' {entries$venue} & {entries$year}"
  )

  row_blocks <- ifelse(
    nzchar(entries$note),
    glue("{main_rows} \\tabularnewline\n",
         "\\hspace{{7mm}} *** \\textbf{{{entries$note}}} ***"),
    main_rows
  )

  glue(
    "\\renewcommand{{\\arraystretch}}{{1.05}}\n",
    "\\begin{{xltabular}}{{\\textwidth}}{{@{{}}>{{\\raggedright}}p{{{col_width}}} >{{\\raggedleft}}X@{{}}}}\n\n",
    "{paste(row_blocks, collapse = ' \\\\tabularnewline\\n\\n')} \\tabularnewline\n\n",
    "\\end{{xltabular}}\n"
  )
}
## data/manuscripts_in_review.csv has one column: citation
## Note: each citation in the CSV already ends with its own line-break
## command (it was copied verbatim from the original document), so entries
## are joined with a blank line only -- no extra separator is added.
format_hangpara_list <- function(csv_path) {
  entries <- read_csv(csv_path, show_col_types = FALSE)
  glue(
    "\\begin{{hangparas}}{{.5in}}{{1}}\n\n",
    "{paste(entries$citation, collapse = '\n\n')}\n\n",
    "\\end{{hangparas}}\n",
    "\\vspace{{-0.75ex}} % hangparas adds its own closing padding; cancel it for consistent heading spacing\n"
  )
}

## ---- Invited symposia -----------------------------------------------------
## data/invited_symposia.csv has columns: venue, year, symposium, title, authors
## `venue` is the conference/meeting name and location.
## `symposium` is the session/symposium name (e.g. "Symposium: Reproductive
## ecology and evolution..."), typed however you'd like it to read.
## `title` is your talk title, plain text (quotes added automatically).
## `authors` is optional -- leave blank for a solo talk. Otherwise plain
## text using the same [Name](role) convention as publications, e.g.
## "[Sheth, S. N.](self) and 24 co-authors" or a full author list ending
## in "presented by X".
format_invited_symposia <- function(csv_path = "data/invited_symposia.csv",
                                     col_width = "5.25in") {
  entries <- read_csv(csv_path, show_col_types = FALSE) |>
    mutate(authors = tidyr::replace_na(authors, ""))

  title_line <- glue(
    "``{entries$title}''",
    "{ifelse(nzchar(entries$authors), paste0(' (', format_author_markup(entries$authors), ')'), '')}"
  )

  line1 <- glue("{entries$venue} & {entries$year} \\tabularnewline")
  line2 <- glue("\\addtolength{{\\leftskip}}{{5ex}} {entries$symposium} \\tabularnewline")
  line3 <- glue("\\addtolength{{\\leftskip}}{{5ex}} {title_line}")
  row_blocks <- paste(line1, line2, line3, sep = "\n")

  glue(
    "\\renewcommand{{\\arraystretch}}{{1.05}}\n",
    "\\begin{{xltabular}}{{\\textwidth}}{{@{{}}>{{\\raggedright}}p{{{col_width}}} >{{\\raggedleft}}X@{{}}}}\n\n",
    "{paste(row_blocks, collapse = ' \\\\tabularnewline\\n\\n')} \\tabularnewline\n\n",
    "\\end{{xltabular}}\n"
  )
}

## ---- Two-column auto-breaking table (Grants, Presentations, Service, ...) -
## data/<section>.csv has one column: entry_latex
## A single entry may itself represent several physical table rows (a title
## row with a year, plus one or more indented description rows underneath).
## Those sub-rows are stored in the same CSV cell separated by " [ROWBREAK] "
## so the CSV still has exactly one row per logical CV entry.
##
## Uses xltabular (a longtable + tabularx hybrid) instead of plain tabularx,
## so the table breaks across pages automatically -- this is what removes
## the need to manually split every table into two chunks and move them
## around whenever content is added or removed.
format_entry_table <- function(csv_path, col_width = "4.9in", breakable = TRUE) {
  entries <- read_csv(csv_path, show_col_types = FALSE)

  row_blocks <- vapply(entries$entry_latex, function(e) {
    rows <- str_split(e, fixed(" [ROWBREAK] "))[[1]]
    # Wrap the trailing date/year field (everything after the last " & ") in
    # \mbox{} so it's treated as a single unbreakable unit. Without this, a
    # date range like "2025 - present" can wrap mid-range when the
    # right-aligned column ends up narrow (long entries on the left leave
    # less room on the right) -- "2025 -" on one line, "present" on the
    # next. This fixes it automatically for every row, current and future,
    # rather than requiring a manual non-breaking-space fix in the CSV.
    rows[1] <- str_replace(rows[1], "^(.*) & (.*)$", "\\1 & \\\\mbox{\\2}")
    paste(rows, collapse = " \\tabularnewline\n")
  }, character(1))

  env <- if (breakable) "xltabular" else "tabularx"

  glue(
    "\\renewcommand{{\\arraystretch}}{{1.05}}\n",
    "\\begin{{{env}}}{{\\textwidth}}{{@{{}}>{{\\raggedright}}p{{{col_width}}} >{{\\raggedleft}}X@{{}}}}\n\n",
    "{paste(row_blocks, collapse = ' \\\\tabularnewline\n\n')} \\tabularnewline\n\n",
    "\\end{{{env}}}\n"
  )
}

## ---- Plain "institution ... year" list (Invited seminars) -----------------
## data/invited_seminars.csv has columns: institution, year, note
## `note` is optional free text shown on its own indented line right below
## the entry (used here for "Graduate Student Invited Speaker").
format_seminar_list <- function(csv_path = "data/invited_seminars.csv") {
  seminars <- read_csv(csv_path, show_col_types = FALSE)
  n <- nrow(seminars)

  lines <- Map(function(institution, year, note, is_last) {
    br <- if (is_last) "" else " \\newline"
    base <- glue("{institution} \\hfill{{{year}}}{br}")
    if (!is.na(note) && nzchar(note)) {
      note_br <- if (is_last) "" else " \\tabularnewline"
      glue("{base}\n\\hspace{{7mm}} *** \\textbf{{{note}}} ***{note_br}")
    } else {
      base
    }
  }, seminars$institution, seminars$year, seminars$note, seq_len(n) == n)

  paste(unlist(lines), collapse = "\n")
}

## ---- Plain flush-left list, no second column (Professional Affiliations) --
## data/professional_affiliations.csv has one column: entry_latex
## For lists with no date/year column at all, a plain \newline list renders
## flush-left cleanly. Using the two-column table machinery here caused a
## stray indent, since the second column was never actually used.
format_plain_list <- function(csv_path) {
  entries <- read_csv(csv_path, show_col_types = FALSE)
  paste(entries$entry_latex, collapse = " \\newline\n")
}
## data/mentoring_ncsu.csv has columns: category, name, years
## `category` groups entries under a subsubsection heading (Postdoctoral
## associates, Graduate students, Undergraduate researchers, Research
## technicians, Graduate student committees) -- rows are grouped by category
## in this fixed order, and kept in CSV row order within each category.
format_mentoring_ncsu <- function(csv_path = "data/mentoring_ncsu.csv") {
  entries <- read_csv(csv_path, show_col_types = FALSE)

  category_order <- c("Postdoctoral associates", "Graduate students",
                       "Undergraduate researchers", "Research technicians",
                       "Graduate student committees")

  blocks <- lapply(category_order, function(cat) {
    rows <- entries[entries$category == cat, ]
    if (nrow(rows) == 0) return("")
    lines <- glue("{rows$name} \\hfill {{{rows$years}}}")
    body <- paste(lines, collapse = " \\newline\n")
    glue("\\subsubsection*{{{cat}}}\n{body}\n")
  })

  paste(unlist(blocks[blocks != ""]), collapse = "\n")
}

