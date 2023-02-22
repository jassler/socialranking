library(kableExtra)
createTables <- function(df, name, col.names = NA, extras = function(kb) {kb}) {
  # replace `...` with \texttt{...}, add line breaks to latex document
  dfLat <- apply(df, 2, function(x) kableExtra::linebreak(
    gsub("`([^`]+)`", "\\\\texttt{\\1}", x), linebreaker = "<br>", align = "l"
  ))
  df <- apply(df, 2, function(x) gsub("`([^`]+)`", "<code>\\1</code>", x))

  if(length(col.names) == 1 && is.na(col.names)) {
    col.names <- gsub("\\.", " ", colnames(df))
  }
  col.namesLat <- gsub("`([^`]+)`", "\\\\texttt{\\1}", col.names)
  col.names <- gsub("`([^`]+)`", "<code>\\1</code>", col.names)

  # create html table
  kb <- kableExtra::kbl(
    df,
    col.names = col.names, format = 'html', escape = FALSE, booktabs = TRUE
  ) %>% kableExtra::kable_styling(bootstrap_options = c("hover"), latex_options = c("scale_down")) %>% kableExtra::row_spec(1:(nrow(df)-1), hline_after = TRUE)
  kb <- extras(kb)

  # create latex table
  kbLat <- kableExtra::kbl(
    dfLat,
    col.names = col.namesLat, format = 'latex', escape = FALSE, booktabs = TRUE
  ) %>% kableExtra::kable_styling(bootstrap_options = c("hover"), latex_options = c("scale_down")) %>% kableExtra::row_spec(1:(nrow(df)-1), hline_after = TRUE)
  kbLat <- extras(kbLat)

  # output files
  print(paste0(">>> >>> Generating vignettes/tables/", name, ".(html|tex) <<< <<<"))
  cat(kb, file = paste0('vignettes/tables/', name, '.html'))
  cat('\n', file = paste0('vignettes/tables/', name, '.html'), append = TRUE)
  cat(kbLat, file = paste0('vignettes/tables/', name, '.tex'))
  cat('\n', file = paste0('vignettes/tables/', name, '.tex'), append = TRUE)
}

local({
  print(">>> Generating Tables with kableExtra <<<")

  tbl <- data.frame(
    "Comparison functions" = c("`dominates()`", "`cumulativelyDominates()`", "`cpMajorityComparison()`<br>`cpMajorityComparisonScore()`", "", "", ""),
    "Score functions" = c("", "`cumulativeScores()`", "`copelandScores()`<br>`kramerSimpsonScores()`", "`lexcelScores()`", "`L1Scores()`", "`ordinalBanzhafScores()`"),
    "Ranking functions" = c("", "", "`copelandRanking()`<br>`kramerSimpsonRanking()`", "`lexcelRanking()`<br>`dualLexcelRanking()`", "`L1Ranking()`", "`ordinalBanzhafRanking()`")
  )
  createTables(tbl, "functionTable")

  tbl <- data.frame(
    c("`elements`", "`eqs`", "`coalitionLookup`", "`elementLookup`"),
    c("Sorted vector of elements", "List containing lists, each<br>containing coalitions in the<br>same equivalence class", "Function to determine a coalition's<br>equivalence class index", "Function to determine, which coalitions<br>an element takes part in"),
    c("`c(1,2)`", "`list(list(c(1,2)),`<br>`list(c(2), c()),`<br>`list(c(1)))`", "`function(coalition)`", "`function(element)`")
  )
  createTables(tbl, "prObject", col.names = c("Attribute", "Description", "Value in `pr`"))

  print(">>> Reading vignette <<<")
  s <- '  rmarkdown::html_document:'
  e <- '  rmarkdown::pdf_document:'

  lines <- readLines('vignettes/socialranking.Rmd')
  range <- which(lines == s | lines == e)

  if(length(range) != 2) {
    stop(paste0("Expected to find one line with '", s, "' and one line with '", e, "'. Instead found ", length(range), " lines:\n  ", paste(range, collapse = "\n  ")))
  }

  if(lines[range[1]] != s || lines[range[2]] != e) {
    stop(paste0("Expected to find '", s, "' in line ", range[1], " and '", e, "' in line ", range[2], "."))
  }

  entry <- which(startsWith(lines, '  %\\VignetteIndexEntry'))
  if(length(entry) != 1) {
    stop(paste("Found", length(entry), " entries of VignetteIndexEntry, expected 1"))
  }

  print(paste(">>> Deleting lines", range[1], "to", range[2]-1, "<<<"))
  print(paste(">>> Found vignette entry in line", entry, "<<<"))

  lines[entry] <- '  %\\VignetteIndexEntry{socialranking vignette pdf}'
  lines <- gsub("xfun::file_string\\((?:'|\")vignettes/([^'\"]+)(?:'|\")\\)", "xfun::file_string('\\1')", lines)


  html_sections <- which(lines == '````{=html}')
  print(paste(">>> Changing the following lines from ````{=html} to ````{=latex}:", paste(html_sections, collapse = ", "), "<<<"))
  lines[html_sections] <- '````{=latex}'

  html_sections <- which(startsWith(lines, 'xfun::file_string'))
  print(paste(">>> Making sure the following lines look for .tex files instead of .html files:", paste(html_sections, collapse = ", "), "<<<"))

  for(i in html_sections) {
    lines[i] <- stringr::str_replace_all(lines[i], '\\.html"', '\\.tex"')
    lines[i] <- stringr::str_replace_all(lines[i], "\\.html'", "\\.tex'")
  }


  print(">>> Writing lines to vignettes/socialranking_pdf.Rmd <<<")
  writeLines(lines[-(range[1]:(range[2]-1))], 'vignettes/socialranking_pdf.Rmd')

#  devtools::build_vignettes()
#
#  print(">>> Compacting doc/prebuilt.pdf <<<")
#  result <- tools::compactPDF("doc/prebuilt.pdf", gs_quality = "ebook")
#
#  if(is.null(result)) {
#    warning("tools::compactPDF returned NULL. Make sure ghostscript is available and listed in PATH.")
#  } else {
#    print(result)
#  }
#
#  print(">>> Moving (hopefully) compacted pdf to vignettes directory <<<")
#
#  file.rename('doc/prebuilt.pdf', 'vignettes/prebuilt.pdf')
#  file.remove('vignettes/prebuilt.Rmd')

  print(">>> Done <<<")
})

