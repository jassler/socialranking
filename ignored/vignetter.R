
local({
  print(">>> Reading vignette <<<")
  s <- '  rmarkdown::html_document:'
  e <- '  rmarkdown::pdf_document:'

  lines <- readLines('vignettes/socialranking.Rmd')
  range <- which(lines == s | lines == e)

  if(length(range) != 2) {
    stop(paste0("Expected to find one line with '", s, "' and one line with '", e, "'. Instead found ", length(range), " lines: ", paste(range, collapse = ", ")))
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

  lines[entry] <- '  %\\VignetteIndexEntry{prebuilt vignette for pdf}'


  print(">>> Writing lines to vignettes/prebuilt.Rmd <<<")

  writeLines(lines[-(range[1]:(range[2]-1))], 'vignettes/prebuilt.Rmd')
  devtools::build_vignettes()

  print(">>> Compacting doc/prebuilt.pdf <<<")

  print(tools::compactPDF("doc/prebuilt.pdf", gs_quality = "ebook"))

  print(">>> Moving (hopefully) compacted pdf to vignettes directory <<<")

  file.rename('doc/prebuilt.pdf', 'vignettes/prebuilt.pdf')
  file.remove('vignettes/prebuilt.Rmd')

  print(">>> Done <<<")
})

