if(!dir.exists("doc")) {
  dir.create("doc")
}

knitr::knit2pdf("vignettes/socialranking.Rmd", output = "doc")
devtools::clean_vignettes()
devtools::build_vignettes()
tools::compactPDF("doc/socialranking_pdf.pdf", gs_quality = "ebook")
file.copy("doc/socialranking_pdf.pdf", "vignettes/prebuilt.pdf")
devtools::build(args = "--compact-vignettes=gs+pdf")
system("R CMD check --as-cran ~/Documents/programming/R/socialranking_0.1.0.tar.gz")
