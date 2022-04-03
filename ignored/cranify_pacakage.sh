
echo 'tools::compactPDF("doc/socialranking_pdf.pdf", gs_quality = "ebook")' | r
rm vignettes/prebuilt.pdf
cp doc/socialranking_pdf.pdf vignettes/prebuilt.pdf
echo 'devtools::build(args = "--compact-vignettes=gs+pdf", path="doc/buid.tar.gz")' | r
R CMD check --as-cran 'doc/build.tar.gz'
