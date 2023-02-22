# To generate automatic checks in all files listed in ../R/*.R, the following has to hold:
# 1. It is a top level function formatted as follows:
#    [line beginning]nameOfFunction <- function(x, y, z) {
# 2. The line after the open curly bracket has to be exactly this (including 2 spaces at the beginning):
#    '  # --- checks (generated) --- #'
# 3. There has to be a line with the following string somewhere after the previous condition:
#    '  # --- end checks --- #'
local({
  funcRegex <- r'(^[a-zA-Z`%=:>]+\s*<-\s*function\((.*)\)\s*\{)'

  if(!dir.exists('R')) {
    stop(paste0('No R/ directory in ', getwd(), '. Make sure the working directory is set correctly.'))
  }

  files <- list.files('R', full.names = TRUE)

  checks <- list(
    "pr_e1" = c(
      "if(!is.list(pr_e1) || length(pr_e1) != 2) stop('To check for a relation, provide a PowerRelation object, add \"%:%\" and then test between 2 elements.')",
      "powerRelation <- pr_e1[[1]]",
      "e1 <- pr_e1[[2]]",
      "if(!is.PowerRelation(powerRelation)) stop('Left side must be an object of type PowerRelation.')",
      "if(!(e1 %in% powerRelation$elements)) stop('First element does not exist in the given PowerRelation object.')"
    ),
    "powerRelation" = "stopifnot(is.PowerRelation(powerRelation))",
    "e1" = c(
      "stopifnot(e1 %in% powerRelation$elements)",
      "stopifnot(class(e1) == class(powerRelation$elements))"
    ),
    "e2" = c(
      "stopifnot(e2 %in% powerRelation$elements)",
      "stopifnot(class(e2) == class(powerRelation$elements))"
    )
    #, "elements" = "if(is.null(elements)) elements <- powerRelation$elements"
  )

  for(f in files) {
    lines <- readLines(f)
    relevant <- which(grepl(funcRegex, lines))
    relevant <- Filter(function(x) lines[x+1] == '  # --- checks (generated) --- #', relevant)
    relevantEnd <- c()
    relevant <- Filter(function(x) {
      include <- FALSE
      for(i in (x+1):length(lines)) {
        if(!startsWith(lines[i], '  '))
          break

        if(lines[i] == '  # --- end checks --- #') {
          include <- TRUE
          relevantEnd <<- c(relevantEnd, i)
          break
        }
      }
      include
    }, relevant)

    if(length(relevant) == 0)
      next

    for(i in length(relevant):1) {
      line <- lines[relevant[i]]
      amount <- c()
      params <- stringr::str_match(lines[relevant[i]], funcRegex)[1,-1]
      params <- stringr::str_replace_all(params, '[^\\w,=]', '')
      params <- stringr::str_replace_all(params, '=\\w+', '')
      params <- strsplit(params, ',')[[1]]
      checkLines <- c()
      for(param in params) {
        if(!(param %in% names(checks)))
          next

        amount <- c(amount, param)
        checkLines <- c(checkLines, paste0('  ', checks[[param]]))
      }

      print(paste0('Checking ', length(amount), ' params in ', stringr::str_replace_all(line, '^(\\w+).*$', '\\1'),
                   ' (', paste(amount, collapse = ', '), ')'))

      lines <- c(lines[1:(relevant[i]+1)], checkLines, lines[relevantEnd[i]:length(lines)])
    }

    fileConn <- file(f)
    writeLines(lines, fileConn)
    close(fileConn)
  }
})
