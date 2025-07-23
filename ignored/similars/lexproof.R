library(shiny)

coalToKey <- function(S) paste0('{',paste(S,collapse=','),'}')

applyLexAxioms <- function(pr, i, j) {
  steps <- list(pr)
  scores <- lexcelScores(pr, c(i,j))
  if(scores[j] > scores[i]) {
    tmp <- i
    i <- j
    j <- tmp
    scores <- scores[c(2,1)]
  }
  print(pr)
  writeLines(paste0(i, if(scores[1] > scores[2]) 'P' else 'I', j))

  ###########
  #   IWS   #
  ###########
  k <- which(scores[[i]] != scores[[j]])[1]
  if(is.na(k)) {
    k <- 0
  }
  if(k == length(scores[[i]]) - 1) {
    writeLines('\nStep 1 (IWS): Does not need to be applied.')
  } else {
    writeLines(paste0('\nStep 1 (IWS): Form a union over the equivalence classes from Σ_', k+1, ' onwards.'))
    eqs <- pr$eqs
    eqs[[k+1]] <- append(eqs[[k+1]], eqs[(k+2):length(eqs)] |> unlist(recursive = FALSE))
    pr <- PowerRelation(eqs[1:(k+1)])
    steps <- append(steps, list(pr))
  }

  print(pr)

  eqs <- pr$eqs

  ###########
  #   CA    #
  ###########
  π <- createPowerset(setdiff(pr$elements, c(i, j)))
  names(π) <- sapply(π, coalToKey)

  iIndexes <- pr$elementLookup(i)
  jIndexes <- pr$elementLookup(j)
  both <- intersect(iIndexes, jIndexes)
  iIndexes <- setdiff(iIndexes, both)
  jIndexes <- setdiff(jIndexes, both)
  for(k in seq_along(jIndexes)) {
    jIndex <- jIndexes[[k]]
    iIndex <- iIndexes[[k]]
    if(jIndex[1] == length(eqs)) {
      break
    }

    iCoal <- eqs[[iIndex[1]]][[iIndex[2]]]
    jCoal <- eqs[[jIndex[1]]][[jIndex[2]]]
    Si <- setdiff(iCoal, i)
    Sj <- setdiff(jCoal, j)

    tmp <- π[[coalToKey(Si)]]
    π[[coalToKey(Si)]] <- Sj
    π[[coalToKey(Sj)]] <- tmp
  }
  writeLines('\nStep 2 (CA): Apply the following permutation π on S u {i}:')
  for(k in seq_along(π)) { writeLines(paste0('π(', names(π)[k], ') = ', coalToKey(π[[k]]))) }
  for(iIndex in iIndexes) {
    S <- eqs[[iIndex[1]]][[iIndex[2]]]
    S <- setdiff(S, i)
    eqs[[iIndex[1]]][[iIndex[2]]] <- c(i, π[[coalToKey(S)]]) |> as.numeric() |> sort()
  }
  pr <- PowerRelation(eqs)
  print(pr)

  writeLines('\nStep 3 (Des): Does desirability hold now?')
  print(dominates(pr, i, j, FALSE))
  writeLines('Strict preference?')
  print(dominates(pr, i, j, TRUE))
  append(steps, pr)
}


ui <- fluidPage(
  titlePanel('Applying Axioms Algorithmically'),
  fluidRow(
    column(1, actionButton('r3', 'N=3', width = '100%')),
    column(1, actionButton('r4', 'N=4', width = '100%')),
    column(1, actionButton('r5', 'N=5', width = '100%')),
    column(8, textInput('pr', 'Power Relation', '123 ~ 1 ~ 23 > 13 > 2 > {}', width = '100%')),
    column(1, actionButton('enter', icon('circle-play'), width = '100%')),
    style='max-width: 980px;margin: auto'
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
