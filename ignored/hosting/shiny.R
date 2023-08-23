library(shiny)
library(socialranking)

ui <- fluidPage(
  titlePanel('Social Rankings'),
  sidebarLayout(
    sidebarPanel(
      actionButton('loadSingleton', 'Singleton', width='100%', style='margin-bottom: 5px'),
      actionButton('loadLex', 'Lexcel', width='100%', style='margin-bottom: 5px'),
      actionButton('loadDualLex', 'Dual Lexcel', width='100%', style='margin-bottom: 5px'),
      actionButton('loadBanzhaf', 'Ordinal Banzhaf', width='100%', style='margin-bottom: 5px'),
      actionButton('loadCopeland', 'Copeland-like method', width='100%', style='margin-bottom: 5px')
    ),
    mainPanel(
      # tags$head(tags$script(src='prism.js')),
      tags$head(tags$style(src='prism.css')),
      tags$head(HTML('<style>textarea { font-family: monospace; } input[type="text"] { padding-right: 0 !important; }</style>')),
      fluidRow(
        column(8, textInput('pr', 'Power Relation', value = '123 > 12 > 13 > 23 > (1 ~ 2 ~ 3) > {}', width='100%')),
        column(2, actionButton('gen123', '123', width='100%', style='margin-top: 25px')),
        column(2, actionButton('gen1234', '1234', width='100%', style='margin-top: 25px'))
      ),
      shiny::checkboxInput('makeComplete', 'Add missing coalitions'),
      shiny::checkboxInput('makeMonotonic', 'Make power relation monotonic'),
      shiny::textAreaInput('scoreIn', 'Calculate score for `element` in power relation object `pr`',
'unlist(lapply(
    pr$eqs,
    function(coalition) sum(element == unlist(coalition))
))', width = '100%', rows=8),
      shiny::textAreaInput('codeIn', 'Compare a vs b (0 → indifference, >0 → aPb, <0 → bPa)', value = 'i <- which(a != b)
if(length(i) == 0) 0
else a[i[1]] - b[i[1]]', width='100%', rows=8),
      shiny::uiOutput('codeFix'),
      hr(),
      shiny::fluidRow(
        column(2, shiny::actionButton('run', 'Run', width='100%', style='padding: 9.5px 0')),
        column(10, verbatimTextOutput("codeResults")),
      ),
      div(
        style="min-height: 100px; margin-bottom: 50px"
      )
    )
  )
)

server <- function(input, output, session) {
  shinyEnv <- environment()

  pr_reactive <- reactive({
    pr <- as.PowerRelation(input$pr)
    if(input$makeComplete && length(unlist(pr$eqs, recursive = FALSE)) < 2^length(pr$elements)) pr <- appendMissingCoalitions(pr)
    if(input$makeMonotonic) pr <- makePowerRelationMonotonic(pr, FALSE)
    pr
  })

  code <- reactive({
    pr_result <- pr_reactive()
    paste0(
'pr <- as.PowerRelation("', capture.output(pr_result), '")

scoreFunction <- function(pr) lapply(pr$elements, function(element) {
    ', gsub('\n', '\n    ', input$scoreIn), '
})

compareFunction <- function(a, b) {
    ', gsub('\n', '\n    ', input$codeIn), '
}

myRanking <- function(pr) doRanking(scoreFunction(pr), compareFunction)

writeLines(capture.output(myRanking(pr)))'
  )})

  createRandomRanking <- function(elements) {
    l <- createPowerset(elements)
    capture.output(as.PowerRelation(
      sample(l),
      comparators = sample(c('~','>'), length(l)-1, replace=TRUE)
    ))
  }

  observeEvent(input$gen123, {
    s <- '123 > 12 > 13 > 23 > 1 > 2 > 3 > {}'
    if(input$pr == s) {
      s <- createRandomRanking(1:3)
    }
    updateTextInput(session, 'pr', value = s)
  }

  )
  observeEvent(input$gen1234, {
    s <- capture.output(as.PowerRelation(createPowerset(1:4)))
    if(input$pr == s) {
      s <- createRandomRanking(1:4)
    }
    updateTextInput(session, 'pr', value = s)
  })

  output$codeFix <- shiny::renderUI({
    shiny::tagList(
      shiny::HTML(paste0("<pre><code class='language-R'>", code(), "</code></pre>")),
      # shiny::tags$script(src = 'Prism.highlightAll()')
      # tags$script(src = "prism.js")
    )
  })

  codeOutput <- reactiveVal('The code output will appear here')
  observeEvent(input$run, codeOutput({
    capture.output(tryCatch(eval(parse(text=code()), envir=shinyEnv), error = function(e) e))
  }))
  output$codeResults <- renderPrint(codeOutput())

  programs <- list(
    loadSingleton = c('pr$coalitionLookup(c(element))', '# singletons in higher-indexed equivalence classes rank worse\nb - a'),
    loadLex = c('unlist(lapply(
    pr$eqs,
    function(coalition) sum(element == unlist(coalition))
))', 'i <- which(a != b)
if(length(i) == 0) 0
else a[i[1]] - b[i[1]]'),
    loadDualLex = c('-rev(unlist(lapply(
    pr$eqs,
    function(coalition) sum(element == unlist(coalition))
)))', 'i <- which(a != b)
if(length(i) == 0) 0
else a[i[1]] - b[i[1]]'),
    loadBanzhaf = c('score <- 0
for(index in pr$elementLookup(element)) {
  coalition <- pr$eqs[[index[1]]][[index[2]]]
  indexWithout <- pr$coalitionLookup(setdiff(coalition, element))
  if(is.null(indexWithout)) next
  if(index[1] < indexWithout) score <- score + 1
  if(index[1] > indexWithout) score <- score - 1
}
score', 'a - b'),
    loadCopeland = c('scores <- setdiff(pr$elements, element) |> sapply(function(p2) {
  sum(cpMajorityComparisonScore(pr, element, p2))
})
sum(scores >= 0) - sum(scores <= 0)', 'a - b')
  )
  updateCodes <- function(name) {
    updateTextAreaInput(session, 'scoreIn', value = programs[[name]][1])
    updateTextAreaInput(session, 'codeIn', value = programs[[name]][2])
  }
  observeEvent(input$loadSingleton, updateCodes('loadSingleton'))
  observeEvent(input$loadLex, updateCodes('loadLex'))
  observeEvent(input$loadDualLex, updateCodes('loadDualLex'))
  observeEvent(input$loadBanzhaf, updateCodes('loadBanzhaf'))
  observeEvent(input$loadCopeland, updateCodes('loadCopeland'))




  # create codeInput variable to capture what the user entered; store results to codeResults
  # codeInput <- reactive({ input$testcode })
  # output$codeResults <- renderPrint({
  #   eval(parse(text=codeInput()), envir=shinyEnv)
  # })
}

shinyApp(ui, server)
