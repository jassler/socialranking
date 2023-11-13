## Example shiny app with rank list

library(shiny)
library(sortable)
library(socialranking)

labels <- lapply(createPowerset(letters[1:4]), paste, collapse = '')
labels[[length(labels)]] <- '∅'

rank_list <- rank_list(
  labels = labels,
  input_id = "rank_list",
  options = sortable_options(multiDrag = TRUE)
)



ui <- fluidPage(
  tags$head(
    tags$style(HTML('#rank-list-rank_list {
    padding-right: 0;
    margin-right: 0;
    border-right: none;
    text-align: right;
  }
  #rank-list-other {
    padding-left: 0;
    margin-left: 0;
    border-left: none;
  }
  #other > div {
    cursor: pointer;
  }
  #other > div:last-child {
    background-color: #FF6961;
    border-color: #8d0700;
    color: white;
    font-weight: bold;
    transform: translatex(10px) translatey(10px);
  }
  .tab-content {
    padding-top: 2em;
  }
  .MathJax_Processed, .hidden {
    opacity: 0;
  }

  #apCP {
    min-height: 45px;
  }

  .col-sm-6 {
    min-height: 110px;
  }
')),
    withMathJax(),
    tags$script(HTML('
$(document).on("click", "#other", (evt) => {
  let el = evt.target;
  let chs = el.parentElement.children;
  let symbol = (el.innerText === ">") ? "~" : ">";

  if(el === chs[chs.length - 1]) {
    for(let i = 0; i < chs.length - 1; i++) {
      chs[i].innerText = el.innerText;
    }
  }

  el.innerText = symbol;

  let s = "";
  for(let i = 0; i < chs.length - 1; i++) {
    s += chs[i].textContent;
  }
  Shiny.setInputValue("toggleEvent", s);
})

// MathJax.Hub.Queue(function() {
//   document.querySelectorAll(".MathJax").forEach(function(el) {
//     el.style.visibility = "";
//   });
// });
'
    )),
  ),
  title = 'Testing ordering',
  titlePanel('Testing ordering'),
  fluidRow(
    column(
      2,
      rank_list,
      style = 'padding-right: 0'
    ),
    column(
      1,
      rank_list(
        labels = c(rep('>', length(labels)-1), '~'),
        input_id = 'other',
        options = sortable_options(disabled = TRUE)
      ),
      style = 'padding-left: 0'
    ),
    column(
      9,
      h4('Results'),
      uiOutput('ranking', style = 'text-align: center'),
      uiOutput('desirable'),
      tabsetPanel(
        tabPanel(
          'Kramer Simpson',
          uiOutput('ksRanking', style = 'text-align: center'),
          br(),
          '$$KS(i) = \\max_{j \\in N} \\big\\{|\\{S \\in U_{ij} : S \\cup \\{j\\} \\succ S \\cup \\{i\\}\\}|\\big\\}$$',
          fluidRow(
            column(6, uiOutput('aKS')),
            column(6, uiOutput('bKS')),
          )
        ),
        tabPanel(
          'Copeland',
          uiOutput('copRanking', style = 'text-align: center'),
          br(),
          '$$Cop(i) = |\\{j \\in U_i: i P_{CP} j\\}| - |\\{j \\in U_i: j P_{CP} i\\}|$$',
          fluidRow(
            column(6, uiOutput('aCop')),
            column(6, uiOutput('bCop'))
          )
        ),
        tabPanel(
          'Ordinal Banzhaf',
          uiOutput('banzRanking', style = 'text-align: center'),
          br(),
          '$$Cop(i) = |\\{j \\in U_i: i P_{CP} j\\}| - |\\{j \\in U_i: j P_{CP} i\\}|$$',
          fluidRow(
            column(6, uiOutput('aBanz')),
            column(6, uiOutput('bBanz'))
          )
        )
      ),
      uiOutput('abCP'),
      hr(),
      fluidRow(
        column(6, uiOutput('aCP')),
        column(6, uiOutput('bCP'))
      )
    )
  )
)

server <- function(input, output, session) {
  comps <- reactiveVal(rep('>', length(labels)-1))
  pr <- reactive({
    rl <- input$rank_list
    if(length(rl) != 16) {
      as.PowerRelation(paste(labels, collapse = '>'))
    }
    if(all(rl == '')) {
      rl <- labels
    }
    rl[1:15] <- paste(rl[1:15], comps())
    suppressWarnings(as.PowerRelation(paste(rl, collapse = '')))
  })

  # output$results <- renderPrint({
  #   pr <- pr()
  #   print(pr)
  #   if(!('a' %in% pr$elements && 'b' %in% pr$elements)) return()
  #   print(if(dominates(pr, 'a', 'b', TRUE)) 'a strictly dominates b' else 'a does not strictly dominate b anymore')
  #   print(copelandRanking(pr))
  #   print(copelandScores(pr, elements = c('a','b')))
  # })

  # output$aCP <- renderPrint({
  #   pr <- pr()
  #   els <- pr$elements
  #   for(op in els) {
  #     if(op == 'a') next
  #     print(paste('a vs', op))
  #     print(cpMajorityComparison(pr, 'a', op))
  #   }
  # })
  #
  # output$bCP <- renderPrint({
  #   pr <- pr()
  #   els <- pr$elements
  #   for(op in els) {
  #     if(op == 'b') next
  #     print(paste('b vs', op))
  #     print(cpMajorityComparison(pr, 'b', op))
  #   }
  # })

  output$ranking <- renderUI({
    pr() |>
        capture.output() |>
        gsub(pattern = '\\{\\}', replacement = '∅')
  })

  output$desirable <- renderUI({
    pr <- pr()
    withMathJax(
      if(dominates(pr, 'a', 'b', strictly = TRUE)) {
        '$$a P_D b$$'
      } else if(dominates(pr, 'a', 'b', strictly = FALSE)) {
        '$$a I_D b$$'
      } else {
        '$$a \\require{cancel}\\cancel{R_D} b$$'
      }
    )
  })

  output$ksRanking <- renderUI({ capture.output(kramerSimpsonRanking(pr())) })
  output$copRanking <- renderUI({ capture.output(copelandRanking(pr())) })

  output$aKS <- renderUI({
    pr <- pr()
    withMathJax(
      tags$script(HTML('{
let el = document.querySelector("#aKS");
el.classList.add("hidden");
setTimeout(() => el.classList.remove("hidden"), Math.random() * 500 + 500);
}')),
      "$$\\begin{aligned}KS(\\text{a}) &= \\max_{j\\in N}\\big\\{",
      paste(
        sapply(pr$elements, function(opp)
          paste('\\underset{\\vphantom{b}', opp, '}{', cpMajorityComparisonScore(pr, opp, 'a', strictly = TRUE)[1], '}')
        ),
        collapse = ', '
      ),
      "\\big\\}\\\\",
      "\\end{aligned}$$"
    )
  })
  output$bKS <- renderUI({
    pr <- pr()
    withMathJax(
      tags$script(HTML('{
let el = document.querySelector("#bKS");
el.classList.add("hidden");
setTimeout(() => el.classList.remove("hidden"), Math.random() * 500 + 500);
}')),
      "$$\\begin{aligned}KS(\\text{b}) &= \\max_{j\\in N}\\big\\{",
      paste(
        sapply(pr$elements, function(opp)
          cpMajorityComparisonScore(pr, opp, 'b', strictly = TRUE)[1]
        ),
        collapse = ', '
      ),
      "\\big\\}\\\\",
      "KS(\\text{c}) &= \\max_{j\\in N}\\big\\{",
      paste(
        sapply(pr$elements, function(opp)
          cpMajorityComparisonScore(pr, opp, 'c', strictly = TRUE)[1]
        ),
        collapse = ', '
      ),
      "\\big\\}\\\\",
      "KS(\\text{d}) &= \\max_{j\\in N}\\big\\{",
      paste(
        sapply(pr$elements, function(opp)
          paste('\\underset{\\vphantom{b}', opp, '}{', cpMajorityComparisonScore(pr, opp, 'd', strictly = TRUE)[1], '}')
        ),
        collapse = ', '
      ),
      "\\big\\}\\end{aligned}$$")
  })
  output$aCop <- renderUI({
    wins <- c()
    loss <- c()
    pr <- pr()
    for(opp in pr$elements) {
      if(opp == 'a') next
      s <- sum(cpMajorityComparisonScore(pr, 'a', opp))
      if(s == 0) next
      if(s > 0) wins <- c(wins, opp)
      else loss <- c(loss, opp)
    }
    paste0('Cop(a) = |{', paste(wins, collapse = ', '), '}| - |{', paste(loss, collapse = ', '), '}|')
  })
  output$bCop <- renderUI({
    wins <- c()
    loss <- c()
    pr <- pr()
    for(opp in pr$elements) {
      if(opp == 'b') next
      s <- sum(cpMajorityComparisonScore(pr, 'b', opp))
      if(s == 0) next
      if(s > 0) wins <- c(wins, opp)
      else loss <- c(loss, opp)
    }
    paste0('Cop(b) = |{', paste(wins, collapse = ', '), '}| - |{', paste(loss, collapse = ', '), '}|')
  })
  output$abCP <- renderUI({
    outp <- capture.output(cpMajorityComparison(pr(), 'a', 'b'))
    withMathJax(
      tags$script(HTML('{
let el = document.querySelector("#abCP");
el.classList.add("hidden");
setTimeout(() => el.classList.remove("hidden"), Math.random() * 500 + 500);
}')),
      '\\(',
      outp[2] |>
        gsub(pattern = '\\{\\}\\}', replacement = '\\\\varnothing\\}') |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_ab', replacement = '_\\{ab\\}'),
      '\\)', br(), '\\(',
      outp[3] |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_ba', replacement = '_\\{ba\\}'),
      '\\)'
    )
  })
  output$aCP <- renderUI({
    outpC <- capture.output(cpMajorityComparison(pr(), 'a', 'c'))
    outpD <- capture.output(cpMajorityComparison(pr(), 'a', 'd'))
    withMathJax(
      tags$script(HTML('{
let el = document.querySelector("#aCP");
el.classList.add("hidden");
setTimeout(() => el.classList.remove("hidden"), Math.random() * 500 + 500);
}')),
      h5('a against c'),
      '\\(',
      outpC[2] |>
        gsub(pattern = '\\{\\}\\}', replacement = '\\\\varnothing\\}') |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_ac', replacement = '_\\{ac\\}'),
      '\\)', br(), '\\(',
      outpC[3] |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_ca', replacement = '_\\{ca\\}'),
      '\\)',

      h5('a against d'),
      '\\(',
      outpD[2] |>
        gsub(pattern = '\\{\\}\\}', replacement = '\\\\varnothing\\}') |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_ad', replacement = '_\\{ad\\}'),
      '\\)', br(), '\\(',
      outpD[3] |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_da', replacement = '_\\{da\\}'),
      '\\)'
    )
  })
  output$bCP <- renderUI({
    outpC <- capture.output(cpMajorityComparison(pr(), 'b', 'c'))
    outpD <- capture.output(cpMajorityComparison(pr(), 'b', 'd'))
    withMathJax(
      tags$script(HTML('{
let el = document.querySelector("#bCP");
el.classList.add("hidden");
setTimeout(() => el.classList.remove("hidden"), Math.random() * 500 + 500);
}')),
      h5('b against c'),
      '\\(',
      outpC[2] |>
        gsub(pattern = '\\{\\}\\}', replacement = '\\\\varnothing\\}') |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_bc', replacement = '_\\{bc\\}'),
      '\\)', br(), '\\(',
      outpC[3] |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_cb', replacement = '_\\{cb\\}'),
      '\\)',

      h5('b against d'),
      '\\(',
      outpD[2] |>
        gsub(pattern = '\\{\\}\\}', replacement = '\\\\varnothing\\}') |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_bd', replacement = '_\\{bd\\}'),
      '\\)', br(), '\\(',
      outpD[3] |>
        gsub(pattern = '\\{', replacement = '\\\\{') |>
        gsub(pattern = '\\}', replacement = '\\\\}') |>
        gsub(pattern = '_db', replacement = '_\\{db\\}'),
      '\\)'
    )
  })

  observeEvent(input$toggleEvent, {
    comps(strsplit(input$toggleEvent, '')[[1]])
  })
}

shinyApp(ui, server)
