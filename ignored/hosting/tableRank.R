library(shiny)
library(shinyjs)
library(shinyMatrix)
library(socialranking)

ui <- fluidPage(
  useShinyjs(),
  titlePanel('Social Rankings'),
  sidebarLayout(
    sidebarPanel(
      h3('Valuation'),
      tags$style('.actionButtons { padding: 0 9px }
                 .actionButtons div { padding: 0 3px }
                 .actionButtons button {background-color: #A7C7E7; border-radius: 500px }
                 .selectionRow { padding: 0 15px; text-align: center }
                 .selectionRow div { padding: 0 }
                 .selectionRow .col-sm-6, #randT { text-align: center; height: 34px; border-top: 1px solid #ccc; border-bottom: 1px solid #ccc; background-color: #555; color: #eee }
                 '),
      fluidRow(
        column(3, actionButton('aM', label = '', icon = icon('minus'), style='border-radius: 500px 0 0 500px;width:100%;background-color:#FF6961;border-right:none')),
        column(6, div(textOutput('aT'), style='display:inline;line-height:34px')),
        column(3, actionButton('aP', label = '', icon = icon('plus'), style='border-radius: 0 500px 500px 0;width:100%;background-color:#C1E1C1;border-left:none')),
        class='selectionRow'
      ),
      br(),
      fluidRow(
        column(3, actionButton('vM', label = '', icon = icon('minus'), style='border-radius: 500px 0 0 500px;width:100%;background-color:#FF6961;border-right:none')),
        column(6, div(textOutput('vT'), style='display:inline;line-height:34px')),
        column(3, actionButton('vP', label = '', icon = icon('plus'), style='border-radius: 0 500px 500px 0;width:100%;background-color:#C1E1C1;border-left:none')),
        class='selectionRow'
      ),
      br(),
      fluidRow(
        column(3, actionButton('VM', label = '', icon = icon('minus'), style='border-radius: 500px 0 0 500px;width:100%;background-color:#FF6961;border-right:none')),
        column(6, div(textOutput('VT'), style='display:inline;line-height:34px')),
        column(3, actionButton('VP', label = '', icon = icon('plus'), style='border-radius: 0 500px 500px 0;width:100%;background-color:#C1E1C1;border-left:none')),
        class='selectionRow'
      ),
      br(),
      # tags$style('#rand-label { display: none }
      #            #rand { text-align: right; border-radius: 500px 0 0 500px; border-right: 0 }
      #            .randFrame div { padding: 0 }
      #            .randFrame { padding: 0 15px }'),
      fluidRow(
        column(6, actionButton('fillRand', 'Generate random valuations', width='100%')),
        column(6, actionButton('sort', withMathJax('Sort valuations \\((T \\rightarrow T_0)\\)'), width='100%')),
        class='actionButtons'
      ),
      br(),
      h3('Permutations'),
      fluidRow(
        column(6, actionButton('perReset', 'Reset', width='100%')),
        column(6, actionButton('perNext', 'Next permutation', width='100%')),
        class='actionButtons'
      ),
      h4('Fix values (row,column)'),
      fluidRow(
        column(4, actionButton('fixNothing', 'Unfix everything', width='100%')),
        column(4, actionButton('fixColumn', 'Fix first column', width='100%')),
        column(4, actionButton('fixEverything', 'Fix everything', width='100%')),
        class='actionButtons'
      ),
      uiOutput('fixed')
    ),
    mainPanel(
      tags$style('th { background-color: #ececec; }
                 #mat table { table-layout: auto; width: auto; }
                 #mat table th, #mat table tr { padding: 0 20px; text-align: center; }
                 td input, th input { max-width: 36px }
                 h3 { margin-top: 0 }
                 #valuationDesc { min-height: 2em }'),
      h3('Valuations'),
      matrixInput(
        'mat',
        value = matrix(c(1, 2, 3, 4, 2, 3), nrow = 3, dimnames = list(letters[1:3],c('C1','C2'))),
        cols = list(editableNames = TRUE),
        rows = list(editableNames = TRUE)
      ),
      uiOutput('valuationDesc'),
      br(),
      h3('Lexcel, Dual Lexcel, and Special Ranking'),
      verbatimTextOutput('ranking', TRUE),
      h3('Special Ranking'),
      verbatimTextOutput('spec'),
      br(),
      withMathJax('We define a social ranking function wherein \\(i \\succ_{F(T)} j \\quad \\Leftrightarrow \\quad \\begin{cases}
                    V_1(i, T) \\succ_V V_1(j, T) &\\text{ and } V_2(i, T) \\succeq_V V_2(j, T)\\\\[5pt]
                    V_1(i, T) \\succeq_V V_1(j, T) &\\text{ and } V_2(i, T) \\succ_V V_2(j, T)
                  \\end{cases}
                  \\)'),
      # h3('No Coincides'),
      # p('In case neither Lexcel nor Dual Lexcel coincide with the special ranking, those matrixes will appear below'),
      # verbatimTextOutput('coincides'),
      div(
        style="min-height: 100px; margin-bottom: 50px"
      ),
    )
  )
)

server <- function(input, output, session) {
  noCoincides <- reactiveVal('')

  output$ranking <- renderPrint({
    m <- input$mat
    m2 <- matrix(as.numeric(m), nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
    m2 <- structure(split(-m2, row(m2)), names = rownames(m2), class = 'LexcelScores')

    hasFaults <- FALSE
    ranking <- doRanking(m2, compare = function(a, b) {
      if(a[1] > b[1] && a[2] >= b[2]) return(1)
      if(a[1] >= b[1] && a[2] > b[2]) return(1)
      if(b[1] > a[1] && b[2] >= a[2]) return(-1)
      if(b[1] >= a[1] && b[2] > a[2]) return(-1)
      if(a[1] == b[1] && a[2] == b[2]) return(0)
      hasFaults <<- TRUE
      return(0)
    })

    r1 <- doRanking(m2)
    r2 <- doRanking(structure(lapply(m2, function(r) -rev(r)), class = 'LexcelScores'))

    print(r1)
    print(r2)

    if(hasFaults) {
      print('Special Ranking invalid')
    } else {
      print(ranking)
      if(ranking == r1) {
        print(paste('Coincides with Lexcel'))
      } else if(ranking == r2) {
        print(paste('Coincides with Dual Lexcel'))
      } else {
        print('! DOES NOT COINCIDE !')
      }
    }
  })

  output$spec <- renderPrint({
    m2 <- input$mat
    m2 <- matrix(as.numeric(m2), nrow = nrow(m2), ncol = ncol(m2), dimnames = dimnames(m2))
    doRanking(structure(
      split(-m2, row(m2)),
      names = rownames(m2)
    ), compare = function(a, b) {
      if(a[1] > b[1] && a[2] >= b[2]) return(1)
      if(a[1] >= b[1] && a[2] > b[2]) return(1)
      if(b[1] > a[1] && b[2] >= a[2]) return(-1)
      if(b[1] >= a[1] && b[2] > a[2]) return(-1)
      if(a[1] == b[1] && a[2] == b[2]) return(0)
      print(paste0('No relation between (', paste(-a, collapse = ','), ') and (', paste(-b, collapse = ','), ').'))
      return(0)
    })
  })

  # output$coincides <- renderPrint(noCoincides())

  V <- reactiveVal(4)

  output$aT <- renderPrint(cat(nrow(input$mat), 'Alternatives'))
  output$vT <- renderPrint(cat(ncol(input$mat), 'Voters'))
  output$VT <- renderPrint(cat('|V| =', V()))

  valuations <- reactive(unique(sort(as.numeric(input$mat))))
  output$valuationDesc <- renderUI(withMathJax(paste0('\\(V = \\{', paste0(valuations(), collapse = ', '), '\\} \\quad ', paste0(valuations(), collapse = ' \\succ_V '),'\\)')))

  observeEvent(input$VM, { if(V() > 1) V(V() - 1) })
  observeEvent(input$VP, { V(V()+1) })
  observeEvent(input$aP, {
    m2 <- input$mat
    m2 <- rbind(m2, 0)
    rownames(m2)[nrow(m2)] <- letters[nrow(m2)]
    updateMatrixInput(session, 'mat', m2)
  })
  observeEvent(input$vP, {
    m2 <- input$mat
    m2 <- cbind(m2, 0)
    colnames(m2)[ncol(m2)] <- paste0('C', ncol(m2))
    updateMatrixInput(session, 'mat', m2)
  })
  observeEvent(input$aM, { if(nrow(input$mat) < 2) return(); updateMatrixInput(session, 'mat', input$mat[-nrow(input$mat),,drop = FALSE]) })
  observeEvent(input$vM, { if(ncol(input$mat) < 2) return(); updateMatrixInput(session, 'mat', input$mat[,-ncol(input$mat),drop = FALSE]) })
  observe({
    toggleState('aM', condition = nrow(input$mat) > 1)
    toggleState('vM', condition = ncol(input$mat) > 1)
    toggleState('VM', condition = V() > 1)
  })
  observeEvent(input$fillRand, {
    m2 <- input$mat
    fixed <- matrix(FALSE, nrow = nrow(m2), ncol = ncol(m2))
    for (i in 1:nrow(m2)) {
      for (j in 1:ncol(m2)) {
        checked <- input[[paste("checkbox", i, j, sep = "_")]]
        if(is.null(checked)) { checked <- FALSE }
        fixed[i,j] <- checked
      }
    }

    m2[!fixed] <- sample(1:V(), size = sum(!fixed), replace = TRUE)
    updateMatrixInput(session, 'mat', m2)
  })
  observeEvent(input$sort, {
    m2 <- input$mat
    m2 <- matrix(as.numeric(m2), nrow = nrow(m2), ncol = ncol(m2), dimnames = dimnames(m2))
    for(i in 1:nrow(m2)) {
      m2[i,] <- sort(m2[i,])
    }
    updateMatrixInput(session, 'mat', m2)
  })

  observeEvent(input$perReset, {
    m2 <- input$mat
    for (j in 1:ncol(m2)) {
      for (i in 1:nrow(m2)) {
        fixed <- input[[paste("checkbox", i, j, sep = "_")]]
        if(!is.null(fixed) && !fixed) {
          m2[i,j] <- 1
        }
      }
    }
    updateMatrixInput(session, 'mat', m2)
  })

  observeEvent(input$perNext, {
    m2 <- input$mat
    fixed <- matrix(FALSE, nrow = nrow(m2), ncol = ncol(m2))
    for (i in 1:nrow(m2)) {
      for (j in 1:ncol(m2)) {
        checked <- input[[paste("checkbox", i, j, sep = "_")]]
        if(is.null(checked)) { checked <- FALSE }
        fixed[i,j] <- checked
      }
    }
    nums <- as.numeric(m2[!fixed])
    for (i in rev(seq_along(nums))) {
      nums[i] <- nums[i] + 1
      if (nums[i] > V()) {
        nums[i] <- 1
      } else {
        break
      }
    }
    m2[!fixed] <- nums
    updateMatrixInput(session, 'mat', m2)
  })

  output$fixed <- renderUI({
    # Create a list to hold the checkboxInput elements
    checkboxes <- list()

    for (col in 1:ncol(input$mat)) {
      for (row in 1:nrow(input$mat)) {
        # Generate a unique ID for each checkbox
        id <- paste("checkbox", row, col, sep = "_")

        # Add a checkboxInput element to the list
        checked <- input[[paste("checkbox", row, col, sep = "_")]]
        if(is.null(checked)) {
          checked <- col == 1
        }
        checkboxes[[id]] <- checkboxInput(id, label = paste0('(',row,',',col,')'), value = checked)
      }
      # Add a br() element to the list at the end of each row
      # checkboxes[[paste("br", row, sep = "_")]] <- br()
    }

    # Return the list of checkboxInput elements
    do.call(tagList, checkboxes)
  })

  updateCheckboxes <- function(session, m, checkIf) {
    for (i in 1:nrow(m)) {
      for (j in 1:ncol(m)) {
        updateCheckboxInput(session, paste('checkbox', i, j, sep = '_'), value = checkIf(i, j))
      }
    }
  }
  observeEvent(input$fixNothing, updateCheckboxes(session, input$mat, function(i, j) FALSE))
  observeEvent(input$fixEverything, updateCheckboxes(session, input$mat, function(i, j) TRUE))
  observeEvent(input$fixColumn, updateCheckboxes(session, input$mat, function(i, j) j == 1))
}

shinyApp(ui, server)
