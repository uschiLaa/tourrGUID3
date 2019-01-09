#' Shiny app for tourr D3 gui
#'
#' @param inputDataFile Path to starting input data file (csv format)
#' @author Ursula Laa
#' @author Michael Kipp
#' @export
#'

launchApp <- function(inputDataFile){
  ui <- fluidPage(
    titlePanel("Welcome to the TourR Shiny app powered by D3.js"),
    fluidRow(
      column(3,
             radioButtons(
               "type",
               label = "Select tour type",
               choices = c("Guided", "Little", "Grand"),
               selected = "Grand"
             ),
             conditionalPanel(
               "input.type == 'Guided'",
               selectInput(
                 "guidedIndex",
                 "Index function",
                 c("Holes", "Centre Mass", "LDA", "PDA", "Scagnostics")
                 ,
                 selected = "LDA"
               )
             ),
             conditionalPanel(
               "input.guidedIndex == 'Scagnostics'",
               selectInput("scagType", "Scagnostics Metric",
                           choices = list(
                             "Outlying",
                             "Skewed",
                             "Clumpy",
                             "Sparse",
                             "Striated",
                             "Convex",
                             "Skinny",
                             "Stringy",
                             "Monotonic"))),

             fileInput("file1", "Choose CSV File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
             ),

             sliderInput(
               "speed",
               label =  "Tour speed",
               min = 0,
               max = 5,
               value = 1,
               step = 0.1
             ) ,
             actionButton("restart_random", "Restart tour with random basis"),
             selectInput("point_label",choices = vector('character'), label = "Select labeling variable"),
             selectInput("class", choices = vector('character'), label = "Select class variable to colour the points"),
             conditionalPanel( condition = "output.numC",
                               sliderInput("cMax",label = "Threshold value", min=0, max =1, value= 0.5, step = 0.1))
             # sliderInput("cMax",label = "Threshold value", min="output.minC", max ="output.maxC", value= "output.medC", step = "output.stepC"))
      ),
      column(3,
             checkboxGroupInput(
               "variables",
               label = "Choose variables for the 2D tour",
               choices = vector('character'),
               selected = vector('character')
             )
      ),
      column(6,
             #tags$div(tags$p(" "), ggvisOutput("ggvis")),
             #tags$div(tags$p(textOutput("type"))),
             tags$script(src = "https://d3js.org/d3.v4.min.js"),
             tags$script(src = "https://d3js.org/d3-contour.v1.min.js"),
             tags$script(src = "https://d3js.org/d3-scale-chromatic.v1.min.js"),
             tags$div(id = "d3_output"),
             tags$div(id = "d3_output_2"),
             tags$div(id = "info"),
             tags$div(id = "info_2"),
             #tags$script(src = "d3anim.js")
             includeScript(system.file("inst/js/d3anim.js", package = "tourrGUID3"))
             #includeScript(system.file("js/d3anim.js", package = "tourrGUID3"))
             )
    )
  )

  server <- function(input, output, session) {
    fps <- 33
    aps <- 5

    rv <- shiny::reactiveValues()

    rv$d <- read.csv(inputDataFile, stringsAsFactors = FALSE)

    observeEvent(input$restart_random,
                 {

                   p <- length(input$variables)
                   if(p==0){return()}
                   b <- matrix(runif(2*p), p, 2)

                   rv$tour <- new_tour(as.matrix(rv$d[input$variables]),
                                       choose_tour(input$type, input$guidedIndex, c(rv$class[[1]]), input$scagType),
                                       b)
                 }, ignoreInit = TRUE)

    observeEvent(input$speed, rv$aps <- input$speed)

    observeEvent(input$file1, {
      inFile <- input$file1
      rv$d <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    })

    observeEvent(rv$d, {

      rv$nums <- sapply(rv$d, is.numeric)
      rv$groups <- sapply(rv$d, is.character)

      updateCheckboxGroupInput(
        session, "variables",
        choices = names(rv$d[rv$nums]),
        selected = names(rv$d[rv$nums])[1:3]
      )

      updateSelectInput(session, "class", choices = names(rv$d),
                        selected = names(rv$d[rv$groups])[1])
      updateSelectInput(session, "point_label", choices = names(rv$d), selected = names(rv$d)[1])

    })


    observeEvent(c(input$type, input$variables, input$guidedIndex, input$class, input$scagType, input$point_label, input$cMax),
                 {

                   session$sendCustomMessage("debug", paste("Changed tour type to ", input$type))
                   if (length(input$variables) == 0) {
                     rv$mat <- rescale(as.matrix(rv$d[names(rv$d[rv$nums])[1:3]]))
                     rv$vars <- names(rv$d[rv$nums])[1:3]
                     rv$class <- unname(rv$d[names(rv$d)[1]])
                     if (is.numeric(rv$class[,1])){
                       output$numC <- reactive(TRUE)
                       minC <- min(rv$d[names(rv$d)[1]])
                       maxC <- max(rv$d[names(rv$d)[1]])
                       if((input$cMax >= minC) & (input$cMax <= maxC) ){medC <- input$cMax}
                       else{medC <- median(rv$d[names(rv$d)[1]][,1])}
                       stepC <- (max(rv$d[names(rv$d)[1]]) - min(rv$d[names(rv$d)[1]])) / 100
                       cl <- rv$class[,1]
                       rv$class <- unname(ifelse(rv$d[names(rv$d)[1]] > input$cMax, "Larger", "Smaller"))
                       updateSliderInput(session, "cMax", min=minC, max=maxC, value=medC, step=stepC)
                     }
                     else{
                       rv$class <- unname(rv$d[input$class])
                       output$numC <- reactive(FALSE)
                       cl <- rv$class[[1]]
                     }
                     rv$pLabel <- unname(rv$d[names(rv$d)[1]])
                   } else {

                     rv$mat <- rescale(as.matrix(rv$d[input$variables]))
                     rv$vars <- input$variables
                     if (rv$nums[input$class]){
                       output$numC <- reactive(TRUE)
                       minC <- min(rv$d[input$class])
                       maxC <- max(rv$d[input$class])
                       if((input$cMax >= minC) & (input$cMax <= maxC) ){medC <- input$cMax}
                       else{medC <- median(rv$d[input$class][,1])}
                       stepC <- (max(rv$d[input$class]) - min(rv$d[input$class])) / 100
                       cl <- rv$class[,1]
                       rv$class <- unname(ifelse(rv$d[input$class] > input$cMax, "Larger", "Smaller"))
                       updateSliderInput(session, "cMax", min=minC, max=maxC, value=medC, step=stepC)


                     }
                     else{
                       rv$class <- unname(rv$d[input$class])
                       output$numC <- reactive(FALSE)
                       cl <- rv$class[[1]]
                     }
                     outputOptions(output, "numC", suspendWhenHidden = FALSE)
                     rv$pLabel <- unname(rv$d[input$point_label])
                   }


                   #cl <- c(rv$class[[1]])

                   session$sendCustomMessage("newcolours", unique(cl))

                   rv$tour <-
                     new_tour(rv$mat,
                              choose_tour(input$type, input$guidedIndex, cl, input$scagType))
                   rv$ini <- FALSE
                 })


    holes_ <- function() {
      function(mat) {
        n <- nrow(mat)
        d <- ncol(mat)

        num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
        den <- 1 - exp(-d / 2)

        val <- num / den
        return(val)
      }
    }

    scags <- function(cl,scagMetricIndex) {

      l <- length(unique(cl))

      if (l != 2)
      {
        stop("Scagnostics indices require two groups.")
      }


      function(mat) {
        mat_ <- cbind.data.frame(mat, class = cl)


        scagResults = c(scagnostics(subset(mat_, class == unique(cl)[1])[1:2])[scagMetricIndex],
                        scagnostics(subset(mat_, class == unique(cl)[2])[1:2])[scagMetricIndex]

        )


        return(abs(scagResults[1] - scagResults[2]))

      }
    }

    cmass_ <- function() {
      function(mat) {
        n <- nrow(mat)
        d <- ncol(mat)

        num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
        den <- 1 - exp(-d / 2)

        val <- num / den
        return(1 - val)
      }
    }

    observe({


      aps <- rv$aps
      tour <- rv$tour

      if(!rv$ini){
        step <- tour(0)
        rv$ini <- TRUE
      }
      else{
        step <- rv$tour(aps / fps)
      }

      if (!is.null(step)) {
        invalidateLater(1000 / fps)

        j <- center(rv$mat %*% step$proj)
        j <- cbind(j, class = rv$class)
        colnames(j) <- NULL

        session$sendCustomMessage(type = "data", message = list(d = toJSON(data_frame(pL=rv$pLabel[,1],x=j[,2],y=j[,1],c=j[,3])),
                                                                a = toJSON(data_frame(n=rv$vars,y=step$proj[,1],x=step$proj[,2]))))
      }

      else{

        if (length(rv$mat[1, ]) < 3) {
          session$sendCustomMessage(type = "debug", message = "Error: Need >2 variables.")
        } else {
          session$sendCustomMessage(type = "debug", message = "Guided tour finished: no better bases found.")
        }
      }
    })



    choose_tour <- function(type,
                            subtype = "",
                            group_variable = "",
                            scagTypeIndex
    )

    {


      if (type == "Grand")
      {
        tourType <- grand_tour()
      }
      else if (input$type == "Little") {
        tourType <- little_tour()

      } else

      {
        if (subtype == "Holes") {
          #browser()
          tourType <- guided_tour(holes_())
        } else if (subtype == "Centre Mass") {
          #browser()
          tourType <- guided_tour(cmass_())
        }
        else if (subtype == "LDA") {
          tourType <- guided_tour(lda_pp(group_variable))
        } else if (subtype == "PDA") {
          tourType <- guided_tour(pda_pp(group_variable))
        } else {
          tourType <- guided_tour(scags(group_variable, scagTypeIndex))
        }

      }

      return(tourType)
    }

  }
  shinyApp(ui, server)
}
