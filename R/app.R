#' Launch Shiny app for tourr D3 gui.
#'
#' Launching the interactive tourr GUI shiny app with selected input.
#'
#' The function is building and launching the shiny app,
#' reading the input data from the specified csv file and
#' displaying the 2-d projections obtained via the tourr package.
#'
#'
#' @param inputDataFile Path to initial input data file (csv format)
#' @author Ursula Laa
#' @author Michael Kipp
#' @author Dianne Cook
#'
#' @examples
#' \dontrun{
#' library(tourrGUID3)
#' launchApp(system.file("extdata", "geozoo.csv", package = "tourrGUID3"))
#' }
#'
#' @export
#'

launchApp <- function(inputDataFile){
  ui <- shiny::fluidPage(
    shiny::titlePanel("Welcome to the TourR Shiny app powered by D3.js"),
    shiny::fluidRow(
      shiny::column(3,
             shiny::radioButtons(
               "type",
               label = "Select tour type",
               choices = c("Guided", "Little", "Grand"),
               selected = "Grand"
             ),
             shiny::conditionalPanel(
               "input.type == 'Guided'",
               shiny::selectInput(
                 "guidedIndex",
                 "Index function",
                 c("Holes", "Centre Mass", "LDA", "PDA")
                 ,
                 selected = "LDA"
               )
             ),

             shiny::fileInput("file1", "Choose CSV File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
             ),

             shiny::sliderInput(
               "speed",
               label =  "Tour speed",
               min = 0,
               max = 5,
               value = 1,
               step = 0.1
             ) ,
             shiny::actionButton("restart_random", "Restart tour with random basis"),
             shiny::selectInput("point_label",choices = vector('character'), label = "Select labeling variable"),
             shiny::selectInput("class", choices = vector('character'), label = "Select class variable to colour the points"),
             shiny::conditionalPanel( condition = "output.numC",
                               shiny::sliderInput("cMax",label = "Threshold value", min=0, max =1, value= 0.5, step = 0.1))
      ),
      shiny::column(3,
             shiny::checkboxGroupInput(
               "variables",
               label = "Choose variables for the 2D tour",
               choices = vector('character'),
               selected = vector('character')
             )
      ),
      shiny::column(6,
             shiny::tags$script(src = "https://d3js.org/d3.v4.min.js"),
             shiny::tags$script(src="https://d3js.org/d3-scale-chromatic.v1.min.js"),
             shiny::tags$div(id = "d3_output"),
             shiny::tags$div(id = "d3_output_2"),
             shiny::tags$div(id = "info"),
             shiny::tags$div(id = "info_2"),
             shiny::includeScript(system.file("js/d3anim.js", package = "tourrGUID3"))
             )
    )
  )

  server <- function(input, output, session) {
    fps <- 33
    aps <- 5

    rv <- shiny::reactiveValues()

    rv$d <- utils::read.csv(inputDataFile, stringsAsFactors = FALSE)

    shiny::observeEvent(input$restart_random,
                 {

                   p <- length(input$variables)
                   if(p==0){return()}
                   b <- matrix(stats::runif(2*p), p, 2)

                   rv$tour <- tourr::new_tour(as.matrix(rv$d[input$variables]),
                                       choose_tour(input$type, input$guidedIndex, c(rv$class[[1]])),
                                       b)
                   rv$ini <- FALSE
                 }, ignoreInit = TRUE)

    shiny::observeEvent(input$speed, rv$aps <- input$speed)

    shiny::observeEvent(input$file1, {
      inFile <- input$file1
      rv$d <- utils::read.csv(inFile$datapath, stringsAsFactors = FALSE)
    })

    shiny::observeEvent(rv$d, {

      rv$nums <- sapply(rv$d, is.numeric)
      rv$groups <- sapply(rv$d, is.character)

      shiny::updateCheckboxGroupInput(
        session, "variables",
        choices = names(rv$d[rv$nums]),
        selected = names(rv$d[rv$nums])[1:3]
      )

      shiny::updateSelectInput(session, "class", choices = names(rv$d),
                        selected = names(rv$d[rv$groups])[1])
      shiny::updateSelectInput(session, "point_label", choices = names(rv$d), selected = names(rv$d)[1])

    })


    shiny::observeEvent(c(input$type, input$variables, input$guidedIndex, input$class, input$point_label, input$cMax),
                 {

                   session$sendCustomMessage("debug", paste("Changed tour type to ", input$type))
                   if (length(input$variables) == 0) {
                     rv$mat <- tourr::rescale(as.matrix(rv$d[names(rv$d[rv$nums])[1:3]]))
                     rv$vars <- names(rv$d[rv$nums])[1:3]
                     rv$class <- unname(rv$d[names(rv$d)[1]])
                     if (is.numeric(rv$class[,1])){
                       output$numC <- shiny::reactive(TRUE)
                       minC <- min(rv$d[names(rv$d)[1]])
                       maxC <- max(rv$d[names(rv$d)[1]])
                       if((input$cMax >= minC) & (input$cMax <= maxC) ){medC <- input$cMax}
                       else{medC <- stats::median(rv$d[names(rv$d)[1]][,1])}
                       stepC <- (max(rv$d[names(rv$d)[1]]) - min(rv$d[names(rv$d)[1]])) / 100
                       cl <- rv$class[,1]
                       rv$class <- unname(ifelse(rv$d[names(rv$d)[1]] > input$cMax, "Larger", "Smaller"))
                       shiny::updateSliderInput(session, "cMax", min=minC, max=maxC, value=medC, step=stepC)
                     }
                     else{
                       rv$class <- unname(rv$d[input$class])
                       output$numC <- shiny::reactive(FALSE)
                       cl <- rv$class[[1]]
                     }
                     rv$pLabel <- unname(rv$d[names(rv$d)[1]])
                   } else {

                     rv$mat <- tourr::rescale(as.matrix(rv$d[input$variables]))
                     rv$vars <- input$variables
                     if (rv$nums[input$class]){
                       output$numC <- shiny::reactive(TRUE)
                       minC <- min(rv$d[input$class])
                       maxC <- max(rv$d[input$class])
                       if((input$cMax >= minC) & (input$cMax <= maxC) ){medC <- input$cMax}
                       else{medC <- stats::median(rv$d[input$class][,1])}
                       stepC <- (max(rv$d[input$class]) - min(rv$d[input$class])) / 100
                       cl <- rv$class[,1]
                       rv$class <- unname(ifelse(rv$d[input$class] > input$cMax, "Larger", "Smaller"))
                       shiny::updateSliderInput(session, "cMax", min=minC, max=maxC, value=medC, step=stepC)


                     }
                     else{
                       rv$class <- unname(rv$d[input$class])
                       output$numC <- shiny::reactive(FALSE)
                       cl <- rv$class[[1]]
                     }
                     shiny::outputOptions(output, "numC", suspendWhenHidden = FALSE)
                     rv$pLabel <- unname(rv$d[input$point_label])
                   }


                   session$sendCustomMessage("newcolours", unique(cl))

                   rv$tour <-
                     tourr::new_tour(rv$mat,
                              choose_tour(input$type, input$guidedIndex, cl))
                   rv$ini <- FALSE
                   rv$stopNext <- FALSE
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

    shiny::observe({

      if (length(rv$mat[1, ]) < 3) {
        session$sendCustomMessage(type = "debug", message = "Error: Need >2 variables.")
      }

      if (rv$stopNext){
        session$sendCustomMessage(type = "debug", message = "Guided tour finished: no better bases found.")
        return()
      }

      aps <- rv$aps
      tour <- rv$tour

      if(!rv$ini){
        step <- tour(0)
        rv$ini <- TRUE
      }
      else{
        step <- rv$tour(aps / fps)
      }

      shiny::invalidateLater(1000 / fps)

      j <- tourr::center(rv$mat %*% step$proj)
      j <- cbind(j, class = rv$class)
      colnames(j) <- NULL

      if (step$step == -1) rv$stopNext <- TRUE # step$step = -1 is telling us that this is the final projection
      else rv$stopNext <- FALSE

      session$sendCustomMessage(type = "data",
                                message = list(d = jsonlite::toJSON(data.frame(pL=rv$pLabel[,1],x=j[,2],y=j[,1],c=j[,3])),
                                                a = jsonlite::toJSON(data.frame(n=rv$vars,y=step$proj[,1],x=step$proj[,2]))))



    })



    choose_tour <- function(type,
                            subtype = "",
                            group_variable = ""
                            )

    {


      if (type == "Grand")
      {
        tourType <- tourr::grand_tour()
      }
      else if (input$type == "Little") {
        tourType <- tourr::little_tour()

      } else

      {
        if (subtype == "Holes") {
          tourType <- tourr::guided_tour(holes_())
        } else if (subtype == "Centre Mass") {
          tourType <- tourr::guided_tour(cmass_())
        }
        else if (subtype == "LDA") {
          tourType <- tourr::guided_tour(tourr::lda_pp(group_variable))
        } else if (subtype == "PDA") {
          tourType <- tourr::guided_tour(tourr::pda_pp(group_variable))
        }
      }

      return(tourType)
    }

  }
  shiny::shinyApp(ui, server)
}
