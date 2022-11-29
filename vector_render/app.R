## adapted from: https://stackoverflow.com/a/64537550
## unchecking stuff, maybe? https://stackoverflow.com/questions/44478182/how-to-access-remember-unchecked-values-in-shiny
## removing stuff, maybe? https://stackoverflow.com/questions/31454185/how-to-add-remove-input-fields-dynamically-by-a-button-in-shiny
## removing stuff again, maybe? https://stackoverflow.com/questions/62722354/dynamically-changing-the-widgets-page-based-on-function-parameters-in-shiny

if(!require("shiny")){
    install.packages("shiny")
}
if(!require("tidyverse")){
    install.packages("tidyverse")
}
if(!require("geomtextpath")){
    install.packages("geomtextpath")
}
if(!require("intervals")){
    install.packages("intervals")
}
if(!require("shinyDirectoryInput")){
    if(!require("remotes")){
        install.packages("remotes")
    }
    remotes::install_github("wleepang/shiny-directory-input")
}

library(shiny)
library(tidyverse)
library(geomtextpath)
library(intervals)
library(shinyDirectoryInput)

MIDDLE <- 2
WIDTH <- 0.2
ARROW_WIDTH <- 0.1
ARROW_LEN <- 10
FONT_SIZE <- 5
PLOT_HEIGHT <- 400 ## in px
PREVIEW_RES <- 72 ## dpi
SAVE_RES <- 300


# > HELPER FUNCTIONS ###########################################################

## |__ RANGE ===================================================================

## 'ranges' is a 2-column dataframe,
## where column 1 is the start pos and column 2 is the end pos
union <- function(ranges, max_len){
  ## split intervals spanning 0
  new_ranges <- data.frame(start = as.numeric(),
                           end = as.numeric())
  add_range <- function(r1, r2){
    rbind(new_ranges, data.frame(start = r1, end = r2))
  }
  for (i in 1:nrow(ranges)){
    r <- ranges[i,]
    if (r[[1]] > r[[2]]){
      new_ranges <- add_range(r[[1]], max_len)
      new_ranges <- add_range(1, r[[2]])
    } else {
      new_ranges <- add_range(r[[1]], r[[2]])
    }
  }
  iranges <- Intervals(new_ranges)
  return(as.data.frame(interval_union(iranges)))
}

## 'df' is a 2-column dataframe,
## where column 1 is the start pos and column 2 is the end pos
first_outside_ranges <- function(df, max_len){
  udf <- union(df[,c("start", "end")],
               max_len) %>%
    dplyr::arrange(V1, V2)
  colnames(udf) <- c("start", "end")
  if (!(1 %in% udf$start 
        && max_len %in% udf$end)){
    coord_offset <- 0.5
  } else {
    coord_offset <- udf[1,"end"]+0.5
  }
  return(coord_offset)
}

## make a function that takes a coordinate and offsets it
make_make_coord <- function(coord_offset, max_len){
  make_coord <- function(p, start = TRUE){
    new_p <- p+max_len-coord_offset
    if (new_p < max_len){return(new_p)}
    else if (new_p > max_len){return(new_p%%max_len)}
    else if (start){return(0)}
    else {return(max_len)}
  }
  return(make_coord)
}

## |__ PLOTTING FUNCTIONS ======================================================

## 'direction' is a numeric argument determining arrow direction
## if 'direction' >= 0: arrow points to positive, else negative
## 'arrow_width' is IN ADDITION to 'width'
## - i.e. actual arrow width is width + arrow_width
## 'width' and 'arrow_width' is the width on ONE SIDE of centre
make_arrow_coords <- function(
    xmin, xmax, mid = MIDDLE, width = WIDTH, 
    arrow_width = ARROW_WIDTH, arrow_len = ARROW_LEN,
    direction = 1){
  ## if no arrow_len, just draw rectangle
  if (arrow_len == 0){
    return(list(c(xmin, mid+width),
                c(xmin, mid-width),
                c(xmax, mid-width),
                c(xmax, mid+width),
                c(xmin, mid+width)))
  }
  ## if feature length is shorter than arrow length,
  ## just draw a triangle
  else if ((xmax - xmin) < arrow_len){
    if (direction >= 0){
      return(list(c(xmin, mid+width+arrow_width),
                  c(xmin, mid-width-arrow_width),
                  c(xmax, mid)))
    } else {
      return(list(c(xmax, mid+width+arrow_width),
                  c(xmax, mid-width-arrow_width),
                  c(xmin, mid)))
    }
  } 
  ## if no arrow_width, then draw a house lol
  else if (arrow_width == 0) {
    if (direction >= 0){
      return(list(c(xmin, mid+width),
                  c(xmax-arrow_len, mid+width),
                  c(xmax, mid),
                  c(xmax-arrow_len, mid-width),
                  c(xmin, mid-width)))
    } else {
      return(list(c(xmax, mid+width),
                  c(xmin+arrow_len, mid+width),
                  c(xmin, mid),
                  c(xmin+arrow_len, mid-width),
                  c(xmax, mid-width)))
    }
  } 
  ## full arrows
  else {
    if (direction >= 0){
      return(list(c(xmin, mid+width),
                  c(xmax-arrow_len, mid+width),
                  c(xmax-arrow_len, mid+width+arrow_width),
                  c(xmax, mid),
                  c(xmax-arrow_len, mid-width-arrow_width),
                  c(xmax-arrow_len, mid-width),
                  c(xmin, mid-width)))
    } else {
      return(list(c(xmax, mid+width),
                  c(xmin+arrow_len, mid+width),
                  c(xmin+arrow_len, mid+width+arrow_width),
                  c(xmin, mid),
                  c(xmin+arrow_len, mid-width-arrow_width),
                  c(xmin+arrow_len, mid-width),
                  c(xmax, mid-width)))
    }
  }
}

reactive_df_polygon <- function(df, arrow_len = 0, ...){
  output <- data.frame()
  for (i in 1:nrow(df)){
    row <- df[i,]
    if (row$strand == "NA"){
      coords <- make_arrow_coords(row$xmin, row$xmax, ...,
                                  arrow_len = 0)
    } else if (row$strand == '+'){
      coords <- make_arrow_coords(row$xmin, row$xmax, ..., 
                                  arrow_len = arrow_len,
                                  direction = 1)
    } else {
      coords <- make_arrow_coords(row$xmin, row$xmax, ..., 
                                  arrow_len = arrow_len,
                                  direction = -1)
    }
    new_df <- df[rep(i, length(coords)),]
    coords_df <- do.call(
      rbind, 
      lapply(coords,
             function(x) as.data.frame(t(x))))
    colnames(coords_df) <- c('x', 'y')
    new_df <- cbind(new_df, coords_df, 
                    data.frame(id = rep(i, length(coords))))
    output <- rbind(output, new_df)
  }
  return(output)
}

# > MODULE #####################################################################

## |__ MODULE UI ===============================================================

featuresUI <- function(
    id,
    default_name = NA,
    default_start = NA,
    default_end = NA,
    default_strand = NA){
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 1,
        checkboxInput(
          inputId = ns("show"),
          label   = " ",
          value   = TRUE
        )
      ),
      
      column(
        width = 5,
        textInput(
          inputId = ns("feature"),
          label   = "Feature name",
          value   = default_name
        )
      ),
      
      column(
        width = 2,
        numericInput(
          inputId = ns("start"),
          label   = "Start",
          value   = default_start,
          min     = 1
        )
      ),
      
      column(
        width = 2,
        numericInput(
          inputId = ns("end"),
          label   = "End",
          value   = default_end, 
          min     = 1
        )
      ),
      
      column(
        width = 2,
        selectInput(
          inputId  = ns("strand"),
          label    = "Strand",
          choices  = c('+' = '+', '-' = '-', 'NA' = NA),
          selected = default_strand
        )
      )
    )
  )
}


## |__ MODULE SERVER ===========================================================

feature <- function(input, output, session){
  reactive({
    req(input$feature, input$start, input$end)
    list(name = input$feature,
         start = input$start,
         end = input$end,
         show = input$show,
         strand = input$strand)
  })
}

# Shiny ########################################################################

## |__ UI ======================================================================

ui <- fluidPage(
  fluidRow(
    column(
      width = 7,
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = "vector.length",
            label   = "Vector length",
            value   = 1000,
            min     = 1
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "offset",
            label   = "Centre",
            value   = 0,
            min     = 0
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "scale",
            label   = "Scale",
            value   = 1,
            min     = 0
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "zoom",
            label   = "Zoom",
            value   = 1,
            min     = 0
          )
        )
      ),
      h5(""),
      actionButton("insertBtn", "Add another feature")
    ),
    column(
      width = 5,
      fluidRow(
        plotOutput("plotPlasmid", width = "100%", height = paste0(PLOT_HEIGHT, "px")),
        h3("Plot aesthetics"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "arrow.len",
              label   = "Arrowhead length",
              value   = 1,
              min     = 0
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "arrow.width",
              label   = "Arrowhead width",
              value   = 1,
              min     = 0
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId  = "stroke",
              label    = "Stroke",
              choices  = c('Show' = TRUE, 
                           'Hide' = FALSE),
              selected = "Show"
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "font.size",
              label   = "Font size",
              value   = FONT_SIZE,
              min     = 0
            )
          )
        ),
        h3("Download map"),
        h5(strong("Output directory")),
        fluidRow(
          column(
            width = 4,
            actionButton(
              inputId = "directory",
              label   = "Select directory"
            )
          ),
          column(
            width = 8,
            textOutput("wd")
          )
        ),
        h5(""),
        textInput(
          inputId = "fname",
          label   = "Output filename (without extension)",
          value   = "my_vector"
        ),
        fluidRow(
          column(
            width = 6,
            checkboxGroupInput(
              inputId  = "fmt",
              label    = "Export format(s)",
              choices  = list("eps" = "eps",
                              "jpeg" = "jpeg",
                              "pdf" = "pdf",
                              "png" = "png",
                              "svg" = "svg"),
              selected = "pdf"
            )
          ),
          column(
            width = 6,
            fluidRow(
              numericInput(
                inputId = "save.res",
                label   = "Resolution (dpi)",
                value   = SAVE_RES,
                min     = 1
              ),
              column(
                width = 4,
                actionButton("download", "Download"),
              )
            )
          )
        )
      )
    )
  )
)

## ui <- ui.2

## |__ Server ==================================================================

server <- function(input, output) {
  
  output$wd <- renderText({getwd()})
  
  ## \__ Get vector length & offset --------------------------------------------
  
  vectorLength <- reactive({
    req(input$vector.length, input$offset)
    list(vector.length = input$vector.length, 
         offset = input$offset)
  })
  
  ### \__ Create Reactive Feature ----------------------------------------------
  
  tmpFeatures <- reactiveValues()
  
  ### \__ Example UI Elements --------------------------------------------------
  
  # ## Make example features
  # examples <- list('1' = list(name = "geneA",
  #                             start = 900, end = 200,
  #                             strand = NA),
  #                  '2' = list(name = "promoter",
  #                             start = 780, end = 880,
  #                             strand = '+'),
  #                  '3' = list(name = "promoter",
  #                             start = 400, end = 500,
  #                             strat = '-'))
  # features <- list()
  # 
  # for (fid in names(examples)){
  #   ex <- examples[[fid]]
  #   insertUI(
  #     selector = "h5",
  #     where    = "beforeEnd",
  #     ui       = tagList(featuresUI(
  #       paste0("feature", fid),
  #       default_name = ex$name,
  #       default_start = ex$start,
  #       default_end = ex$end,
  #       default_strand = ex$strand))
  #   )
  #   assign(paste0("feature", fid), 
  #          callModule(feature, paste0("feature", fid)))
  #   # feature_new <- callModule(
  #   #   feature, paste0("feature", fid))
  #   observeEvent(get(paste0("feature", fid))(), {
  #     tmpFeatures[[fid]] <- get(paste0("feature", fid))()
  #   }, ignoreNULL = FALSE)
  #   # print(features)
  #   # print(tmpFeatures)
  #   # observe(tmpFeatures[[fid]] <- get(paste0("feature", fid))())
  # }
  
  ## Element 1
  insertUI(
    selector = "h5",
    where    = "beforeEnd",
    ui       = tagList(featuresUI(
      paste0("feature", 1),
      default_name = "geneA",
      default_start = 900,
      default_end = 200,
      default_strand = NA))
  )

  ### Update Reactive Feature with first feature

  feature01 <- callModule(feature, paste0("feature", 1))

  observe(tmpFeatures[['1']] <- feature01())

  ## Element 2
  insertUI(
    selector = "h5",
    where    = "beforeEnd",
    ui       = tagList(featuresUI(
      paste0("feature", 2),
      default_name = "promoter",
      default_start = 780,
      default_end = 880,
      default_strand = '+'))
  )

  ### Update Reactive Feature with first feature

  feature02 <- callModule(feature, paste0("feature", 2))

  observe(tmpFeatures[['2']] <- feature02())

  ## Element 3
  insertUI(
    selector = "h5",
    where    = "beforeEnd",
    ui       = tagList(featuresUI(
      paste0("feature", 3),
      default_name = "promoter",
      default_start = 400,
      default_end = 500,
      default_strand = '-'))
  )

  ### Update Reactive Feature with first feature

  feature03 <- callModule(feature, paste0("feature", 3))

  observe(tmpFeatures[['3']] <- feature03())
  
  ### \__ Other UI Elements ----------------------------------------------------
  ### Add other UI elements with column names and update the feature list
  
  observeEvent(input$insertBtn, {
    
    btn <- sum(input$insertBtn, 3)
    
    insertUI(
      selector = "h5",
      where    = "beforeEnd",
      ui       = tagList(featuresUI(paste0("feature", btn)))
    )
    
    newFeature <- callModule(feature, paste0("feature", btn))
    
    observeEvent(newFeature(), {
      tmpFeatures[[paste0("'", btn, "'")]] <- newFeature()
    }, ignoreNULL = FALSE)
    
  })
  
  ### \__ Make Plotting Dataframe ----------------------------------------------
  
  dfReactive <- reactive({
    req(tmpFeatures)
    
    tmpList <- reactiveValuesToList(tmpFeatures)
    if (length(tmpList) == 0){
      return(data.frame())
    }
    
    df <- map_df(tmpList, as_tibble) %>%
      as.data.frame() %>%
      dplyr::filter(show)
    
    return(df)
  })
  
  arrowWidth <- reactive({
    return(ARROW_WIDTH*input$arrow.width)
  })
  
  arrowLen <- reactive({
    req(input$vector.length)
    return((input$vector.length/100)*input$arrow.len)
  })
  
  dfToPlot <- reactive({
    
    df <- dfReactive()
    if (nrow(df) == 0){return(df)}
    
    ## coord_offset offsets all features so that none
    ## of them intersect with the space between
    ## <vector len> and 0
    coord_offset <- first_outside_ranges(
      df[,c("start", "end")],
      input$vector.length)
    effective_offset <- coord_offset-input$offset
    
    ## adjust plotting coordinates
    make_coord <- make_make_coord(
      coord_offset,
      input$vector.length)
    df <- df %>%
      dplyr::mutate(xmin = sapply(start, make_coord, start = TRUE),
                    xmax = sapply(end, make_coord, start = FALSE),
                    offset = effective_offset,
                    labpos = (xmax + xmin)/2) %>%
      reactive_df_polygon(arrow_len = arrowLen(),
                          arrow_width = arrowWidth())
    df
  })
  
  ### \__ Plot Vector Map ------------------------------------------------------
  
  plotInput <- reactive({
    mid <- MIDDLE*input$scale
    ylims <- c(0, MIDDLE*input$scale*input$zoom+WIDTH+WIDTH)
    xlims <- c(0, input$vector.length)
    
    df <- dfToPlot()
    
    ## base plot
    p <- ggplot() + 
      geom_hline(yintercept = mid, colour = "black") + 
      coord_polar() +
      theme_void() +
      ggplot2::theme(legend.position = "none") +
      # Specify how long the plasmid will be (using length of sequence)
      ggplot2::xlim(xlims) +
      # Zoom the circle (how much of the 'viewport' should it take up?)
      # The lower the zoom level, the more room there is for labels to be
      # arranged around the outside of the plot
      ggplot2::ylim(ylims)
    
    if (nrow(df) == 0){
      return(p)
    }
    
    ## geom_textpath+coord_polar combo doesn't play nice if your dataframe only has 1 row
    df.lab <- data.frame(labpos = c(df$labpos, c(0, 0)),
                         name = c(df$name, c(' ', ' ')),
                         id = c(df$id, c(-1, -2))) %>%
      dplyr::distinct()
    plot_offset <- ((((df[1,"offset"]+1)%%input$vector.length)-1)/
                      input$vector.length)*2*pi
    
    p <- p +
      coord_polar(start = plot_offset) +
      geom_polygon(data = df,
                   aes(x = x, y = y,
                       group = id,
                       fill = name),
                   colour = ifelse(input$stroke,
                                   "black",
                                   NA)) +
      geom_textpath(data = df.lab,
                    aes(x = labpos,
                        y = mid,
                        group = id,
                        label = name),
                    size = input$font.size)
    
    p
  })
  
  output$plotPlasmid <- renderPlot({
      plotInput()
  }, res = PREVIEW_RES)
  
  ## \__ Export Image----------------------------------------------------------
  
  observeEvent(input$directory, {
    new_dir <- choose.dir(getwd())
    if (!(is.na(new_dir) || is.null(new_dir))){
      setwd(new_dir)
    }
    output$wd <- renderText(getwd())
  })
  
  outputFbase <- reactive({
    req(input$fmt, input$fname)
    return(
      sapply(
        input$fmt,
        function(ext){
          file.path(getwd(),
                    paste0(input$fname,
                           '.', input$fmt))})
    )
  })
  
  ## download files (doesn't open a prompt)
  observeEvent(input$download, {
    fouts <- outputFbase()
    for (fout in fouts){
        ggsave(fout, plotInput(),
               h = input$save.res*PLOT_HEIGHT/PREVIEW_RES,
               w = input$save.res*PLOT_HEIGHT/PREVIEW_RES,
               units = "px")
    }
  })
}

## server <- server.2

#------------------------------------------------------------------------------#
shinyApp(ui, server)
