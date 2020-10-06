#TODO :
#Add req(input) for renderUI({}) outputs to remove inital error messages on load  

############# Libraries #########################
library(googleAuthR)
library(googleAnalyticsR)
library(bigrquery) #Use bigquery 
#library(bigQueryR)
library(DBI)

##Models
library(ChannelAttribution) #generating markov models
#source("channelAttribution.R")
#library(prophet)

##Shiny
library(DT)
library(rsconnect)
library(shiny)
#library(shinydashboard)
library(shinycssloaders)
library(shinybusy)

##Visualisation tools
library(ggplot2)
library(plotly)
library(purrrlyr)
library(visNetwork)
library(igraph)

##General
library(devtools)
library(sqldf) #SQL on data frames
library(Rcpp)
library(rlang)
library(dplyr)
library(reshape)
library(stringr)
library(data.table)
library(scales)
library(progress)
library(rlang)  
library(tibble)
library(hms)
library(pillar) 
library(ellipsis)
library(stringr)
library(readr)
library(lubridate)


#### Functions ####

source("functions.R")

############## Implementation ###########

source("setup.R")

##### Authentication Shiny #########


rsconnect::setAccountInfo(name='ddl-dentsu-aegis',
                          token='510A4371324FF078B438352530B8561E',
                          secret='CPLcTKr6TuUIbdLVJvbcbRgaav8xOpCj19JfRw9q')


####### Authentication Google #########

#authEmail = "analytics-support-emea@dentsuaegis.com"
#Authentication service Account - iprospect-manchester-analytics@appspot.gserviceaccount.com
set_client_token <- file.path(config$general$authentication$client_token)
token_path_ga <- file.path(config$general$authentication$service_account)
googleAuthR::gar_set_client(json = set_client_token,
                            scopes = c("https://www.googleapis.com/auth/analytics.readonly"#,
                                       #"https://www.googleapis.com/auth/analytics.edit"
                            )
)
#googleAnalyticsR::ga_auth(email = "analytics-support-emea@dentsuaegis.com")

gar_auth_service(json_file = token_path_ga)

###### Authentication Bigquery #######

token_path <- file.path(config$general$authentication$service_account)
bq_auth(path = token_path)
connection <- DBI::dbConnect(bigquery(), project = config$bq$project)


########### Global ##########

pok = T # Make F when app ready 

bqFields <- read.csv("bq_fields.csv")
fieldsMerge <- read.csv("fields_merge.csv")

segmentMap <- data.frame(
  name = config$general$segments$name,
  type = config$general$segments$type,
  stringsAsFactors = FALSE
)

segmentExpressionValues <- data.frame(name = NULL, values = NULL)
values <- config$general$segments$bigquery$standard$with_built_in_expression_value
if (!is.null(values)) {
  for (each_data_frame in 1:length(values)) {
    if (is.null(segmentExpressionValues)) {
      segmentExpressionValues <- values[[each_data_frame]]
    } else {
      segmentExpressionValues <- rbind(segmentExpressionValues, values[[each_data_frame]])
    }
  }  
} else {
  NULL
}

bqCookie <- "fullVisitorId"


########### UI #####################

ui <- shinyUI(fluidPage(
  fluidRow(
    column(
      width = 2,
      imageOutput("image.client", width = 100, height = 80, inline = TRUE)
    ),
    column(
      width = 9,
      h1("Custom Markov Attribution")
    ),
    column(
      width = 1,
      imageOutput("image.ddl", width = 100, height = 80, inline = TRUE)
    )
  ),
  uiOutput("in.pss"),
  tabsetPanel(
    sidebarLayout(
      position = "left", 
      sidebarPanel(
        width=3,
        uiOutput("in.markov.report"),
        uiOutput("in.markov.method.custom"),
        
        uiOutput("in.markov.property"),
        uiOutput("in.markov.viewId"),
        
        br(),
        uiOutput("in.markov.dateRange"),
        uiOutput("in.markov.lookBackPeriod"),
        
        uiOutput("in.markov.dimension"),
        uiOutput("in.markov.secondary.dimension.logic"),
        uiOutput("in.markov.secondary.dimension"),
        uiOutput("in.markov.secondary.filter"),
        
        uiOutput("in.markov.conversion.type"),
        uiOutput("in.markov.conversion"),
        
        uiOutput("in.markov.filter.logical"),
        uiOutput("in.markov.filter.type"),
        uiOutput("in.markov.filter.builtin"),
        
        uiOutput("in.markov.filter.expression.find"),
        br(),
        uiOutput("in.markov.filter.dimension"),
        uiOutput("in.markov.filter.dimension.type"),
        uiOutput("in.markov.filter.operator"),
        uiOutput("in.markov.filter.expression"),
        
        uiOutput("in.markov.go"),
        
        uiOutput("in.markov.order.estimate"),
        uiOutput("in.markov.order.choose"),
        uiOutput("in.markov.removeNonConversionPaths")
      ),
      mainPanel(
        uiOutput("in.markov.ga.bq.unnest"),
        #“double-bounce”, “circle”, “bounce”, “folding-cube”, “rotating-plane”, “cube-grid”, “fading-circle”, “dots”, “cube”, “flower”, “pixel”, “hollow-dots”, “intersecting-circles”, “orbit”, “radar”, “scaling-squares”, “half-circle”, “trinity-rings”, “fulfilling-square”, “circles-to-rhombuses”, “semipolar”, “self-building-square”, “swapping-squares”, “fulfilling-bouncing-circle”, “fingerprint”, “spring”, “atom”, “looping-rhombuses”, “breeding-rhombus”
        use_busy_spinner(
          spin = "flower", 
          position = 'bottom-left', 
          margin = c(500,800),
          spin_id = "spin.run.query"
        ), 
        fluidRow(
          column(
            width = 4,
            uiOutput("in.markov.filter.advanced.type")    
          ),
          column(
            width = 7,
            uiOutput("in.markov.filter.advanced.audience.name")    
          )
        ),
        uiOutput("in.markov.filter.advanced.audience.table"),
        tabsetPanel(type = "tabs",
                    tabPanel("Model Comparison", 
                             br(),
                             fluidRow(
                               column(width = 4,
                                      uiOutput("in.markov.model1")    
                               ),
                               column(width = 1,
                                      uiOutput("in.markov.vs")
                               ),
                               column(width = 4,
                                      uiOutput("in.markov.model2"),    
                               ),
                               column(width = 3, style = "margin-top: 25px;",
                                      uiOutput("in.markov.export")    
                               )
                             ),
                             DT::dataTableOutput("markov.attribution.totals"),
                             DT::dataTableOutput("markov.attribution"),
                             DT::dataTableOutput("markov.test")
                    ),
                    tabPanel("Removal Effect", 
                             DT::dataTableOutput("markov.rawRemoval")
                    ),
                    tabPanel("Raw Models Data", 
                             DT::dataTableOutput("markov.attributionTest")
                    ),
                    tabPanel("Model Info", 
                             visNetworkOutput("markov.modelInfoVis", height = "600px"),
                             DT::dataTableOutput("markov.modelInfoRaw")
                    ),
                    tabPanel("Path Data",
                             fluidRow(
                               column(width = 6,
                                      uiOutput("in.markov.path.top"),
                                      uiOutput("in.markov.path.segment")  
                               ),
                               column(width = 6,
                                      uiOutput("in.markov.path.filter")  
                               )
                             ),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Flow",
                                                  uiOutput("in.markov.path.depth"),
                                                  uiOutput("in.markov.path")    
                                         ),
                                         tabPanel("Scatter",
                                                  uiOutput("in.markov.path.scatter")
                                         )
                             )
                    ),
                    tabPanel("Queries", 
                             DT::dataTableOutput("markov.gaQuery"),
                             uiOutput("in.markov.path.export"),
                             DT::dataTableOutput("markov.markovQuery")
                             
                    )
        )
      )
    )
  )
))

############### Server ####################

server <- function(input, output, session) {
  
  observe({
    if (!pok) {
      password <- input$pss
      if (!is.null(password) && password == '(G2B38"£BWBEoih")($bew3r0Th3R') {
        pok <<- TRUE
      }
    }
  })
  
  ############ Shiny User Interface #####################
  
  output$in.pss <- renderUI({
    input$pss; if (pok) return(NULL) else return(textInput("pss", "Enter Password:",""))
  })
  
  output$image.client <- renderImage({
    list(
      src = "images/client.png",
      contentType = "image/png",
      width = 200,
      height = 80,
      alt = "Client"
    )
  }, deleteFile = FALSE)
  
  
  output$image.ddl <- renderImage({
    list(
      src = "images/ddl.jpg",
      contentType = "image/png",
      width = 105,
      height = 80,
      alt = "Dentu Data Labs"
    )
  }, deleteFile = FALSE)
  
  
  
  ########## Markov Attribution UI #####
  
  output$in.markov.report <- renderUI({
    input$pss; if (pok) 
      return(
        selectInput("markov.report", label = "Report", choices = c("Built-in", "Custom"))
      )
  })
  output$in.markov.method.custom <- renderUI({
    input$pss; if (pok && input$markov.report == "Custom") 
      return(
        selectInput("markov.method", label = "Pull Method", choices = c("Bigquery", "GA Unsampled"))
      )
  })
  
  output$in.markov.property <- renderUI({
    input$pss; if (pok) return(
      selectInput(
        "markov.property",
        label = "Property",
        choices = account_info()$name,
        selected = config$general$favourite$property
      )
    )
  })
  output$in.markov.viewId <- renderUI({
    input$pss; if (pok) return(uiOutput("markov.viewId"))
  })
  output$markov.viewId <- renderUI({
    h5(paste("Default View ID:", view_id()))  
  })
  output$in.markov.dateRange <- renderUI({
    input$pss; if (pok) return(dateRangeInput(inputId = "markov.dateRange", 
                                              label ="Date", 
                                              max = Sys.Date()
    )) else return(NULL)
  })
  output$in.markov.lookBackPeriod <- renderUI({
    numericInput(
      "markov.lookBackPeriod",
      label = "Look-back Window (days)",
      value = 360
    )
  })
  
  output$in.markov.dimension <-renderUI({
    input$pss; 
    if (pok) {
      
      if (input$markov.report == "Built-in") {
        dimension_list <- na.omit(config$general$builtin_dimension_list)
        select <- config$general$favourite$dimension
      } else {
        if (input$markov.method == "Bigquery") {
          config$general$builtin_dimension_list
          dimension_list <- config$general$builtin_dimension_list #TODO: WRITE DIMENSION LIST for this not from config
          select <- dimension_list[1] 
        } else {
          dimension_list <- available_dimensions()  
          select <- NULL
        }
      }
      return(
        selectInput(
          inputId = "markov.dimension",
          label = "Dimension:",
          choices = dimension_list, 
          selected = select
        )
      )
    } else {return(NULL)}
  })
  output$in.markov.secondary.dimension.logic <- renderUI({
    if (!is.null(config$general$with_secondary_dimension) || !is.null(config$general$with_secondary_dimension$secondary_dimension_list)) {
      if (config$general$favourite$secondary_dimension_on) {
        val <- TRUE
      } else {val <- FALSE}
      return(
        checkboxInput("markov.secondary.dimension.logic", label = "Secondary Dimension", value = val)
      )
    } else {
      return(NULL)
    }
  })
  output$in.markov.secondary.dimension <- renderUI({
    input$pss; 
    if (!is.null(config$general$with_secondary_dimension) || !is.null(config$general$with_secondary_dimension$secondary_dimension_list)) {
      if (pok && input$markov.secondary.dimension.logic) {
        if (input$markov.report == "Built-in") {
          dimension_list <- na.omit(config$general$with_secondary_dimension$secondary_dimension_list)
          select <- config$general$favourite$secondary_dimension
        } else if (input$markov.method == "Bigquery") {
          dimension_list <- c("v2ProductName") #TO DO WRITE Entire DIMENSION LIST for bq
          select <- dimension_list[1]
        } else {
          dimension_list <- available_dimensions()  
          select <- c("Page Path")
        }
        return(
          selectInput(
            inputId = "markov.secondary.dimension",
            label = NULL,
            choices = dimension_list, 
            selected = select
          )
        )
      } else {return(NULL)}
    } else {return(NULL)}
    
  })
  
  segment <- reactive({
    subset(segmentMap, name %in% input$markov.filter.type)
  })
  #The select variable for conversion.type and conversion may need to be edited when new segments are introduced 
  output$in.markov.conversion.type <- renderUI({
    input$pss; if (pok) {
      
      
      
      if ("Product" %in% segment()$type) {
        select <- "Ecommerce"  
      } else if ("Product" %ni% segment()$type) {
        select <- config$general$favourite$conversion_type
      } else {
        select <- NULL
      }
      
      return(
        selectInput(
          inputId = "markov.conversion.type",
          label = "Conversion KPI:",
          choices = c( "Ecommerce", "Goal"),
          selected = select
        )
      ) 
    } else return(NULL)
  })
  output$in.markov.conversion <- renderUI({
    input$pss;
    type <- input$markov.conversion.type
    if (pok) {
      
      if ("Product" %in% segment()$type) {
        select <- "Product Revenue"  
      } else if ("Product" %ni% segment()$type) {
        select <- config$general$favourite$conversion
      } else {
        select <- NULL
      }
      
      if (type == "Goal") {
        return(selectInput(
          inputId = "markov.conversion",
          label = NULL,
          choices = goals()$name,
          selected = select
        )
        )
      } else if (type == "Ecommerce") {
        
        return(selectInput(
          inputId = "markov.conversion",
          label = NULL,
          choices = c("Revenue", "Product Revenue"),
          selected = select
        )
        )
      }
    } else {return(NULL)}
  })
  
  output$in.markov.filter.logical <- renderUI({
    input$pss;
    if (pok) {
      checkboxInput("markov.filter.logical", "Segmentation", value = TRUE)
    } else {return(NULL)}
  })
  output$in.markov.filter.type <- renderUI({
    segment <- input$markov.filter.logical 
    if (pok && isTRUE(segment) && input$markov.report == "Built-in") {
      choices <- config$general$segments$name
      return(
        selectizeInput("markov.filter.type",
                       label = "Segments",
                       choices = choices,
                       multiple = TRUE,
                       selected = config$general$favourite$segment,
                       options = list(placeholder = config$general$favourite$segment)
        )
      )
    } else if (pok && isTRUE(segment) && input$markov.report == "Custom") {
      return(
        selectInput("markov.filter.type",
                    label = "Filter Type",
                    choices = c("Built-in", "Standard", "Advanced"),
                    selected = "Standard"
        )
      )
    } else {return(NULL)}
  })
  
  output$in.markov.filter.dimension <- renderUI({
    input$pss;
    segmentLogic <- input$markov.filter.logical 
    if (input$markov.report == "Built-in") {
      if (input$markov.dimension %in% config$bq$dimensions) {
        choices <- config$general$segments$bigquery$standard$names
      } else {
        choices <- unique(available_dimensions())
      }
      
      if (pok && isTRUE(segmentLogic) && ("Standard" %in% segment()$type || segment()$type == "Standard") ){
        return(
          selectInput("markov.filter.dimension",
                      label = "Filter Logic",
                      choices = choices,
                      selected = "Channel Grouping"
          )  
        )
      } else {return(NULL)}
    } else if (input$markov.report == "Custom") {
      if (input$markov.method == "Bigquery") {
        choices <- as.character(bqFields$tidy.name)
      } else {
        choices <- unique(available_dimensions())
      }  
      standardFilter <- input$markov.filter.type == "Standard"
      if (pok && isTRUE(segmentLogic) && ("Standard" %in% segment()$type || isTRUE(standardFilter))) {
        return(
          selectInput("markov.filter.dimension",
                      label = "Filter Logic",
                      choices = choices,
                      selected = "Channel Grouping"
          )  
        )
      }
    } else {return(NULL)}
  })
  output$in.markov.filter.dimension.type <- renderUI({
    input$pss;
    segment <- input$markov.filter.logical 
    standardFilter <- input$markov.filter.type == "Standard"
    if (pok && isTRUE(segment) && ("Standard" %in% segment()$type || isTRUE(standardFilter))){
      dimensionType <- bq_dimension(bqFields, input$markov.filter.dimension)$type
      h5(paste("Dimension Type:", dimensionType))   
    } else {return(NULL)}
    
  })
  output$in.markov.filter.operator <- renderUI({
    input$pss;
    segment <- input$markov.filter.logical 
    standardFilter <- input$markov.filter.type == "Standard"
    
    if (!is.null(segmentExpressionValues) && input$markov.report == "Built-in" && isTRUE(segment) && (isTRUE(standardFilter) || segment()$type == "Standard")) {
      expression <- segmentExpressionValues %>%
        filter(name == input$markov.filter.dimension)
      if (isTRUE(unique(expression$select_multiple_values))) {
        return(NULL)
      } else {return(NULL)}
    } else if (pok && isTRUE(segment) && (isTRUE(standardFilter) || segment()$type == "Standard")) {
      if (input$markov.method == "Bigquery" || input$markov.dimension %in% config$bq$dimensions) {
        choices = c("REGEXP", "BEGINS_WITH", "ENDS_WITH", "EXACT")
      } else {
        choices =  c("REGEXP", "BEGINS_WITH", "ENDS_WITH", "PARTIAL", "EXACT")
      }
      
      selectInput("markov.filter.operator",
                  label = NULL,
                  choices = choices
      ) 
    } else {return(NULL)}
    
  })
  output$in.markov.filter.expression.find <- renderUI({
    input$pss;
    segment <- input$markov.filter.logical 
    standardFilter <- input$markov.filter.type == "Standard"
    
    if (pok && isTRUE(segment) && (isTRUE(standardFilter || "Standard" %in% segment()$type))) {
      if (input$markov.method == "Bigquery" || input$markov.dimension %in% config$bq$dimensions) {
        actionButton(
          "markov.filter.expression.find",
          label = paste("Find available values from",input$markov.filter.dimension)
        )
      } else {
        return(NULL)
      }
    } else {return(NULL)}
  })
  bq_dimension_choices <- eventReactive(input$markov.filter.expression.find, {
    
    project <- config$bq$project
    dataset <- view_id()
    startDate <- input$markov.dateRange[1]
    endDate <- input$markov.dateRange[2]
    dimension <- input$markov.filter.dimension
    #dimension <- "Hits Product V2 Product Name"
    dimension <- bq_dimension(bqFields, dimension)
    
    if (isTRUE(dimension$isArray)) {
      unnest <- paste(", UNNEST(hits) as hits, UNNEST(",as.character(dimension$bq.array),") as", as.character(dimension$bq.array))
    } else {
      unnest <- ", UNNEST(hits) as hits"
    }
    
    sql <- paste(
      'CREATE OR REPLACE TABLE
      `',project,'.',dataset,'.markov_dimension_choices`
      AS
      SELECT ',
      as.character(dimension$bq.name), '
      FROM `',project,'.',dataset,'.ga_sessions_*`',
      unnest, '
      WHERE 
        PARSE_DATE("%Y%m%d", date) BETWEEN "',startDate,'" AND "',endDate,'"
      GROUP BY ',as.character(dimension$bq.name),
      
      sep = ""
    )
    
    return(DBI::dbGetQuery(connection, sql))
    
  })
  output$in.markov.filter.expression <- renderUI({
    input$pss;
    segment <- input$markov.filter.logical 
    standardFilter <- input$markov.filter.type == "Standard"
    if (pok && isTRUE(segment) && (isTRUE(standardFilter) || "Standard" %in% segment()$type)) {
      if ((input$markov.method == "Bigquery" || input$markov.dimension %in% config$bq$dimensions) && input$markov.filter.expression.find > 0) {
        return(  
          selectizeInput(
            "markov.filter.expression",
            label = NULL,
            choices = bq_dimension_choices()[[1]],
            options = list(create = TRUE)
          )
        )
      } else { 
        if (!is.null(segmentExpressionValues) && "Standard" %in% segment()$type ) {
          expression <- segmentExpressionValues %>%
            filter(name == input$markov.filter.dimension)
          if (isTRUE(unique(expression$select_multiple_values))) {
            return(
              
              selectizeInput(
                "markov.filter.expression",
                label = NULL,
                choices = expression$values,
                selected = expression$values,
                multiple = TRUE,
                options = list(
                  create = TRUE,
                  delimiter = "|",
                  placeholder = paste(expression$values, collapse = "|", sep = "")
                )
              ) 
            )
          } else {
            return(
              selectizeInput(
                "markov.filter.expression",
                label = NULL,
                choices = expression$values,
                options = list(create = TRUE)
              ) 
            )  
          }
          
        } else {
          return(
            textInput("markov.filter.expression",
                      label = NULL
            )
          )
        }
      }
    } else {return(NULL)}
  })
  output$in.markov.filter.builtin <- renderUI({
    input$pss;
    if (input$markov.method == "Bigquery" || input$markov.dimension %in% config$bq$dimensions) {
      choices <- "New Customers"
    } else {
      choices <- NULL
    }
    segment <- input$markov.filter.logical 
    builtinFilter <- input$markov.filter.type == "Built-in"
    if (pok && isTRUE(segment) && isTRUE(builtinFilter)) {
      selectInput("markov.filter.builtin",
                  label = "Segment",
                  choices = choices,
                  selected = "New Customers"
      ) 
    } else {return(NULL)}
  })
  output$in.markov.filter.advanced.type <- renderUI({
    input$pss; 
    if (pok && 
        input$markov.filter.logical &&
        (input$markov.filter.type == "Advanced" || input$markov.filter.type == "Audience")
    ) {
      return(
        selectInput(
          "markov.filter.advanced.type",
          label = "Type of Advanced Filter",
          choices = c("Built-in", "Audience", "Advanced")
        )
      )
    } else {return(NULL)}
  })
  output$in.markov.filter.advanced.audience.name <- renderUI({
    if (pok && 
        input$markov.filter.logical && 
        (input$markov.filter.type == "Advanced" || input$markov.filter.type == "Audience") &&
        (input$markov.filter.advanced.type == "Audience" || input$markov.filter.advanced.type == "Built-in")
    ) {
      accountId <- config$ga$account
      propertyId <- property_id(accountId, input$markov.property)
      
      if (input$markov.filter.advanced.type == "Audience") {
        audience_list <- ga_remarketing_list(accountId, propertyId)$items$name
      } else if (input$markov.filter.advanced.type == "Built-in") {
        audience_list <- config$general$segments$bigquery$advanced$audience_builtin_name
      }
      return(
        selectInput(
          "markov.filter.advanced.audience.name",
          label = "Name",
          choices =  audience_list,
          width = "800px"
        )  
      )
    } else {return(NULL)}
  })
  output$in.markov.filter.advanced.audience.table <- renderUI({
    if (pok && 
        input$markov.filter.logical && 
        (input$markov.filter.type == "Advanced" || input$markov.filter.type == "Audience") &&
        input$markov.filter.advanced.type == "Audience" 
    ) {
      tableOutput("markov.filter.advanced.audience.table")
    } else {return(NULL)}    
  })
  #TO DO: Change type to "meta" when App is ready -->
  audience_metadata <- reactive({
    accountId <- config$ga$account
    propertyName <- input$markov.property
    audienceName <- input$markov.filter.advanced.audience.name 
    
    return(get_audience(accountId, propertyName, audienceName, type = "meta"))
  })
  output$markov.filter.advanced.audience.table <- renderTable({
    return(audience_metadata())
  })
  
  output$in.markov.go <- renderUI({
    input$pss; 
    if (pok) {
      if (input$markov.report == "Built-in") {
        if (input$markov.dimension %in% config$bq$dimensions) {
          action <- actionButton(
            "markov.runQuery", 
            "Run Query", 
            class = "btn-primary"
          )
        } else if (input$markov.dimension %in% config$ga$dimensions) {
          action <- actionButton(
            "markov.runQuery",
            "Pull GA Unsampled Report", 
            class = "btn-primary"
          )
        }
      } else {
        if (input$markov.method == "Bigquery") {
          action <- actionButton(
            "markov.runQuery",
            "Run Query", 
            class = "btn-primary"
          )
        } else if (input$markov.method == "GA Unsampled") {
          action <- actionButton(
            "markov.runQuery", 
            "Pull GA Unsampled Report", 
            class = "btn-primary"
          )
        }  
      }
      return(withBusyIndicatorUI(action))
    } else {return(NULL)}
  })
  
  
  output$in.markov.order.estimate <-renderUI({
    checkboxInput(
      "markov.order.estimate",
      "Estimate Most Appropriate Order",
      value = FALSE
    )
  })
  output$in.markov.order.choose <-renderUI({
    if (isFALSE(input$markov.order.estimate)) {
      numericInput(
        "markov.order.choose",
        label = "Choose Order",
        value = 1,
        min=1, 
        max = 10
      )  
    } else {NULL}
    
  })
  output$in.markov.removeNonConversionPaths <- renderUI({
    checkboxInput(
      "markov.removeNonConversionPaths",
      "Remove Non-conversion Paths",
      value = TRUE
    )
  })
  
  
  
  
  output$in.markov.model1 <-renderUI({
    input$pss; 
    if (input$markov.conversion.type == "Ecommerce") {select <- "Last Non Direct "} else {select <- "Last Non Direct"}
    if (pok) return(selectInput(inputId = "markov.model1",
                                label = "Model 1:",
                                choices = as.character(unique(sub('Value|Conversions', '', names(raw_attribution()[0:-1])))),
                                selected = select
    )) else return(NULL)
  })
  
  vs_text <- eventReactive(input$markov.model1, {
    h3("Vs.")
  })
  output$in.markov.vs <- renderUI({
    vs_text()
  })  
  output$in.markov.model2 <-renderUI({
    input$pss;
    if (input$markov.conversion.type == "Ecommerce") {select <- "Markov Total "} else {select <- "Markov Total"}
    if (pok) return(selectInput(inputId = "markov.model2",
                                label = "Model 2:",
                                choices = as.character(unique(sub('Value|Conversions', '', names(raw_attribution()[0:-1])))),
                                selected = select
    )) else return(NULL)
  })
  attribution_export <- eventReactive(input$markov.model1,{
    downloadButton("markov.export", "Export Data")
  })
  output$in.markov.export <- renderUI({
    attribution_export()
  })
  
  output$in.markov.path.export <- renderUI({
    input$pss; if (pok) return(downloadButton("markov.path.export", "Export Path Data"))
  })
  output$in.markov.path.top <- renderUI({
    input$pss; if (pok) return(
      numericInput("markov.path.top", "Segment Users By Top: ", 10, min = 1, max = 100)
    )
  })
  output$in.markov.path.segment <- renderUI({
    input$pss; if (pok) return(
      selectInput(
        inputId = "markov.path.segment",
        label = NULL,
        choices = c("Converters", "Non-Converters"),
        selected = "Converters"
      )
    )
  })
  output$in.markov.path.filter <- renderUI({
    input$pss; if (pok) return(
      textInput("markov.path.filter", "Filter Paths:", value = NULL)
    )
  })
  output$in.markov.path.depth <- renderUI({
    input$pss; if (pok) return(
      numericInput("markov.path.depth", "Depth", 5, min = 2, max = 100)
    )
  })
  output$in.markov.path <- renderUI({
    input$pss; if (pok) return(
      plotlyOutput("markov.path",height=800, width = 1200)
    )
  })
  output$in.markov.path.scatter <- renderUI({
    input$pss; if (pok) return(
      plotlyOutput("markov.path.scatter",height=800, width = 1200)
    )
  })
  
  ######################################## Markov Attribution ################################################################### 
  
  #                                       Markov Attribution 
  
  account_info <- reactive({
    filter(ga_webproperty_list(config$ga$account), level == "PREMIUM")
  }) 
  view_id <- reactive({
    property <- input$markov.property
    viewId <- filter(account_info(), name == property)
    viewId <- viewId$defaultProfileId
    return(viewId)
  })
  goals <- reactive({
    
    account <- config$ga$account
    accountInfo <- filter(ga_webproperty_list(account), level == "PREMIUM")
    
    property <- filter(accountInfo, name == input$markov.property)
    
    goals <- ga_goal_list(accountId = account, webPropertyId = property$id, profileId =  property$defaultProfileId )
    return(goals)
    
  })  
  goal_id <- reactive({
    return((filter(goals(), name == input$markov.conversion))$id)
  })
  goalXXCompletions <- reactive({
    goalIds <- goals()$id
    
    gaGoalIds <- NULL
    for (id in goalIds) {
      if (is.null(gaGoalIds) ) {
        gaGoalIds <- paste("goal", id, "Completions", sep="")
      } else {
        gaGoalIds <- append(gaGoalIds, paste("goal", id, "Completions", sep=""))
      }
    }
    return(gaGoalIds)
  })
  input_goal <- reactive({
    conversion  <- input$markov.conversion
    return(filter(goals(), name == conversion))
  })
  
  conversion_metric <- reactive({
    conversionType <- input$markov.conversion.type
    if (conversionType == "Goal") {
      goal <- input_goal()
      if (goal$type == "EVENT") {
        return("totalEvents")
      } else if (goal$type == "URL_DESTINATION") {
        return("uniquePageviews")
        #return(paste("goal", goal$id, "Completions", sep=""))
      }
    } else if (conversionType == "Ecommerce") {
      return(c("transactions", "transactionRevenue"))
    }
    
  })
  input_dimension <- reactive({
    
    #available_dimension(dimension) NEEDS TO BE ABLE TO DEAL WITH CONFIG
    
    
    if (input$markov.report == "Built-in") {
      
      #available_dimension() should include config for built-in
      if (input$markov.dimension %in% config$ga$dimensions) {
        return(available_dimensions(input$markov.dimension))  
      } else {
        dimension <- gsub(" ", "_", input$markov.dimension)
        return(dimension) 
      }
    } else {
      if (input$markov.method == "GA Unsampled") {
        return(available_dimensions(input$markov.dimension))  
      } else {
        dimension <- gsub(" ", "_", input$markov.dimension)
        return(dimension) 
      }  
    }
    
    
  })
  dimensions <- reactive({
    conversionType <- input$markov.conversion.type 
    if (conversionType == "Goal") {
      goal <- input_goal()
      if (goal$type == "EVENT") {
        if (input_dimension() != "pagePath") {
          return(append(input_dimension(), c("pagePath", "eventCategory", "eventAction")))
        } else {
          return(append(input_dimension(), c("eventCategory", "eventAction")))
        }
      } else if (goal$type == "URL_DESTINATION") {
        return(input_dimension()) 
      }  
    } else if (conversionType == "Ecommerce") {
      return(input_dimension())
    }
  })
  ga <- reactive({
    dimensions <- c("clientId","dateHourMinute")
    dimensions <- append(dimensions, dimensions())
    ga <- list(dimensions = dimensions, metrics = conversion_metric())  
  })
  
  
  ##### Initial Execution ####
  
  bq_ga_unnest_r <- eventReactive(input$markov.property, {
    return(bq_ga_unnest(config, dataset = view_id()))
  })
  output$in.markov.ga.bq.unnest <- renderUI({
    textOutput("ga.bq.unnest")
  })
  
  
  ############ rest of ##############
  
  
  observeEvent(input$markov.runQuery,{
    show_spinner()
    bq_path_data()
    bq_direct_data()
    #markov_data_paths()
    #direct_model()
    hide_spinner()
  })
  
  ################# Event Reactives (sql) -->  
  
  #TODO: MAKE WORK FOR WHEN input_dimension() is event category, action, label for ga_data()
  
  ##### GA WORKFLOW #####
  ga_data <- eventReactive(input$markov.runQuery, {
    input$pss;
    conversionType <- input$markov.conversion.type
    
    startDate <- input$markov.dateRange[1]
    endDate <- input$markov.dateRange[2]
    property <- input$markov.property
    
    viewId <- filter(account_info(), name == property)
    viewId <- viewId$defaultProfileId
    
    goal <- input_goal()
    
    if (input$markov.filter.logical) { 
      
      dimensionFilter <- available_dimensions(input$markov.filter.dimension)
      operatorFilter <- input$markov.filter.operator
      expressionFilter <- input$markov.filter.expression
      
      filter <- dim_filter(dimension = dimensionFilter, operator = operatorFilter, expressions = expressionFilter)
      standardFilter <- filter_clause_ga4(list(filter))
      
      if (conversionType == "Goal") {
        if (goal$type == "EVENT") {
          
          conditions <- event_conditions_from_goal(goal)
          
          if (length(conditions$dimensions) == 1) {
            category <- dim_filter(dimension=conditions$dimensions[[1]],operator=conditions$operators[[1]],expressions=conditions$expressions[[1]])
            
            conversionFilter <- filter_clause_ga4(list(filter, category), operator = "AND")
          } else if (length(conditions$dimensions) == 2) {
            category <- dim_filter(dimension=conditions$dimensions[[1]],operator=conditions$operators[[1]],expressions=conditions$expressions[[1]])
            action <- dim_filter(dimension=conditions$dimensions[[2]],operator=conditions$operators[[2]],expressions=conditions$expressions[[2]])
            
            conversionFilter <- filter_clause_ga4(list(filter, category, action), operator = "AND")
          } else if (length(conditions$dimensions) == 3) {
            category <- dim_filter(dimension=conditions$dimensions[[1]],operator=conditions$operators[[1]],expressions=conditions$expressions[[1]])
            action <- dim_filter(dimension=conditions$dimensions[[2]],operator=conditions$operators[[2]],expressions=conditions$expressions[[2]])
            label <- dim_filter(dimension=conditions$dimensions[[3]],operator=conditions$operators[[3]],expressions=conditions$expressions[[3]])
            
            conversionFilter <- filter_clause_ga4(list(filter, category, action, label), operator = "AND")
          }
          
          conversionData <- google_analytics(viewId = view_id(),
                                             date_range = c(startDate,endDate),
                                             metrics = ga()$metrics,
                                             dimensions = ga()$dimensions,
                                             dim_filters = conversionFilter,
                                             anti_sample = TRUE,
                                             anti_sample_batches = 1
          )
          
          data <- google_analytics(viewId = view_id(),
                                   date_range = c(startDate,endDate),
                                   metrics = ga()$metrics,
                                   dimensions = ga()$dimensions[1:3],
                                   dim_filters = standardFilter,
                                   anti_sample = TRUE
          )
          
          if (input_dimension() != "pagePath") {
            if (length(conditions$dimensions) == 1) {
              data$pagePath <- NA
              data$eventCategory <- NA
            } else if (length(conditions$dimensions) == 2) {
              data$pagePath <- NA
              data$eventCategory <- NA
              data$eventAction <- NA
            } else if (length(conditions$dimensions) == 3) {
              data$pagePath <- NA
              data$eventCategory <- NA
              data$eventAction <- NA
              data$eventLabel <- NA
            }
            data <- rbind(conversionData, data)
          } else {
            if (length(conditions$dimensions) == 1) {
              data$eventCategory <- NA
            } else if (length(conditions$dimensions) == 2) {
              data$eventCategory <- NA
              data$eventAction <- NA
            } else if (length(conditions$dimensions) == 3) {
              data$eventCategory <- NA
              data$eventAction <- NA
              data$eventLabel <- NA
            }
            data <- rbind(conversionData, data)
          }
          
          
        } else if (goal$type == "URL_DESTINATION") {
          
          data <- google_analytics(viewId = view_id(),
                                   date_range = c(startDate,endDate),
                                   metrics = ga()$metrics,
                                   dimensions = ga()$dimensions[1:3],
                                   dim_filters = standardFilter,
                                   anti_sample = TRUE
          )
          
        }
      } else if (conversionType == "Ecommerce") {
        data <- google_analytics(viewId = view_id(),
                                 date_range = c(startDate,endDate),
                                 metrics = ga()$metrics,
                                 dimensions = ga()$dimensions[1:3],
                                 dim_filters = standardFilter,
                                 anti_sample = TRUE
        )
      }
      
      
    } else {
      
      if(conversionType == "Goal") {
        
        if (goal$type == "EVENT") {
          
          conditions <- event_conditions_from_goal(goal)
          
          if (length(conditions$dimensions) == 1) {
            category <- dim_filter(dimension=conditions$dimensions[[1]],operator=conditions$operators[[1]],expressions=conditions$expressions[[1]])
            
            conversionFilter <- filter_clause_ga4(list(category), operator = "AND")
          } else if (length(conditions$dimensions) == 2) {
            category <- dim_filter(dimension=conditions$dimensions[[1]],operator=conditions$operators[[1]],expressions=conditions$expressions[[1]])
            action <- dim_filter(dimension=conditions$dimensions[[2]],operator=conditions$operators[[2]],expressions=conditions$expressions[[2]])
            
            conversionFilter <- filter_clause_ga4(list(category, action), operator = "AND")
          } else if (length(conditions$dimensions) == 3) {
            category <- dim_filter(dimension=conditions$dimensions[[1]],operator=conditions$operators[[1]],expressions=conditions$expressions[[1]])
            action <- dim_filter(dimension=conditions$dimensions[[2]],operator=conditions$operators[[2]],expressions=conditions$expressions[[2]])
            label <- dim_filter(dimension=conditions$dimensions[[3]],operator=conditions$operators[[3]],expressions=conditions$expressions[[3]])
            
            conversionFilter <- filter_clause_ga4(list(category, action, label), operator = "AND")
          }
          
          conversionData <- google_analytics(viewId = view_id(),
                                             date_range = c(startDate,endDate),
                                             metrics = ga()$metrics,
                                             dimensions = ga()$dimensions,
                                             dim_filters = conversionFilter,
                                             anti_sample = TRUE,
                                             anti_sample_batches = 1
          )
          
          
          data <- google_analytics(viewId = view_id(),
                                   date_range = c(startDate,endDate),
                                   metrics = ga()$metrics,
                                   dimensions = ga()$dimensions[1:3],
                                   anti_sample = TRUE
          )
          
          if (input_dimension() != "pagePath") {
            if (length(conditions$dimensions) == 1) {
              data$pagePath <- NA
              data$eventCategory <- NA
            } else if (length(conditions$dimensions) == 2) {
              data$pagePath <- NA
              data$eventCategory <- NA
              data$eventAction <- NA
            } else if (length(conditions$dimensions) == 3) {
              data$pagePath <- NA
              data$eventCategory <- NA
              data$eventAction <- NA
              data$eventLabel <- NA
            }
            data <- rbind(conversionData, data)
          } else {
            if (length(conditions$dimensions) == 1) {
              data$eventCategory <- NA
            } else if (length(conditions$dimensions) == 2) {
              data$eventCategory <- NA
              data$eventAction <- NA
            } else if (length(conditions$dimensions) == 3) {
              data$eventCategory <- NA
              data$eventAction <- NA
              data$eventLabel <- NA
            }
            data <- rbind(conversionData, data)
          }
          
          
          
          
        } else if (goal$type == "URL_DESTINATION") {
          
          data <- google_analytics(viewId = view_id(),
                                   date_range = c(startDate,endDate),
                                   metrics = ga()$metrics,
                                   dimensions = ga()$dimensions[1:3],
                                   anti_sample = TRUE
          )
          
        }
      } else if (conversionType ==  "Ecommerce") {
        
        
        conversionData <-  google_analytics(viewId = viewId,
                                            date_range = c(startDate,endDate),
                                            metrics = ga()$metrics,
                                            dimensions = ga()$dimensions,
                                            anti_sample = TRUE
        )
        
        nonConversionData <-  google_analytics(viewId = viewId,
                                               date_range = c(startDate,endDate),
                                               metrics = c("sessions"),
                                               dimensions = ga()$dimensions,
                                               anti_sample = TRUE
        )
        
        
        conversionData$sessions <- NA
        
        nonConversionData$transactions = 0
        nonConversionData$transactionRevenue = 0
        dimension <- input_dimension()
        data <- nonConversionData %>% 
          filter_at(vars(dimension), all_vars(. %ni% unique(conversionData[[dimension]]))) %>%
          rbind(conversionData)
        
      }
    }
    
    data$dateHourMinute <- as.numeric(data$dateHourMinute)
    
    return(data)
  })
  ga_path_data <- reactive({
    conversionType <- input$markov.conversion.type
    data <- ga_data()
    
    if (conversionType == "Goal") {
      goal <- input_goal()
      
      if (goal$type == "EVENT") {
        if (input_dimension() %like% "page|content") {
          inputDimensionSql <- paste("
            CASE
              WHEN", input_dimension(), " = '(not set)' THEN pagePath
              ELSE", input_dimension(), " 
            END as ",input_dimension(),"
          ")
        } else {
          inputDimensionSql <- input_dimension()
        }
        sql <- paste("
          SELECT
            clientId,
            GROUP_CONCAT(",input_dimension(),",  ' > ') as ",input_dimension(),", 
            SUM(conversion) as conversion,
            CASE 
              WHEN conversion IS 0 THEN 1
              ELSE 0
            END as no_of_nulls
            
          FROM( 
            SELECT
              clientId,
              dateHourMinute,
              ",inputDimensionSql,",
              SUM(conversion) as conversion
            FROM(
              SELECT
                *,
                CASE 
                   WHEN eventCategory IS NOT NULL THEN totalEvents
                  ELSE 0
                END as conversion
              FROM data
              ORDER BY clientId DESC, dateHourMinute ASC
            )
            GROUP BY clientId, dateHourMinute, ",input_dimension(),"
          )
          GROUP BY clientId 
      ")
        
        data <- sqldf(sql)
        #data[is.na(data)] <- 0
        #return(data)
        
        data <- data %>% 
          group_by_at(input_dimension()) %>% 
          summarise_at(c("conversion", "no_of_nulls"), sum) 
        
        return(data)
        
      } else if (goal$type == "URL_DESTINATION") {
        
        goalDestination <- goal$urlDestinationDetails.url
        
        data <- data %>%
          mutate(
            conversion = ifelse(pagePath %in% goalDestination, uniquePageviews, 0)
          )
        
        sql <- paste("
          SELECT
            ", input_dimension() ,",
            SUM(", last(ga()$metrics),") as ", last(ga()$metrics),",
            SUM(conversion) as conversion,
            SUM(no_of_nulls) as no_of_nulls
          FROM(
            SELECT
              clientId,
              GROUP_CONCAT(",input_dimension(),",  ' > ') as ",input_dimension(),", 
              SUM(",last(ga()$metrics),") as ", last(ga()$metrics),",
              SUM(conversion) as conversion,
              CASE 
                WHEN SUM(conversion) = 0 THEN 1
                ELSE 0
              END as no_of_nulls
              
            FROM(
              SELECT
                *
              FROM data
              ORDER BY clientId DESC, dateHourMinute ASC
            )
            GROUP BY clientId
          ) 
          GROUP BY ",input_dimension(),"
        ")
        
        data <- sqldf(sql)
      }
    } else if (conversionType == "Ecommerce") {
      
      sql <- paste("
          SELECT 
            ", input_dimension() ,",
            SUM(conversion) as conversion,
            SUM(revenue) as revenue,
            SUM(no_of_nulls) as no_of_nulls
          FROM ( 
            SELECT 
              clientId,
              GROUP_CONCAT(",input_dimension(),",  ' > ') as ",input_dimension(),",
              SUM(transactions) as conversion,
              SUM(transactionRevenue) as revenue,
              CASE
                WHEN transactions = 0 THEN 1
                ELSE 0
              END as no_of_nulls
              
            FROM( 
              SELECT
                *
              FROM data 
              ORDER BY clientId DESC, dateHourMinute ASC
            ) 
            GROUP BY clientId
          )
          GROUP BY ",input_dimension(),"
        ")
      
      data <- sqldf(sql)
    }
    return(data)
  })
  ga_direct_data <- reactive({
    conversionType <- input$markov.conversion.type
    data <- ga_data()
    if (conversionType == "Goal") {
      goal <- input_goal()
      if (goal$type == "EVENT") {
        if (input_dimension() %like% "page|content") {
          inputDimensionSql <- paste("
            CASE
              WHEN", input_dimension(), " = '(not set)' THEN pagePath
              ELSE", input_dimension(), " 
            END as ",input_dimension(),"
          ")
        } else {
          inputDimensionSql <- input_dimension()
        }
        sql <- paste("
          SELECT
            clientId,
            dateHourMinute,
            ",inputDimensionSql,",
            SUM(conversion) as conversion
          FROM(  
            SELECT 
              *,
              CASE 
                WHEN eventCategory IS NOT NULL THEN totalEvents
                ELSE 0
              END AS conversion
            FROM data
          ) GROUP BY clientId, dateHourMinute, ",input_dimension(),"
        ")
        
        data <- sqldf(sql)
        
        
        data <- data %>% 
          group_by_at(input_dimension()) %>% 
          summarise_at("conversion", sum) 
        
        
      } else if (goal$type == "URL_DESTINATION") {
        
        goalDestination <- goal$urlDestinationDetails.url
        
        
        data <- data %>%
          mutate(
            conversion = ifelse(pagePath %in% goalDestination, uniquePageviews, 0)
          ) %>%
          group_by_at(input_dimension()) %>% 
          summarise_at(c(conversion_metric(),"conversion"), sum) 
        
        
      }
    } else if (conversionType == "Ecommerce") {
      data <- data %>%
        dplyr::rename(
          conversion = transactions,
          revenue = transactionRevenue
        ) %>%
        group_by_at(input_dimension()) %>%
        summarise_at(c("conversion", "revenue"), sum)
    }
    
    return(data)
  })
  
  #### BIGQUERY WORKFLOW  ####
  bq_path_data <- eventReactive(input$markov.runQuery, {
    withBusyIndicatorServer("markov.runQuery", {
      input$pss;
      validate(need(input$markov.dateRange, "Need date range"))
      print("App -- Running Path")
      
      #primarily for bq_sql
      pathLogic <- TRUE #This variable is the only difference between path and lnd.
      dataset <- view_id()
      goal <- input_goal()
      dateRange <- input$markov.dateRange
      lookBackPeriod <- input$markov.lookBackPeriod
      dimension <- input$markov.dimension
      secondaryDimensionLogic <- input$in.markov.secondary.dimension.logic
      secondaryDimension <- input$in.markov.secondary.dimension
      conversionType <- c(input$markov.conversion.type, input$markov.conversion)
      segmentLogic <- input$markov.filter.logical
      
      #for main function 
      report <-input$markov.report
      segmentMap <- segment()
      filterBuiltin <- input$markov.filter.builtin
      filterType <- input$markov.filter.type
      filterDimension <- input$markov.filter.dimension
      fiterOperator <- input$markov.filter.operator
      filterExpression <- input$markov.filter.expression
      audienceType <- input$markov.filter.advanced.type
      audienceName <- input$markov.filter.advanced.audience.name
      
      sql <- app_ready_markov_sql(
        bqFields,
        config,
        dataset,
        goal,
        dateRange,
        lookBackPeriod,
        dimension,
        secondaryDimensionLogic,
        secondaryDimension,
        conversionType,
        pathLogic,
        segmentLogic,
        report,
        segmentMap,
        filterBuiltin,
        filterType,
        filterDimension,
        fiterOperator,
        filterExpression,
        audienceType,
        audienceName
      )
      
      print("App -- running markov_ready")
      print(sql[[1]])
      project <- config$bq$project
      bq_dataset_query(paste(project,".",dataset, sep = ""), sql[[1]])
      print(sql[[2]])
      bq_dataset_query(paste(project,".",dataset, sep = ""), sql[[2]])
      
      print("App -- running markov paths")
      print(sql[[3]])
      return(
        DBI::dbGetQuery(connection, sql[[3]])
      )
    })
  })
  bq_direct_data <- eventReactive(input$markov.runQuery, {
    withBusyIndicatorServer("markov.runQuery", {
      input$pss;
      validate(need(input$markov.dateRange, "Need date range"))
      print("App -- Running direct")
      
      #primarily for bq_sql
      pathLogic <- FALSE #This variable is the only difference between path and lnd.
      dataset <- view_id()
      goal <- input_goal()
      dateRange <- input$markov.dateRange
      lookBackPeriod <- input$markov.lookBackPeriod
      dimension <- input$markov.dimension
      secondaryDimensionLogic <- input$in.markov.secondary.dimension.logic
      secondaryDimension <- input$in.markov.secondary.dimension
      conversionType <- c(input$markov.conversion.type, input$markov.conversion)
      segmentLogic <- input$markov.filter.logical
      
      #for main function 
      report <-input$markov.report
      segmentMap <- segment()
      filterBuiltin <- input$markov.filter.builtin
      filterType <- input$markov.filter.type
      filterDimension <- input$markov.filter.dimension
      fiterOperator <- input$markov.filter.operator
      filterExpression <- input$markov.filter.expression
      audienceType <- input$markov.filter.advanced.type
      audienceName <- input$markov.filter.advanced.audience.name
      
      sql <- app_ready_markov_sql(
        bqFields,
        config,
        dataset,
        goal,
        dateRange,
        lookBackPeriod,
        dimension,
        secondaryDimensionLogic,
        secondaryDimension,
        conversionType,
        pathLogic,
        segmentLogic,
        report,
        segmentMap,
        filterBuiltin,
        filterType,
        filterDimension,
        fiterOperator,
        filterExpression,
        audienceType,
        audienceName
      )
      print(sql)
      return(DBI::dbGetQuery(connection, sql))
      
    })
    #return(query_exec(sql, project = project, max_pages = Inf, use_legacy_sql = FALSE, quiet = TRUE))
  })
  
  ######## FINALISE PATH ANF DIRECT ####
  
  markov_data_paths <- reactive({
    
    if (input$markov.report == "Built-in") {
      if(input$markov.dimension %in% config$ga$dimensions ) {
        data <- ga_path_data()
      } else if (input$markov.dimension %in% config$bq$dimensions) {
        data <- bq_path_data()
      }
    } else {
      if(input$markov.method == "GA Unsampled" ) {
        data <- ga_path_data()
      } else if (input$markov.method == "Bigquery" ) {
        data <- bq_path_data()
      }
    }
    
    if (isTRUE(input$markov.removeNonConversionPaths)) {
      data <- filter(data, conversion != 0)
    } else {
      data <- data
    }
    
    return(data)
  })
  output$markov.markovQuery <- DT::renderDataTable({
    markov_data_paths()
  })
  
  direct_model <- reactive({
    input$pss;
    
    if (input$markov.report == "Built-in") {
      if(input$markov.dimension %in% config$ga$dimensions ) {
        data <- ga_direct_data()
      } else if (input$markov.dimension %in% config$bq$dimensions) {
        data <- bq_direct_data()
      }
    } else {
      if(input$markov.method == "GA Unsampled" ) {
        data <- ga_direct_data()
      } else if (input$markov.method == "Bigquery" ) {
        data <- bq_direct_data()
      }
    }
    return(data)
  })
  output$markov.gaQuery <- DT::renderDataTable({
    #lastNonDirectBQ()
    data <- direct_model()
    return(data)
  })
  output$markov.path.export <- downloadHandler(
    filename = function() {
      paste(input_dimension(), "Path Data.csv", sep = " ")
    }, 
    content = function(file) {
      write.csv(markov_data_paths(), file, row.names = FALSE)
    }
  )
  
  #####  HOLLISTIC/ CORRELATING  PATHS ####
  sankey_paths <- reactive({
    
    dimension <- input_dimension()
    top = input$markov.path.top
    segment <- input$markov.path.segment
    filterPaths <- input$markov.path.filter
    depth = input$markov.path.depth
    
    path <- markov_data_paths()
    
    
    
    if (is.na(filterPaths) || filterPaths == "") {
      NULL
    } else {
      path  <- path[grep(filterPaths, path[[dimension]]), ]    
    }
    
    
    if (segment == "Converters") {
      path <- filter(path, conversion != 0)
      
      path[[dimension]] <- paste(path[[dimension]], " > Conversion", sep="")
      
      path <- path[1:top,]
      
    } else {
      path <- filter(path, conversion == 0)
      
      path[[dimension]] <- paste(path[[dimension]], " > Null", sep="")
      
      path <- path[1:top,]
    }
    
    path$path_count = path$conversion + path$no_of_nulls
    path$path_list <- strsplit(x=path[[dimension]],split=">")
    
    
    
    node_labels=rep(list(list()),depth)
    label_length = list()
    for(i in 1:depth){
      for(j in 1:length(path[[dimension]])){
        if(!is.na(path$path_list[j][[1]][i]))
          node_labels[[i]][j] = path$path_list[j][[1]][i]
      }
      node_labels[[i]] = unique(unlist(node_labels[[i]]))
      node_labels[[i]] = node_labels[[i]][order(node_labels[[i]])]
      label_length[[i]] = length(node_labels[[i]])
    }
    node_labels = unlist(node_labels)
    label_length = unlist(label_length)
    
    #Build a data frame to fill out with each path view
    combos = NULL
    for(i in 1:(depth-1)){
      for(j in (1 + sum(label_length[1:i-1])):(label_length[i] + sum(label_length[1:i-1]))){
        for(k in (1 + label_length[i] + sum(label_length[1:i-1])):(label_length[i+1] + label_length[i] + sum(label_length[1:i-1]))){
          combos = rbind(combos, c(i,j,k,0))
        } 
      }
    }
    combos = as.data.frame(combos)
    names(combos) = c("step","source","target","value")
    
    #Populate the combo table
    for(i in 1:(dim(combos)[1])){
      for(j in 1:(dim(path)[1])){
        combos$value[i] = sum(combos$value[i], ifelse(
          (node_labels[combos$source[i]] == path$path_list[j][[1]][combos$step[i]]) &
            (node_labels[combos$target[i]] == path$path_list[j][[1]][combos$step[i]+1]),
          path$path_count[j],0), na.rm = TRUE)
      }
    }
    
    uniques = unique(c(combos$source,combos$target))
    uniques
    converts = as.data.frame(list("step"=rep(0,length(uniques)), "source"=uniques, "target"=rep(max(uniques)+1,length(uniques)), 
                                  "value"=rep(0,length(uniques))))
    
    combos = rbind(combos,converts)
    
    for(i in 1:(dim(path)[1])){
      stack_depth = min(depth,length(path$path_list[i][[1]]))
      index_val = which(combos$step == 0 & combos$source == (which(node_labels == path$path_list[i][[1]][stack_depth]) + 
                                                               ifelse(stack_depth>1, sum(label_length[1:(stack_depth-1)]),0)))
      combos$value[index_val] = combos$value[index_val] + path$conversion[i]
    }
    
    
    #Populate the conversion node values
    display_node_labels = node_labels
    #for(i in 1:length(label_length)){
    #  for(j in 1:label_length[i]){
    #    display_node_labels[j+ifelse(i==1,0,sum(label_length[1:(i-1)]))] = paste0(i,":",node_labels[j+ifelse(i==1,0,sum(label_length[1:(i-1)]))])
    #  }
    #}
    
    #display_node_labels = c(display_node_labels, "Conversion")
    
    
    #Generate Sankey diagram
    p <- plot_ly(
      type = "sankey",
      orientation = "v",
      
      node = list(
        label = display_node_labels,
        #color = node_colors,
        pad = 10,
        thickness = 30,
        line = list(
          color = "black",
          width = 0
        )
      ),
      
      link = list(
        source = combos$source-1, # convert to zero index
        target = combos$target-1, # convert to zero index
        value = combos$value#, #size of connection
        #color = combos$color #add colors for each link if desired
      )
    ) %>% 
      layout(
        title = "Conversion Flow Diagram",
        font = list(
          size = 10
        )
      )
    
    return(p)
  })
  output$markov.path <- renderPlotly({
    sankey_paths()
  })
  
  scatter_paths <- reactive({
    
    dimension <- input_dimension()
    top = input$markov.path.top
    segment <- input$markov.path.segment
    depth = input$markov.path.depth
    
    path <- markov_data_paths()
    path <- path[1:top,]
    
    if (segment == "Converters") {
      y <- path$conversion
    } else {
      y <- path$no_of_nulls
    }
    
    path$path_count = path$conversion + path$no_of_nulls
    path$conversion_rate = path$conversion / path$path_count
    
    
    
    p = plot_ly(
      path, 
      y=y, 
      x=~path_count,
      color=~conversion_rate, 
      size=~conversion_rate,
      text=path[[dimension]]
    ) %>% layout(
      xaxis = list(type="log", title="Number of Paths"),
      yaxis = list(type="log", title="Number of Conversions")
    ) %>% colorbar(
      title = "Rate"
    )
    
    return(p)
    
  }) 
  output$markov.path.scatter <- renderPlotly({
    scatter_paths()
  })
  
  ##### RAW ATTRIBUTION AND MODEL COMPARISOON #####
  
  raw_attribution <- reactive({
    input$pss;
    dimension <- input_dimension();
    conversionType <- input$markov.conversion.type
    conv <- "conversion";
    null <- "no_of_nulls"
    order <- c(input$markov.order.choose, input$markov.order.estimate)
    
    #print(input$markov.order.estimate[[1]])
    estimate <- order[2][[1]][[1]]
    
    print(as.integer(estimate))
    
    if (conversionType == "Ecommerce") {
      value <- "conversion_value"
      attribution <- markov_attribution(
        path_data = markov_data_paths(),
        non_path_data = direct_model(), 
        input_dimension = dimension, 
        var_conv = conv, 
        var_value = value, 
        var_null = null,
        input_order = order
      )
      names(attribution) <- c(simpleCap(paste(strsplit(dimension, "_")[[1]], collapse = " ")), 
                              simpleCap(paste(strsplit(names(attribution[2]), "_")[[1]], collapse= " ")), 
                              simpleCap(paste(strsplit(names(attribution[3]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[4]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[5]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[6]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[7]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[8]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[9]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[10]), "_")[[1]], collapse= " ")),
                              simpleCap(paste(strsplit(names(attribution[11]), "_")[[1]], collapse= " "))
                              
      )
    } else {
      
      dimension <- input_dimension()
      var_conv <- "conversion"
      var_null <- "no_of_nulls"
      
      pathData <- markov_data_paths() %>% 
        select_at(c(dimension, var_conv, var_null)) %>% 
        group_by_at(c(dimension)) %>% 
        summarise_at(c(var_conv, var_null), sum)
      
      if (pathData[1,][[1]]=="") {
        pathData = pathData[-1,]
      } else {NULL}
      
      if (isFALSE(input$markov.order.estimate)) {
        markov <- markov_model(pathData, var_path = dimension, var_conv = var_conv, var_null = var_null, order = input$markov.order.choose)
      } else {
        order <- choose_order(pathData, var_path = dimension, var_conv = var_conv, var_null = var_null, max_order = 10)
        markov <- markov_model(pathData, var_path = dimension, var_conv = var_conv, var_null = var_null, order = order$suggested_order)
      }
      names(markov) <- c("dimension", "markov_total_conversions")
      
      heuristics <- heuristic_models(pathData, var_path = dimension, var_conv = var_conv)
      names(heuristics)[1] <- "dimension"
      
      attribution <- merge(heuristics, markov, by = "dimension", all=TRUE)
      
      directData <- direct_model()
      names(directData) <- c("dimension", "last_non_direct_conversions")
      
      attribution <- merge(attribution, directData, by = "dimension", all = TRUE)
      
      
      names(attribution) <- c(
        simpleCap(paste(strsplit(dimension, "_")[[1]], collapse = " ")),
        simpleCap(paste(strsplit(names(attribution[2]), "_")[[1]], collapse= " ")),
        simpleCap(paste(strsplit(names(attribution[3]), "_")[[1]], collapse= " ")),
        simpleCap(paste(strsplit(names(attribution[4]), "_")[[1]], collapse= " ")),
        gsub(" Conversions", "",simpleCap(paste(strsplit(names(attribution[5]), "_")[[1]], collapse= " "))),
        gsub(" Conversions", "",simpleCap(paste(strsplit(names(attribution[6]), "_")[[1]], collapse= " ")))
      )  
    }
    
    return(attribution)
    
  })
  
  attribution_totals <- reactive({
    
    input$markov.model1;
    input$markov.model2;
    dimension <- input_dimension();
    dimension <- simpleCap(paste(strsplit(dimension, "_")[[1]], collapse = " "))
    conversionType <- input$markov.conversion.type
    
    if (conversionType == "Ecommerce") {
      
      firstConversions <- paste(input$markov.model1, "Conversions", sep="")
      firstValue <- paste(input$markov.model1, "Value", sep="")
      secondConversions <- paste(input$markov.model2, "Conversions", sep="")
      secondValue <- paste(input$markov.model2, "Value", sep="")
      
      attribution <- select_at(raw_attribution(), c(dimension, firstConversions, firstValue, secondConversions, secondValue))
      attribution <- attribution[order(-attribution[firstValue]),]
      
    } else {
      attribution <- select_at(raw_attribution(), c(dimension, input$markov.model1, input$markov.model2))
    }
    
    attribution <- attribution %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    row.names(attribution) <- "Totals"
    
    return(attribution)
    
  })
  output$markov.attribution.totals <- DT::renderDataTable({
    datatable(attribution_totals(), options = list(
      paging = FALSE,
      searching = FALSE
    ))
  })
  
  attribution <- reactive({
    
    input$markov.model1;
    input$markov.model2;
    dimension <- input_dimension();
    dimension <- simpleCap(paste(strsplit(dimension, "_")[[1]], collapse = " "))
    conversionType <- input$markov.conversion.type
    
    if (conversionType == "Ecommerce") {
      
      firstConversions <- paste(input$markov.model1, "Conversions", sep="")
      firstValue <- paste(input$markov.model1, "Value", sep="")
      secondConversions <- paste(input$markov.model2, "Conversions", sep="")
      secondValue <- paste(input$markov.model2, "Value", sep="")
      
      attribution <- select_at(raw_attribution(), c(dimension, firstConversions, firstValue, secondConversions, secondValue))
      attribution <- attribution[order(-attribution[firstValue]),]
      
      if (input$markov.model1 == input$markov.model2) {
        
        names(attribution) <- c(simpleCap(paste(strsplit(dimension, "_")[[1]], collapse = " ")), 
                                simpleCap(paste(strsplit(names(attribution[2]), "_")[[1]], collapse= " ")), 
                                simpleCap(paste(strsplit(names(attribution[3]), "_")[[1]], collapse= " "))
        )
        #Round transactions
        attribution[2] <- round_df(attribution[2],2)
        #include money sign
        attribution[[3]] <- paste('£',formatC(attribution[[3]], big.mark=',', format="fg"))
      } else {
        
        #Create and format percentage differences 
        attribution$transaction_difference <- ((attribution[4]/attribution[2])-1)
        attribution$transaction_difference <- as.numeric(attribution$transaction_difference[[1]])
        attribution$transaction_difference <- percentage_df(attribution$transaction_difference)
        
        attribution$revenue_difference <- ((attribution[5] / attribution[3])-1)
        attribution$revenue_difference <- as.numeric(attribution$revenue_difference[[1]])
        attribution$revenue_difference <- percentage_df(attribution$revenue_difference)
        
        #Tidy coloumn Names
        names(attribution) <- c(simpleCap(paste(strsplit(dimension, "_")[[1]], collapse = " ")), 
                                simpleCap(paste(strsplit(names(attribution[2]), "_")[[1]], collapse= " ")), 
                                simpleCap(paste(strsplit(names(attribution[3]), "_")[[1]], collapse= " ")),
                                simpleCap(paste(strsplit(names(attribution[4]), "_")[[1]], collapse= " ")),
                                simpleCap(paste(strsplit(names(attribution[5]), "_")[[1]], collapse= " ")),
                                simpleCap(paste(strsplit(names(attribution[6]), "_")[[1]], collapse= " ")),
                                simpleCap(paste(strsplit(names(attribution[7]), "_")[[1]], collapse= " "))
                                
        )
        
        #Round transactions
        attribution[2] <- round_df(attribution[2],2)
        attribution[4] <- round_df(attribution[4],2)
        
        #include money sign
        attribution[[3]] <- paste('£',formatC(attribution[[3]], big.mark=',', format="fg"))
        attribution[[5]] <- paste('£',formatC(attribution[[5]], big.mark=',', format="fg"))
        
      }
      
    } else {
      
      attribution <- select_at(raw_attribution(), c(dimension, input$markov.model1, input$markov.model2))
      
      if (input$markov.model1 == input$markov.model2) {
        
        attribution <- select_at(attribution, c(dimension, input$markov.model1))
        attribution[2] <- round_df(attribution[2],2)
        
      } else {
        
        attribution$percentage_increase <- ((attribution[3]/attribution[2])-1)
        attribution[[4]] <- attribution[[4]] %>% mutate_each(funs(percentage_df))
        names(attribution)[[4]] <- paste("Percentage Increase of", input$markov.model2)
        attribution[2] <- round_df(attribution[2],2)
        attribution[3] <- round_df(attribution[3],2)
        
        
      }
      
      attribution <- attribution[order(-attribution[input$markov.model2]),]      
    }
    
    return(attribution)
  })
  output$markov.attribution <- DT::renderDataTable({
    attribution()
  })
  output$markov.export <- downloadHandler(
    filename = function() {
      paste(input$markov.model1, "Vs", input$markov.model2, "Markov Attribution.csv", sep = " ")
    }, 
    content = function(file) {
      df <- attribution()
      df[length(df)] <- as.character(unlist(df[length(df)]))
      write.csv(df, file, row.names = FALSE)
    }
  )
  output$markov.attributionTest <- DT::renderDataTable({
    raw_attribution()
  })
  
  ##### REMOVAL EFFECT AND PROBABILITY MATRACIES #####
  
  markov <- reactive({
    
    dimension <- input_dimension()
    var_conv <- "conversion"
    var_null <- "no_of_nulls"
    
    sqlData <- markov_data_paths() %>% 
      select_at(c(dimension, var_conv, var_null)) %>% 
      group_by_at(c(dimension)) %>% 
      summarise_at(c(var_conv, var_null), sum)
    
    if (sqlData[1,][[1]]=="") {
      sqlData = sqlData[-1,]
    } else {NULL}
    
    if (isFALSE(input$markov.order.estimate)) {
      markov <- markov_model(sqlData, var_path = dimension, var_conv = var_conv, var_null = var_null, order = input$markov.order.choose, out_more = TRUE)
    } else {
      order <- choose_order(sqlData, var_path = dimension, var_conv = var_conv, var_null = var_null, max_order = 10)
      markov <- markov_model(sqlData, var_path = dimension, var_conv = var_conv, var_null = var_null, order = order$suggested_order, out_more = TRUE)
    }
    
    return(markov)
    
  })
  output$markov.modelInfoVis <- renderVisNetwork({
    # minimal example
    
    trans_matrix_prob <- markov()$transition_matrix %>%
      dmap_at(c(1, 2), as.character)
    
    edges <-
      data.frame(
        from = trans_matrix_prob$channel_from,
        to = trans_matrix_prob$channel_to,
        label = round(trans_matrix_prob$transition_probability, 2),
        font.size = trans_matrix_prob$transition_probability * 100,
        width = trans_matrix_prob$transition_probability,# * 15,
        #shadow = TRUE,
        arrows = "to",
        color = list(color = "#95cbee", highlight = "red")
      )
    
    nodes <- data.frame(id = c( c(trans_matrix_prob$channel_from), c(trans_matrix_prob$channel_to) )) %>%
      distinct(id) %>%
      arrange(id) %>%
      mutate(
        label = id,
        color = ifelse(
          label %in% c('(start)', '(conversion)'),
          '#4ab04a',
          ifelse(label == '(null)', '#ce472e', '#ffd73e')
        ),
        shadow = TRUE,
        shape = "box"
      )
    
    
    visNetwork(nodes,
               edges,
               height = "2000000px",
               width = "150%",
               main = "Probabilistic model's Transition Matrix") %>%
      visIgraphLayout(layout="layout_in_circle") %>%
      #visLayout(hierarchical = TRUE) %>%
      visNodes(size = 10) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T), 
                 nodesIdSelection = T)
    
    
  })
  output$markov.modelInfoRaw <- DT::renderDataTable({
    
    transition <- reactive({
      
      dimension <- input_dimension()
      
      transition <- markov()[[2]]
      transition[[3]] <- percent(transition[[3]])
      
      
      names(transition) <- c(paste(input_dimension(), " From"), paste(input_dimension(), " To"), "Transition Probability")
      return(transition)
    })
    
    
    return(transition())
  })
  
  removal <- reactive({
    removal <- markov()[[3]]
    return(removal)
  })
  output$markov.rawRemoval <- DT::renderDataTable({
    removal <- removal()
    removal <- removal %>% ungroup() %>% arrange(desc(removal_effects))
    removal[[2]] <- percent(removal[[2]])
    colnames(removal) <- c(input$markov.dimension, "Removal Effect")
    #removal <- removal %>% ungroup() %>% arrange_at(desc("Removal Effect"))
    return(removal)
  })
  
  
  #output$markov.test <- DT::renderDataTable({
  #markov_data_paths()
  #raw_attribution()
  #markov()
  #ga_data()
  #  markov()$transition_matrix
  #}) 
  
  #                                     End Markov Attribution
  ##======================================================================================================##
  
  
  
  
}

shinyApp(ui, server)


