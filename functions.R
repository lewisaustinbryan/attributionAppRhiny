source("setup.R")
##### Modelling ############

#auto_markov_model(Data, 
#                  var_path, var_conv, var_null,var_value=NULL, 
#                  max_order=10, roc_npt=100, plot=FALSE, nsim_start=1e5, max_step=NULL, out_more=FALSE, sep=">",
#                  ncore=Inf, nfold=10, seed=0, conv_par=0.05, rate_step_sim=1.5, verbose=TRUE)
markov_attribution <- function(path_data, non_path_data, input_dimension, var_conv, var_value = NULL, var_null, input_order = NULL) {
  #input_order must include the numeric input and the action buttion retrospectively.
  if(!is.null(input_order)) {
    if (is.null(var_value)) {
      path_data <- select(path_data, input_dimension, var_conv, var_null)
      names(path_data)[1] <- "dimension"
      path_data <- path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      heuristics <- heuristic_models(path_data, var_path = "dimension", var_conv = var_conv)
      names(heuristics)[1] <- "dimension"
      
      if (input_order[2][[1]] == 0) {
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, order = input_order[1])
      } else {
        order <- choose_order(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, max_order = 10)
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, order = order$suggested_order)
      }
      
      colnames(markov) <- c("dimension", "markov_total_conversions")
      
      attribution <- merge(heuristics,markov, by='dimension', all=TRUE)
      
      non_path_data <- select(non_path_data, input_dimension, var_conv)
      names(non_path_data)<- c("dimension", "last_non_direct_conversions")
      non_path_data <- non_path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      attribution <- inner_join(attribution, non_path_data, by="dimension", all=TRUE)
      
    } else {
      path_data <- select(path_data, input_dimension, var_conv, var_value, var_null)
      names(path_data)[1] <- "dimension"
      path_data <- path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      heuristics <- heuristic_models(Data = path_data, var_path = "dimension", var_conv = var_conv, var_value = var_value)
      names(heuristics)[1] <- "dimension"
      
      if (input_order[2][[1]] == 0) {
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv, var_value = var_value, var_null = var_null, order = input_order[1])
      } else {
        order <- choose_order(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, max_order = 10)
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv,var_value = var_value, var_null = var_null, order = order$suggested_order)
      }
      #if (apply(non_path_data["conversion_value"],2,sum)[[1]] == 0) {
      #  colnames(markov) <- c("dimension", "markov_total_conversions")
      #} else {
      colnames(markov) <- c("dimension", "markov_total_conversions", "markov_total_value")  
      #}
      
      attribution <- merge(heuristics,markov, by='dimension', all=TRUE)
      
      non_path_data <- select(non_path_data, input_dimension, var_conv, var_value)
      names(non_path_data)<- c("dimension", "last_non_direct_conversions", "last_non_direct_value")
      non_path_data <- non_path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      attribution <- inner_join(attribution, non_path_data, by="dimension", all=TRUE)
    }
  } else {
    if (is.null(var_value)) {
      path_data <- select(path_data, input_dimension, var_conv, var_null)
      names(path_data)[1] <- "dimension"
      path_data <- path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      heuristics <- heuristic_models(path_data, var_path = "dimension", var_conv = var_conv)
      names(heuristics)[1] <- "dimension"
      
      if (apply(path["no_of_nulls"], 2, sum)[[1]] == 0 ) {
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, order = 1)
      } else {
        order <- choose_order(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, max_order = 10)
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, order = order$suggested_order)
      }
      
      colnames(markov) <- c("dimension", "markov_total_conversions")
      
      attribution <- merge(heuristics,markov, by='dimension', all=TRUE)
      
      non_path_data <- select(non_path_data, input_dimension, var_conv)
      names(non_path_data)<- c("dimension", "last_non_direct_conversions")
      non_path_data <- non_path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      attribution <- inner_join(attribution, non_path_data, by="dimension", all=TRUE)
      
    } else {
      path_data <- select(path_data, input_dimension, var_conv, var_value, var_null)
      names(path_data)[1] <- "dimension"
      path_data <- path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      heuristics <- heuristic_models(Data = path_data, var_path = "dimension", var_conv = var_conv, var_value = var_value)
      names(heuristics)[1] <- "dimension"
      
      if (apply(path_data["no_of_nulls"], 2, sum)[[1]] == 0 ) {
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv, var_value = var_value, var_null = var_null, order = 1)
      } else {
        order <- choose_order(Data = path_data, var_path = "dimension", var_conv = var_conv, var_null = var_null, max_order = 10)
        markov <- markov_model(Data = path_data, var_path = "dimension", var_conv = var_conv,var_value = var_value, var_null = var_null, order = order$suggested_order)
      }
      #if (apply(non_path_data["conversion_value"],2,sum)[[1]] == 0) {
      #  colnames(markov) <- c("dimension", "markov_total_conversions")
      #} else {
      colnames(markov) <- c("dimension", "markov_total_conversions", "markov_total_value")  
      #}
      
      attribution <- merge(heuristics,markov, by='dimension', all=TRUE)
      
      non_path_data <- select(non_path_data, input_dimension, var_conv, var_value)
      names(non_path_data)<- c("dimension", "last_non_direct_conversions", "last_non_direct_value")
      non_path_data <- non_path_data %>% group_by(dimension) %>% summarise_all(sum, na.rm=TRUE)
      
      attribution <- inner_join(attribution, non_path_data, by="dimension", all=TRUE)
    }
  }
  
  
  return(attribution)
  
}

####### BQ ##########


#Converts BQ Dimension Input name into bq sql name via mapping 
#Rememeber -- Must incluse a dynamic UNNEST(array) as array for SQL statements
#TO DO get the actual tidy value
bq_dimension <- function(bqFields, dimension_input) {
  dimension <- bqFields %>% filter(tidy.name == dimension_input)
}

#Makes the UX a bit nicer when buit in report -- when selecting mutiple from a standard filter is available we assume regex between the values, otherwise operation is equal.
segment_operator <- function(segment_operation_input, segment_expression_input, regexp = FALSE) {
  if (is.null(segment_operation_input)) {
    if (length(segment_expression_input) == 1) {
      segment_operation_input <- "EXACT"
      segment_expression_input <- segment_expression_input
    } else {
      segment_operation_input <- "REGEXP"
      if (isFALSE(regexp)) {
        segment_expression_input <- segment_expression_input  
      } else if (isTRUE(regexp)) {
        segment_expression_input <- paste(segment_expression_input, collapse = "|", sep = "")
      } else {
        stop("Must have a logical regexp value")
      }
    }   
  } else {
    segment_operation_input <- segment_operation_input 
    segment_expression_input <- segment_expression_input
  }
  return(list(
    segment_operation = segment_operation_input,
    segment_expression = segment_expression_input
  ))
}

#Uses event_conditions_from_goal()
convert_ga_goal_to_bq_sql <- function(input_goal) {
  
  operator_case <- function(dimension, operator, expression) {
    if (operator == "EXACT") {
      paste(
        dimension, 
        " = ",  
        "'",expression, "' ",
        sep = "")
    } else { #if (conditions$operator == "REGEXP") {
      paste("REGEXP_CONTAINS(",dimension,",'",expression,"')", sep = "")
    }
  }
  
  
  if (input_goal$type == "EVENT") {
    print("Event Based Goal")
    
    conditions <- event_conditions_from_goal(input_goal)
    conditions$dimensions <- paste("hits.eventInfo.", conditions$dimensions, sep = "") 
    
    unique_events <- "COUNT(DISTINCT CONCAT(CAST(fullVisitorId AS STRING), CAST(visitStartTime AS STRING)))"
    
    if (length(conditions$dimension) == 1) {
      if (conditions$operator == "EXACT") {
        when_then <- paste(
          "WHEN ",
          conditions$dimension, 
          " = ", conditions$expression, 
          " THEN ",
          unique_events,  
          sep = ""
        )
      } else { #if (conditions$operator == "REGEXP") {
        when_then <- paste(
          "WHEN REGEXP_CONTAINS(",
          conditions$dimension, ",
          '", conditions$expression, "')
           THEN ",
          unique_events,
          sep = "")
      }
    } else if (length(conditions$dimension) == 2) {
      when_then <- paste(
        "WHEN ", 
        operator_case(conditions$dimension[1], conditions$operator[1], conditions$expression[1]), 
        " AND ",
        operator_case(conditions$dimension[2], conditions$operator[2], conditions$expression[2]),
        " THEN ",
        unique_events,
        sep = ""
      )
    } else if (length(conditions$dimension) == 3) {
      when_then <- paste(
        "WHEN ", 
        operator_case(conditions$dimension[1], conditions$operator[1], conditions$expression[1]), 
        " AND ",
        operator_case(conditions$dimension[2], conditions$operator[2], conditions$expression[2]),
        " AND ",
        operator_case(conditions$dimension[3], conditions$operator[3], conditions$expression[3]),
        " THEN ",
        unique_events,
        sep = ""
      )
    }
    
    case <- paste("CASE", when_then," ELSE 0 END")
    grouping <- conditions$dimension
    return(list(
      inner_goal_sql = case, 
      inner_grouping_sql = grouping,
      outer_goal_sql = NULL, 
      outer_grouping_sql = NULL
    ))
    
    
  } else { #Use
    print("Page based Goal")
    
    urlRegex <- input_goal$urlDestinationDetails.url
    
    outer <-
      paste(
        ' COUNT(DISTINCT(
          IF
            (REGEXP_CONTAINS(Goal_Completion_Location, r"',urlRegex,'"),
            Session,
            NULL)
          ))',
        sep = ""
      )
    inner <- 
      ' hits.page.pagePath as Goal_Completion_Location,
        CONCAT(CAST(fullvisitorid AS string),CAST(visitStartTime AS string)) as Session
      '
    return(list(
      inner_goal_sql = inner, 
      inner_grouping_sql = c("fullvisitorid", "visitStartTime", "hits.page.pagePath"),
      outer_goal_sql = outer, 
      outer_grouping_sql = NULL
    ))
  }
}

do_standard_segment <- function(field_map, standard_segment_dimension_input, standard_segment_logic, standard_segment_value) {
  segmentDimensionInfo <- bq_dimension(bqFields = field_map, dimension_input = standard_segment_dimension_input) 
  segmentDimensionInput <- as.character(segmentDimensionInfo$bq.name)
  if (isTRUE(segmentDimensionInfo$isArray)) {
    array <- as.character(segmentDimensionInfo$bq.array)
    arrayC <- append(arrayC, array) 
  } else {
    arrayC <- arrayC
  }  
  #TODO - THIS LOGIC ONLY IS WORKING ON A DIMENSION STRTDARD AND DOESNT APPLY TO METRICS YET
  if (standard_segment_logic == "EXACT") {
    innerWhereSegment <- paste('
                           AND ', segmentDimensionInput,' = "', standard_segment_value, '"', sep = "")
  } else if (standard_segment_logic == "BEGINS_WITH") {
    innerWhereSegment <- paste('
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "^', standard_segment_value, '")', sep = "")
  } else if (standard_segment_logic == "ENDS_WITH") {
    innerWhereSegment <- paste('
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "', standard_segment_value, '$")', sep = "")
  } else {
    innerWhereSegment <- paste(' 
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "', standard_segment_value, '")', sep = "")
  }
  innerGroupBySegment <- segmentDimensionInput
  
  return(list(
    innerWhereSegment = innerWhereSegment,
    innerGroupBySegment = innerGroupBySegment,
    arrayC = arrayC
  ))
}

do_built_in_segment <- function(builtin_segment_value) {
  innerSegmentField <- NULL
  outerSegmentField <- NULL
  outerWhereSegment <- NULL
  for (segment in 1:length(builtin_segment_value)) {
    if (is.null(outerWhereSegment)) {
      if (builtin_segment_value[segment] == "New Customers") {
        if (conversion_type[1] == "Ecommerce") {
          outerWhereSegment <- paste(" WHERE conversion = 1")
        } else {
          innerSegmentField <- paste(",
              COUNT(DISTINCT hits.transaction.transactionId) AS outerSegmentField"
          )
          outerSegmentField <- "SUM(outerSegmentField) as outerSegmentField, "
          outerWhereSegment <- paste(" WHERE outerSegmentField = 1")
        }  
      } else if (builtin_segment_value[segment] == "Existing Customers") {
        if (conversion_type[1] == "Ecommerce") {
          outerWhereSegment <- paste(" WHERE conversion > 1")
        } else {
          innerSegmentField <- paste(",
              COUNT(DISTINCT hits.transaction.transactionId) AS outerSegmentField"
          )
          outerSegmentField <- "SUM(outerSegmentField) as outerSegmentField, "
          outerWhereSegment <- paste(" WHERE outerSegmentField > 1")
        } 
      } else {
        warning(paste("Built in Segment", builtin_segment_value[segment], "Does not exsit. Running SQL Without this segment"))
      } 
    } else {
      if (builtin_segment_value[segment] == "New Customers") {
        if (conversion_type[1] == "Ecommerce") {
          outerWhereSegment <- append(outerWhereSegment,"
              AND conversion = 1"
          )
        } else {
          innerSegmentField <- innerSegmentField
          outerSegmentField <- outerSegmentField
          outerWhereSegment <- append(outerWhereSegment,"
            AND outerSegment = 1"
          ) 
        }  
      } else if (builtin_segment_value[segment] == "Existing Customers") {
        if (conversion_type[1] == "Ecommerce") {
          outerWhereSegment <- append(
            outerWhereSegment,"
              AND conversion > 1"
          )
        } else {
          innerSegmentField <- innerSegmentField
          outerSegmentField <- outerSegmentField
          print("hello")
          outerWhereSegment <- append(
            outerWhereSegment,"
              AND outerSegmentField > 1")
        }
      } else {
        warning(paste("Built in Segment", builtin_segment_value[segment], "Does not exsit. Running SQL Without this segment"))
      }
    }
  }
  return(list(
    innerSegmentField = innerSegmentField,
    outerSegmentField = outerSegmentField,
    outerWhereSegment = paste(outerWhereSegment, collapse = " ")
  ))   
}

#Creates SQL for Bigquery -- 
# Currently works on fullVisitorId as cookie
# converts any not set dimension values into retrospective channel. 
# Choose a secondary dimension for more granular paths
# Can choose between path data and (last-non-) direct with path=TRUE
bq_sql <- function(
  config,
  dataset_input, 
  date_range_input,
  look_back_period_input,
  dimension_input,  
  secondary_dimension_input = NULL, 
  conversion_type, 
  input_goal, 
  path = TRUE, 
  field_map = NULL,
  standard_segment_dimension_input = NULL,
  standard_segment_logic = NULL,
  standard_segment_value = NULL,
  builtin_segment_value = NULL,
  audience_type = NULL,
  audience_name = NULL
) {
  
  print("Running Function")
  #get Project from setup
  project_config <- config$bq$project 
  
  ##Handle Date Range and look back
  startDate <- date_range_input[1]
  endDate <- date_range_input[2]
  lookbackDate <- startDate - look_back_period_input 
  
  # Handle dimension_case ########
  print("Handle dimension_case")
  if (dimension_input == "Campaign"){
    dimension_case <- '
      CASE
        WHEN campaign = "(not set)" THEN channelGrouping
        ELSE campaign
      END
    '
    lnd_dimension_case <- '
      CASE
        WHEN lndcampaign = "(not set)" THEN lndchannelGrouping
        ELSE lndcampaign
      END
    '
    dimensionGroupBy <- c("channelGrouping", "lndchannelGrouping", "campaign", "lndcampaign")
  } else if (dimension_input == "Keyword") {
    dimension_case <- '  
      CASE 
        WHEN keyword = "(not provided)" THEN channelGrouping
        WHEN keyword = "(not set)" THEN channelGrouping
        ELSE keyword
      END
    '
    lnd_dimension_case <- '  
      CASE 
        WHEN lndkeyword = "(not provided)" THEN lndchannelGrouping
        WHEN lndkeyword = "(not set)" THEN lndchannelGrouping
        ELSE lndkeyword
      END
    '
    dimensionGroupBy <- c("channelGrouping", "keyword", "lndchannelGrouping", "lndkeyword")
    
  } else if (dimension_input == "Default Channel Grouping" || dimension_input == "Channel") {
    
    dimension_case <- "channelGrouping"
    lnd_dimension_case <- "lndchannelGrouping"
    dimensionGroupBy <- c("channelGrouping", "lndchannelGrouping")
    
  } else if (dimension_input == "Custom Channel Grouping") {
    baseDimensions <- c("source", "medium", "campaign", "keyword")
    
    dimension_case <- config$bq$custom_channel_grouping_case
    lnd_dimension_case <- dimension_case %>%
      gsub("source", "lndsource",.) %>%
      gsub("medium", "lndmedium",.) %>%
      gsub("campaign", "lndcampaign",.) %>%
      gsub("keyword", "lndkeyword",.)
    
    dimensionGroupBy <- config$bq$custom_channel_grouping_group_by
    dimensionGroupBy <- c(
      dimensionGroupBy,
      paste("lnd", subset(dimensionGroupBy, dimensionGroupBy %in%  baseDimensions), sep = "")
    )
  } else if (dimension_input %in% as.character(config$ga$content_group_and_dimension_name_map$dimension_name)) {
    # Custom Dimension #TO DO -- MAke this w0rk for Multiple dimensions
    #And make sure that the name is readable for SQL
    index <- as.character(config$ga$content_group_and_dimension_name_map$dimension_index)
    dimension_case <- paste(' (
    SELECT
      value
    FROM
      UNNEST(table.customDimensions)
    WHERE
      index = ',index,'
    GROUP BY
      value 
  )')
    dimensionGroupBy <- dimension_input
  } else {
    print("Default Dimension Case")
    dimension_case <- tolower(dimension_input)
    lnd_dimension_case <- paste("lnd", dimension_case, sep = "")
    dimension_input <- dimension_input
    dimensionGroupBy <- c(dimension_case, lnd_dimension_case)
  }
  
  ########## Handle Segmentation where and grouping and unnest array c #################
  
  print("Handle Segmentation where and grouping and overall unnesting")
  innerWhereSegment <- NULL
  innerGroupBySegment <- NULL
  innerSegmentField <- NULL
  outerSegmentField <- NULL
  outerWhereSegment <- NULL
  arrayC <- "hits"
  
  #print(field_map)
  print(standard_segment_dimension_input)
  print(standard_segment_logic)
  print(standard_segment_value)
  print(builtin_segment_value)
  print(audience_name)
  print(audience_type)
  
  
  #Logic for different types of segments -->
  standard <- !(is.null(standard_segment_dimension_input) && is.null(standard_segment_value) && is.null(field_map))
  builtin <- !is.null(builtin_segment_value) #&& is.null(field_map)
  audience <- !(is.null(audience_type) && is.null(audience_name))
  #all 3 segment types
  standard_builtin_audiece <- standard && builtin && audience
  #2 segment types
  standard_builtin <- standard && builtin && !audience
  standard_audience <- standard && audience && !builtin
  builtin_audience <- builtin && audience && !standard
  #1 segment type
  justStandard <- standard && !builtin && !audience
  justBuiltin <- builtin && !standard & !audience
  justAudience <- audience && !builtin && !standard
  
  
  if (
    (is.null(audience_type) || is.null(audience_name)) &&
    (is.null(standard_segment_dimension_input) || is.null(standard_segment_value)) &&
    is.null(builtin_segment_value)
  ) { #If no Segment
    print("No Segment")
  } else if (standard_builtin_audiece) {
    print("standard_builtin_audiece")
  } else if (standard_builtin) {
    print("standard_builtin")
    #No cross over in variables (currently) so can just combine the lists
    #TODO: just put these together as separate funtions
    
    
    segmentDimensionInfo <- bq_dimension(bqFields = field_map, dimension_input = standard_segment_dimension_input) 
    segmentDimensionInput <- as.character(segmentDimensionInfo$bq.name)
    if (isTRUE(segmentDimensionInfo$isArray)) {
      array <- as.character(segmentDimensionInfo$bq.array)
      arrayC <- append(arrayC, array) 
    } else {
      arrayC <- arrayC
    }  
    #TODO - THIS LOGIC ONLY IS WORKING ON A DIMENSION STRTDARD AND DOESNT APPLY TO METRICS YET
    if (standard_segment_logic == "EXACT") {
      innerWhereSegment <- paste('
                           AND ', segmentDimensionInput,' = "', standard_segment_value, '"', sep = "")
    } else if (standard_segment_logic == "BEGINS_WITH") {
      innerWhereSegment <- paste('
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "^', standard_segment_value, '")', sep = "")
    } else if (standard_segment_logic == "ENDS_WITH") {
      innerWhereSegment <- paste('
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "', standard_segment_value, '$")', sep = "")
    } else {
      innerWhereSegment <- paste(' 
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "', standard_segment_value, '")', sep = "")
    }
    innerGroupBySegment <- segmentDimensionInput
    
    standardList <- list(
      innerWhereSegment = innerWhereSegment,
      innerGroupBySegment = innerGroupBySegment,
      arrayC = arrayC
    )
    
    
    #builtin_segment_value <- "New Customers"
    #conversion_type <- "Goal"
    
    innerSegmentField <- NULL
    outerSegmentField <- NULL
    outerWhereSegment <- NULL
    for (segment in 1:length(builtin_segment_value)) {
      if (is.null(outerWhereSegment)) {
        if (builtin_segment_value[segment] == "New Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- paste(" conversion_lookback = 1")
          } else {
            innerSegmentField <- paste(",
              COUNT(DISTINCT hits.transaction.transactionId) AS outerSegmentField"
            )
            outerSegmentField <- "SUM(outerSegmentField) as outerSegmentField, "
            outerWhereSegment <- paste(" outerSegmentField = 1")
          }  
        } else if (builtin_segment_value[segment] == "Existing Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- paste(" conversion_lookback > 1")
          } else {
            innerSegmentField <- paste(",
              COUNT(DISTINCT hits.transaction.transactionId) AS outerSegmentField"
            )
            outerSegmentField <- "SUM(outerSegmentField) as outerSegmentField, "
            outerWhereSegment <- paste(" outerSegmentField > 1")
          } 
        } else {
          warning(paste("Segment", builtin_segment_value[segment], "Is not built in"))
          next
        } 
      } else {
        if (builtin_segment_value[segment] == "New Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- append(outerWhereSegment,"
              AND conversion_lookback = 1"
            )
          } else {
            innerSegmentField <- innerSegmentField
            outerSegmentField <- outerSegmentField
            outerWhereSegment <- append(outerWhereSegment,"
            AND outerSegment = 1"
            ) 
          }  
        } else if (builtin_segment_value[segment] == "Existing Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- append(
              outerWhereSegment,"
              AND conversion_lookback > 1"
            )
          } else {
            innerSegmentField <- innerSegmentField
            outerSegmentField <- outerSegmentField
            print("hello")
            outerWhereSegment <- append(
              outerWhereSegment,"
              AND outerSegmentField > 1")
          }
        } else {
          warning(paste("Built in Segment", builtin_segment_value[segment], "Does not exsit. Running SQL Without this segment"))
          next
        }
      }
    }
    builtinList <- list(
      innerSegmentField = innerSegmentField,
      outerSegmentField = outerSegmentField,
      outerWhereSegment = paste(outerWhereSegment, collapse = " ")
    )
    
    
    seg <- combine_lists(
      standardList, 
      builtinList
    ) 
    print(seg)
    #standard -->
    innerWhereSegment <- seg$innerWhereSegment
    innerGroupBySegment <- seg$innerGroupBySegment
    arrayC <- seg$arrayC
    #builtin -->
    innerSegmentField <- seg$innerSegmentField
    outerSegmentField <- seg$outerSegmentField
    outerWhereSegment <- seg$outerWhereSegment
    
  } else if (standard_audience) {
    print("standard_audience")
  } else if (builtin_audience) {
    print("builtin_audience")
  } else if (justStandard) { 
    print("Standard Filter logic")
    
    
    segmentDimensionInfo <- bq_dimension(bqFields = field_map, dimension_input = standard_segment_dimension_input) 
    segmentDimensionInput <- as.character(segmentDimensionInfo$bq.name)
    if (isTRUE(segmentDimensionInfo$isArray)) {
      array <- as.character(segmentDimensionInfo$bq.array)
      arrayC <- append(arrayC, array) 
    } else {
      arrayC <- arrayC
    }  
    #TODO - THIS LOGIC ONLY IS WORKING ON A DIMENSION STRTDARD AND DOESNT APPLY TO METRICS YET
    if (standard_segment_logic == "EXACT") {
      innerWhereSegment <- paste('
                           AND ', segmentDimensionInput,' = "', standard_segment_value, '"', sep = "")
    } else if (standard_segment_logic == "BEGINS_WITH") {
      innerWhereSegment <- paste('
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "^', standard_segment_value, '")', sep = "")
    } else if (standard_segment_logic == "ENDS_WITH") {
      innerWhereSegment <- paste('
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "', standard_segment_value, '$")', sep = "")
    } else {
      innerWhereSegment <- paste(' 
                           AND REGEXP_CONTAINS(',segmentDimensionInput,', "', standard_segment_value, '")', sep = "")
    }
    innerGroupBySegment <- segmentDimensionInput
    
    seg <- list(
      innerWhereSegment = innerWhereSegment,
      innerGroupBySegment = innerGroupBySegment,
      arrayC = arrayC
    )
    
    #seg <- do_standard_segment(field_map, standard_segment_dimension_input, standard_segment_logic, standard_segment_value)
    print(seg)
    innerWhereSegment <- seg$innerWhereSegment
    innerGroupBySegment <- seg$innerGroupBySegment
    arrayC <- seg$arrayC
  } else if (justBuiltin){ #If built in filter
    print("built-in logic")
    
    innerSegmentField <- NULL
    outerSegmentField <- NULL
    outerWhereSegment <- NULL
    for (segment in 1:length(builtin_segment_value)) {
      if (is.null(outerWhereSegment)) {
        if (builtin_segment_value[segment] == "New Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- paste(" conversion_lookback = 1")
          } else {
            innerSegmentField <- paste(",
              COUNT(DISTINCT hits.transaction.transactionId) AS outerSegmentField"
            )
            outerSegmentField <- "SUM(outerSegmentField) as outerSegmentField, "
            outerWhereSegment <- paste(" outerSegmentField = 1")
          }  
        } else if (builtin_segment_value[segment] == "Existing Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- paste(" conversion_lookback > 1")
          } else {
            innerSegmentField <- paste(",
              COUNT(DISTINCT hits.transaction.transactionId) AS outerSegmentField"
            )
            outerSegmentField <- "SUM(outerSegmentField) as outerSegmentField, "
            outerWhereSegment <- paste(" outerSegmentField > 1")
          } 
        } else {
          warning(paste("Segment", builtin_segment_value[segment], "Is not built in"))
          next
        } 
      } else {
        if (builtin_segment_value[segment] == "New Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- append(outerWhereSegment,"
              conversion = 1"
            )
          } else {
            innerSegmentField <- innerSegmentField
            outerSegmentField <- outerSegmentField
            outerWhereSegment <- append(outerWhereSegment,"
              outerSegment = 1"
            ) 
          }  
        } else if (builtin_segment_value[segment] == "Existing Customers") {
          if (conversion_type[1] == "Ecommerce") {
            outerWhereSegment <- append(
              outerWhereSegment,"
                conversion_lookback > 1"
            )
          } else {
            innerSegmentField <- innerSegmentField
            outerSegmentField <- outerSegmentField
            print("hello")
            outerWhereSegment <- append(
              outerWhereSegment,"
                outerSegmentField > 1")
          }
        } else {
          warning(paste("Built in Segment", builtin_segment_value[segment], "Does not exsit. Running SQL Without this segment"))
          next
        }
      }
    }
    seg <- list(
      innerSegmentField = innerSegmentField,
      outerSegmentField = outerSegmentField,
      outerWhereSegment = paste(outerWhereSegment, collapse = " ")
    )
    
    #seg <- do_built_in_segment(builtin_segment_value)
    print(seg)
    innerSegmentField <- seg$innerSegmentField
    outerSegmentField <- seg$outerSegmentField
    outerWhereSegment <- seg$outerWhereSegment
  } else { #If Advanced filter
    if (justAudience) { #Audience
      print("Audience-Logic")
      if (audience_type == "Built-in") {
        innerWhereSegment <- audience_segment(audience_name)
      } else {
        print("GA aud")
        #audience_conditions 
        #advanced_segent_audience_name
        #field_map  
        
      }
    } else { #Custom Segment 
      print("Custom Segment (Filter) Currently nothing")
    }
    
  }
  if (!is.null(outerSegmentField)) {
    lookBackSegmentField <- paste(
      '
      CASE 
        WHEN UNIX_DATE(Date) < UNIX_DATE("',startDate,'") THEN NULL
        ELSE SUM(outerSegmentField)
      END as outerSegmentField  
      ',
      sep = ""
    )
  } else {
    NULL
  }
  
  
  # Handle Conversion value,  dimension path grouping, and arrayC ######
  print("Handle Conversion value,  dimension path grouping")
  #TO DO -- Figure out a good way of handling conversion value
  conversionGroupBy <- NULL 
  if (identical(conversion_type, c("Ecommerce", "Revenue"))) {
    print("conversion Ecommerce Revenue")
    #For conversion first layer -->
    innerConversion <- '
    COUNT(DISTINCT hits.transaction.transactionId) AS conversion,
    SUM(hits.transaction.transactionRevenue)/1000000 AS conversion_value'
    #Conversion second layer -->
    outerConversion <- '
    SUM(conversion) as conversion,
    SUM(conversion_value) as conversion_value
    '
    lookbackConversion <- paste(
      '
      CASE 
        WHEN UNIX_DATE(Date) < UNIX_DATE("',startDate,'") THEN 0
        ELSE conversion
      END as conversion_from_start_date,
      CASE 
        WHEN UNIX_DATE(Date) < UNIX_DATE("',startDate,'") THEN 0
        ELSE conversion_value
      END as conversion_value_from_start_date
      ',
      sep = ""
    )
    conversionFromStartDate <- c("conversion_from_start_date", "conversion_value_from_start_date")
    #For calculating no_of_nulls in path data -->
    conversion <- "SUM(conversion)"
  } else if (identical(conversion_type, c("Ecommerce", "Product Revenue"))) {
    print("conversion Ecommerce Product Revenue")
    arrayC <- append(arrayC, "product")
    #For conversion first layer -->
    innerConversion <- '
    COUNT(DISTINCT hits.transaction.transactionId) AS conversion,
    SUM(productRevenue)/1000000 AS conversion_value'
    #Conversion second layer -->
    outerConversion <- '
    SUM(conversion) as conversion,
    SUM(conversion_value) as conversion_value
    '
    lookbackConversion <- paste(
      '
      CASE 
        WHEN UNIX_DATE(Date) < UNIX_DATE("',startDate,'") THEN 0
        ELSE conversion
      END as conversion_from_start_date,
      CASE 
        WHEN UNIX_DATE(Date) < UNIX_DATE("',startDate,'") THEN 0
        ELSE conversion_value
      END as conversion_value_from_start_date
      ',
      sep = "" 
    )
    conversionFromStartDate <- c("conversion_from_start_date", "conversion_value_from_start_date")
    
    #For calculating no_of_nulls in path data -->
    conversion <- "SUM(conversion)"
  } else if (conversion_type[1] != "Ecommerce"){
    if (input_goal$type == "EVENT") {
      print("conversion type is event goal")
      eventSql <- convert_ga_goal_to_bq_sql(input_goal)
      innerConversion <- paste(eventSql$inner_goal_sql, "AS conversion") 
      conversion <- "SUM(conversion)"
      outerConversion <- "SUM(conversion) AS conversion"
      lookbackConversion <- paste(
        '
        CASE 
          WHEN UNIX_DATE(Date) < UNIX_DATE("',startDate,'") THEN NULL
          ELSE conversion
        END as conversion_from_start_date
        ', 
        sep = ""
      )
      conversionFromStartDate <- "conversion_from_start_date"
      
      
      conversionGroupBy <- eventSql$inner_grouping_sql
    } else {  # Goal type is Page
      print("conversion type is url destination goal")
      pageSql <- convert_ga_goal_to_bq_sql(input_goal)
      innerConversion <- pageSql$inner_goal_sql
      conversion <- pageSql$outer_goal_sql
      outerConversion <- paste(pageSql$outer_goal_sql, " AS conversion", sep = "")
      conversionGroupBy <- pageSql$inner_grouping_sql
      lookbackConversion <- paste(
        '
        CASE 
          WHEN UNIX_DATE(Date) < UNIX_DATE(',startDate,') THEN NULL
          ELSE conversion
        END as conversion_from_start_date
        '
      )
      conversionFromStartDate <- "conversion_from_start_date"
    }
    
  }
  sumConversionFromStartDate <- paste("SUM(",conversionFromStartDate,") as ",conversionFromStartDate, sep = "")
  
  
  
  ### Inner Table and overall nesting#####
  #arrayC <- c("hits", "product")
  arrayC <- unique(arrayC)
  unnest <- NULL
  for (array in 1:length(arrayC)){
    if (is.null(unnest)) {
      unnest <- paste("UNNEST(",arrayC[array],") as",arrayC[array])  
    } else {        
      unnest <- paste(unnest, ", UNNEST(",arrayC[array],") as",arrayC[array])
    }
  }
  
  from_table_where_date <- paste(' FROM `',project_config,'.',dataset_input,'.ga_sessions_*`AS table, 
                                 ',     unnest,'
                                  WHERE 
                                    PARSE_DATE("%Y%m%d", date) BETWEEN "', lookbackDate ,'" AND "',endDate,'"
                                    AND totals.visits = 1',
                                 sep = "")
  
  
  #Make Dimension Input readbale for sql
  dimension_input <- gsub(" ", "_", dimension_input)
  
  #Concat entire Groupings By Statement and check for duplication ###########
  innerGroupBy <- paste(unique(c(dimensionGroupBy, conversionGroupBy, innerGroupBySegment)), collapse = ", ")
  print(paste("INNER GROUP BY:", innerGroupBy))
  if(!is.null(innerGroupBy)) { innerGroupBy <- paste(", ", innerGroupBy, sep = "") } else { innerGroupBy <- innerGroupBy }
  midGroupBy <- 
    outerGroupBy <- dimension_input
  #Handle BQ SQL ############
  if (is.null(secondary_dimension_input)) {
    if (isTRUE(path)) {
      
      conversionFromStartDate <- paste(conversionFromStartDate, collapse = ", ")
      
      ## PULLS DATA FROM GA_SESSIONS TABLE TO DETERMINE MCF ACQUISITION
      markov_ready_unnest <- paste('
  CREATE OR REPLACE TABLE
  `',project_config,'.',dataset_input,'.markov_ready_unnest`
  PARTITION BY Date
  AS
    SELECT
      clientId,
      visitNumber,
      Date,
    ',dimension_case,' as ',dimension_input,',
    ',lnd_dimension_case,' as lnd',dimension_input,',
    ',outerConversion,',
    ',outerSegmentField,'
    FROM (
      SELECT  
        clientId,
        visitNumber,
        PARSE_DATE("%Y%m%d", date) as Date,
        
        trafficSource.source AS lndsource,
        trafficSource.medium AS lndmedium,
        trafficSource.campaign AS lndcampaign,
        trafficSource.keyword AS lndkeyword,
        channelGrouping AS lndchannelGrouping,
        
        IF (trafficSource.isTrueDirect, "(direct)", trafficSource.source) AS source,
        IF (trafficSource.isTrueDirect, "(none)", trafficSource.medium) AS medium,
        IF (trafficSource.isTrueDirect, "(not set)", trafficSource.campaign) AS campaign,
        IF (trafficSource.isTrueDirect, "(not set)", trafficSource.keyword) AS keyword,
        IF (trafficSource.isTrueDirect,"Direct", channelGrouping) AS channelGrouping,
        trafficSource.adwordsClickInfo.adNetworkType AS adNetworkType,
        social.hasSocialSourceReferral AS hasSocialSourceReferral,
      ',innerConversion,
                                   innerSegmentField,'
    ',from_table_where_date,
                                   innerWhereSegment,'
      GROUP BY
        clientId, visitNumber, Date, channelGrouping, source, medium, campaign, keyword, adNetworkType, hasSocialSourceReferral ,
        lndchannelGrouping, lndsource, lndmedium, lndcampaign, lndkeyword
        ',innerGroupBy,'
      )
      GROUP BY
      clientId, visitNumber, Date,
    ',paste(unique(dimensionGroupBy), collapse = ", "),'
  ',
                                   sep = ""
      )  
      
      if (!is.null(outerWhereSegment)) {
        print(outerWhereSegment)
        outerWhereSegment <- paste("AND", outerWhereSegment)
      } 
      print(outerWhereSegment)
      
      markov_ready_array <- paste(
        'CREATE TEMP FUNCTION first_element_of_array(arr ANY TYPE) AS (
          	arr[offset(0)]
          );
          CREATE TEMP FUNCTION last_element_of_array(arr ANY TYPE) AS (
          	arr[ORDINAL(ARRAY_LENGTH(arr))]
          );
          
          CREATE OR REPLACE TABLE
            `',project_config,'.',dataset_input,'.markov_ready_array` 
          AS
          -- GET INDIVIDUAL USER JOURNIES:
          ---- (OPTIONAL) APPLY SEGMENTATION
          ---- REMOVE USERS WHOES:- 
          ------  FINAL END DATE IS LOWER THAN UI START DATE
          ------  (OPTIONAL) FIRST VISITNUMBER IS NOT 1 (TRUE FIRST CLICK) 
          SELECT 
            clientId,
            Date_arr, visitNumber_arr, ',dimension_input,'_arr,
            ARRAY_TO_STRING(',dimension_input,'_arr , " > ") AS funnel,
            CASE 
              WHEN first_element_of_array(visitNumber_arr) = 1 THEN true
              ELSE false
            END AS is_true_first_click,
            conversion_lookback,'
        ,conversionFromStartDate,',	
            CASE
              WHEN conversion_from_start_date = 0 THEN 1 
              ELSE 0
            END AS no_of_nulls
          FROM(
            -- AGGREGATE DATE, VISITNUMBER AND DIMENSIION INTO ARRAY
              SELECT 
                clientId,
                ARRAY_AGG(Date ORDER BY Date) AS Date_arr,
                ARRAY_AGG(visitNumber ORDER BY visitNumber) as visitNumber_arr,
                ARRAY_AGG(',dimension_input,' ORDER BY Date, visitNumber) AS ',dimension_input,'_arr,
                SUM(conversion) as conversion_lookback,'
        ,sumConversionFromStartDate,'
              FROM ( 
              -- SEPARATE CONVERSION INTO LOOKBACK AND START DATE AGGRAGTION 
                SELECT 
                  * ,'
        ,lookbackConversion
        ,'FROM `',project_config,'.',dataset_input,'.markov_ready_unnest` AS table 
              )
              GROUP BY clientId
          )
          WHERE 
            --first_element_of_array(visitNumber_arr) = 1
            --AND 
            UNIX_DATE(last_element_of_array(Date_arr)) >= UNIX_DATE("',startDate,'")
            ',outerWhereSegment,'
  
        ', 
        sep = ""
      )
      
      if (length(conversionFromStartDate) == 2) {
        sumConversionFromStartDate <- paste(
          "SUM(",conversionFromStartDate[1],") as conversion,
           SUM(",conversionFromStartDate[2]," as conversion_value",
          sep = ""
        )
      } else {
        sumConversionFromStartDate <- paste( "SUM(",conversionFromStartDate,") as conversion", sep = "")
      }
      
      markov_paths <- paste(
        '
        CREATE OR REPLACE TABLE
        `',project_config,'.',dataset_input,'.markov_paths` AS
        --REMOVE ARRAYS AND AGGREGATE DIMENSION
        SELECT 
        #is_true_first_click,
        funnel AS ',dimension_input,',
      ',sumConversionFromStartDate,',
        SUM(no_of_nulls) AS no_of_nulls
        FROM  `',project_config,'.',dataset_input,'.markov_ready_array`
        GROUP BY #is_true_first_click, 
          funnel
        ORDER BY conversion DESC
        ',
        sep = ""
      )
      
      return(list(
        markov_ready_unnest,
        markov_ready_array,
        markov_paths
      ))  
      
    } else if (isFALSE(path)) {
      if (!is.null(outerWhereSegment)) {
        outerWhereSegment <- paste("WHERE", outerWhereSegment)
      } 
      
      if (length(conversionFromStartDate) ==2) {
        sumConversionFromStartDate <- paste(
          "SUM(",conversionFromStartDate[1],") as conversion,
           SUM(",conversionFromStartDate[2],") as conversion_value",
          sep = ""
        )
      } else {
        sumConversionFromStartDate <- paste( "SUM(",conversionFromStartDate,") as conversion", sep = "")
      }
      
      lookbackConversion <- str_replace(lookbackConversion, "conversion", "SUM(conversion)")
      lookbackConversion <- str_replace(lookbackConversion, "conversion_value", "SUM(conversion_value)")
      
      
      markov_direct <- paste(
        '
      	CREATE OR REPLACE TABLE 
      		`',project_config,'.',dataset_input,'.markov_direct` AS
      	SELECT 
      		',dimension_input,',
      		',sumConversionFromStartDate,'
      	FROM(	
      		SELECT 
        		lnd',dimension_input,' as ',dimension_input,',
        		',lookbackConversion,'
      		FROM `',project_config,'.',dataset_input,'.markov_ready_unnest` 
      		GROUP BY Date, lnd',dimension_input,'
      	)
      	GROUP BY ',dimension_input,'
      	ORDER BY conversion DESC	
      	'
        ,
        sep = ""
      )
      
      return(markov_direct)
    }
  } else {
    
    #TODO: Make this work on Paths 
    
    if (isTRUE(path)) {
      if (isTRUE(channel_group_by)) {
        sql <- paste('CREATE OR REPLACE TABLE
      `',project_config,'.',dataset_input,'.markov_paths`
      AS
      SELECT
        ',dimension_input, '#,
        #IFNULL(SUM(conversion), 0) as conversions,
        #IFNULL(SUM(Disqualified), 0)  as no_of_nulls
       FROM(
        SELECT
           fullVisitorId,
           STRING_AGG(',dimension_input, ', " > ") as ',dimension_input, '#,
           #conversion,
           #Disqualified
          FROM( 
            SELECT 
              fullVisitorId,
              PARSE_DATE("%Y%m%d", date) as Date,
              visitStartTime,
              ',dimension_case,' as ', dimension_input, '
            FROM `',project_config,'.',dataset_input,'.ga_sessions_*`, UNNEST(hits) as hits
            WHERE 
              PARSE_DATE("%Y%m%d", date) BETWEEN "',startDate,'" AND "',endDate,'"
              AND ',dimension_case,' IS NOT NULL
            GROUP BY
              fullVisitorId, Date, visitStartTime, channelGrouping, ',bq_dimension_name,'
            ORDER BY visitStartTime ASC, fullVisitorId 
          )  
          GROUP BY  
            fullVisitorId
        ) 
        GROUP BY ', dimension_input,'', 
                     sep = "" )
      } else if (isFALSE(channel_group_by)) {
        sql <- paste('CREATE OR REPLACE TABLE
      `',project_config,'.',dataset_input,'.markov_paths`
      AS
      SELECT
        ',dimension_input, '#,
        #IFNULL(SUM(conversion), 0) as conversions,
        #IFNULL(SUM(Disqualified), 0)  as no_of_nulls
       FROM(
        SELECT
           fullVisitorId,
           STRING_AGG(',dimension_input, ', " > ") as ',dimension_input, '#,
           #conversion,
           #Disqualified
          FROM( 
            SELECT 
              fullVisitorId,
              PARSE_DATE("%Y%m%d", date) as Date,
              visitStartTime,
              ',dimension_case,' as ', dimension_input, '
            FROM `',project_config,'.',dataset_input,'.ga_sessions_*`, UNNEST(hits) as hits
            WHERE 
              PARSE_DATE("%Y%m%d", date) BETWEEN "',startDate,'" AND "',endDate,'"
              AND ',dimension_case,' IS NOT NULL
            GROUP BY
              fullVisitorId, Date, visitStartTime, ',bq_dimension_name,'
            ORDER BY visitStartTime ASC, fullVisitorId 
          )  
          GROUP BY  
            fullVisitorId
        ) 
        GROUP BY ', dimension_input,'', 
                     sep = "" )
      }
    } else if (isFALSE(path)) {
      if(isTRUE(channel_group_by)) {
        sql <- paste(
          'SELECT 
              ',dimension_case,' as ', dimension_input, ',
              ',secondary_dimension_input,'
            FROM `',project_config,'.',dataset_input,'.ga_sessions_*`,
              UNNEST(hits) as hits, UNNEST(product) as product
            WHERE 
              PARSE_DATE("%Y%m%d", date) BETWEEN "',startDate,'" AND "',endDate,'"
            AND ',dimension_case,' IS NOT NULL
            GROUP BY
              channelGrouping,',bq_dimension_name,', ', secondary_dimension_input,'', 
          sep = ""
        )
      } else if(isFALSE(channel_group_by)) {
        sql <- paste(
          'SELECT 
              ',dimension_case,' as ', dimension_input, ',
              ',secondary_dimension_input,'
            FROM `',project_config,'.',dataset_input,'.ga_sessions_*`,
              UNNEST(hits) as hits, UNNEST(product) as product
            WHERE 
              PARSE_DATE("%Y%m%d", date) BETWEEN "',startDate,'" AND "',endDate,'"
            AND ',dimension_case,' IS NOT NULL
            GROUP BY
              ',bq_dimension_name,', ', secondary_dimension_input,'', 
          sep = ""
        )  
      }
    }
    
    
    
    
  } #TO DO: MAKE SECONDARY PATH DIMENSION WORK.. maybe.. if it becomes relavent
  
  
}

app_ready_markov_sql <- function(
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
  segment,
  filterBuiltin,
  filterType,
  filterDimension,
  filterOperator,
  filterExpression,
  audienceType,
  audienceName
) {
  
  if (isTRUE(segmentLogic)){
    if (report == "Built-in") {
      print("App Function - Report -- Built-in")
      print(segment)
      segmentType <- segment$type 
    } else {
      segmentType <- filterType  
    }
    print(segmentType)
    #segmentType <- "Standard"
    standard <- "Standard" %in% segmentType
    nStandard <- "Standard" %ni% segmentType
    builtin <- "Built-in" %in% segmentType
    nBuiltin <- "Built-in" %ni% segmentType
    audience <- "Audience" %in% segmentType
    nAudience <- "Audience" %ni% segmentType
    
    #TO DO: Fill in all possibilities
    if (audience && builtin && standard) {
      print("App Function - Segment -- Audience and builtin and standard")
    } else if (audience && builtin && nStandard) {
      print("App Function - Segment -- Audience and built-in")
    } else if (audience && standard && nBuiltin) {
      print("App Function - Segment -- Audience and standard")
    } else if (builtin && standard && nAudience) {
      print("App Function - Segment -- Built-in and standard")
      ##### BUilt-in -->
      if (report == "Built-in") {
        segment <- segment$name 
      } else {
        segment <- filterBuiltin
      }
      # standard -->
      segmentDimension <- filterDimension
      segmentConfig <- segment_operator(filterOperator, filterExpression, regexp = TRUE) 
      segmentExpr <- segmentConfig$segment_expression
      segmentOp <- segmentConfig$segment_operation
      if (isTRUE(secondaryDimensionLogic)) {
        sql <- bq_sql(
          config,
          dataset_input = dataset,
          date_range_input = dateRange,
          look_back_period_input = lookBackPeriod,
          dimension_input = dimension, 
          secondary_dimension_input = secondaryDimension,
          conversion_type = conversionType,
          input_goal = goal,
          path = pathLogic,
          field_map = bqFields,
          standard_segment_dimension_input = segmentDimension,
          standard_segment_logic = segmentOp,
          standard_segment_value = segmentExpr,
          builtin_segment_value = segment
        )
      } else {
        sql <- bq_sql(
          config,
          dataset_input = dataset,
          date_range_input = dateRange,
          look_back_period_input = lookBackPeriod,
          dimension_input = dimension, 
          conversion_type = conversionType,
          input_goal = goal,
          path = pathLogic,
          field_map = bqFields,
          standard_segment_dimension_input = segmentDimension,
          standard_segment_logic = segmentOp,
          standard_segment_value = segmentExpr,
          builtin_segment_value = segment
        )
      }
      
    } else if (audience && nBuiltin && nStandard) {
      print("App Function - Segment -- Audience")
      if (audienceType == "Built-in") {print("App Function - Segment -- Built-in Audience")} else {print("App Function - Segment -- GA Audience")} #useful for debug
      sql <- bq_sql(
        config,
        dataset_input = dataset,
        date_range_input = dateRange,
        look_back_period_input = lookBackPeriod,
        dimension_input = dimension, 
        conversion_type = conversionType,
        input_goal = goal,
        path = pathLogic,
        audience_type = audienceType,
        audience_name = audienceName
      )
      
    } else if (builtin && nAudience &&  nStandard) {
      if (report == "Built-in") {
        segment <- segment$name 
      } else {
        segment <- filterBuiltin
      }
      
      print(paste("App Function - Segment -- Built-in", segment))
      if (isTRUE(secondaryDimensionLogic)) {
        sql <- bq_sql(
          config,
          dataset_input = dataset,
          date_range_input = dateRange,
          look_back_period_input = lookBackPeriod,
          dimension_input = dimension, 
          secondary_dimension_input = secondaryDimension,
          conversion_type = conversionType,
          input_goal = goal,
          path = pathLogic,
          builtin_segment_value = segment
        )
      } else {
        sql <- bq_sql(
          config,
          dataset_input = dataset,
          date_range_input = dateRange,
          look_back_period_input = lookBackPeriod,
          dimension_input = dimension, 
          conversion_type = conversionType,
          input_goal = goal,
          path = pathLogic,
          builtin_segment_value = segment
        )
      }
    } else if (standard && nAudience && nBuiltin) {
      print("App Function - Segment -- Standard")
      segmentDimension <- filterDimension
      segmentConfig <- segment_operator(filterOperator, filterExpression, regexp = TRUE) 
      segmentExpr <- segmentConfig$segment_expression
      segmentOp <- segmentConfig$segment_operation
      if (isTRUE(secondaryDimensionLogic)) {
        sql <- bq_sql(
          config,
          dataset_input = dataset,
          date_range_input = dateRange,
          look_back_period_input = lookBackPeriod,
          dimension_input = dimension,
          secondary_dimension_input = secondaryDimension,
          conversion_type = conversionType,
          input_goal = goal,
          path = pathLogic,
          field_map = bqFields,
          standard_segment_dimension_input = segmentDimension,
          standard_segment_logic = segmentOp,
          standard_segment_value = segmentExpr
        )
      } else {
        sql <- bq_sql(
          config,
          dataset_input = dataset,
          date_range_input = dateRange,
          look_back_period_input = lookBackPeriod,
          dimension_input = dimension, 
          conversion_type = conversionType,
          input_goal = goal,
          path = pathLogic,
          field_map = bqFields,
          standard_segment_dimension_input = segmentDimension,
          standard_segment_logic = segmentOp,
          standard_segment_value = segmentExpr
        )
      } 
    }
  } else {
    print("app - No segmentation")
    if (isTRUE(secondaryDimensionLogic)) {
      sql <- bq_sql(
        config,
        dataset_input = dataset,
        date_range_input = dateRange,
        look_back_period_input = lookBackPeriod,
        dimension_input = dimension, 
        secondary_dimension_input = secondaryDimension,
        conversion_type = conversionType,
        input_goal = goal,
        path = pathLogic
      )
    } else {
      sql <- bq_sql(
        config,
        dataset_input = dataset,
        date_range_input = dateRange,
        look_back_period_input = lookBackPeriod,
        dimension_input = dimension, 
        conversion_type = conversionType,
        input_goal = goal,
        path = pathLogic
      )
    }
  }
}



###### GA #######

property_id <- function(account_id, property_name) {
  property <- ga_webproperty_list(account_id) %>%
    filter(name == property_name)
  return(property$id)
}

get_audience <- function(account_id, property_name, audience_name, type) {
  propertyId <- property_id(account_id, property_name)
  audiences <- ga_remarketing_list(account_id, propertyId)$items
  audience <- audiences %>% 
    filter(name == audience_name)
  audience <- ga_remarketing_get(account_id, propertyId, audience$id)
  if (type == "meta") {
    if (audience$audienceType == "SIMPLE") {
      audience <- data.frame(
        Id = audience$id,
        Name = audience$name,
        Description = audience$description,
        Linked_Ad_Accounts = paste(unique(audience$linkedAdAccounts$type), collapse = ", "),
        Audience_Type = audience$audienceType,
        Membership_Duration_Days = audience$audienceDefinition$includeConditions$membershipDurationDays
      ) 
    } else {
      audience <- data.frame(
        Id = audience$id,
        Name = audience$name,
        Description = audience$description,
        Linked_Ad_Accounts = paste(unique(audience$linkedAdAccounts$type), collapse = ", "),
        Audience_Type = audience$audienceType,
        Membership_Duration_Days = audience$stateBasedAudienceDefinition$includeConditions$membershipDurationDays,
        Days_To_Look_Back = audience$stateBasedAudienceDefinition$includeConditions$daysToLookBack
      )   
    }
  } else if (type == "condition") {
    if (audience$audienceType == "SIMPLE") {
      audience <- list(
        id = audience$id,
        name = audience$name,
        include = audience$audienceDefinition$includeConditions$segment,
        exclude = NA
      )
    } else {
      audience <- list(
        id = audience$id,
        name = audience$name,
        include = audience$stateBasedAudienceDefinition$includeConditions$segment,
        exclude = audience$stateBasedAudienceDefinition$excludeConditions$segment
      )  
    }
  } else {
    audience <- audience 
  }
  return(audience)
}

available_dimensions <- function(inputDimension = NULL) { #Takes available dimensions in the GA API and readies them for a reporting API call
  
  dimensions <- filter(ga_meta(), type == "DIMENSION")
  dimensions <- dimensions$name
  dimensions <- gsub("ga:", "", dimensions)
  dimensions <- gsub("([a-z])([A-Z])", "\\1 \\2", dimensions)
  
  cleanDimensions <- NULL
  for (dimension in 1:length(dimensions)) {
    if (is.null(cleanDimensions)) {
      cleanDimensions <- simpleCap(dimensions[dimension])
    } else {
      cleanDimensions <- append(cleanDimensions, simpleCap(dimensions[dimension]))
      
    }
  }
  
  cleanDimensions <- append(cleanDimensions, c("Content Group 1", "Content Group 2", "Content Group 3"))
  
  
  for (i in 1:200) {
    cleanDimensions <- append(cleanDimensions,  paste("Dimension", i))
  }
  
  
  if (is.null(inputDimension)) {
    return(cleanDimensions)
  } else {
    
    inputDimension <- strsplit(inputDimension, " ")[[1]]
    
    if (length(inputDimension) == 1) {
      apiDimension <- tolower(inputDimension)
    } else if (length(inputDimension) == 2) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], sep = "")   
    } else if (length(inputDimension) == 3) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], sep = "")   
    } else if (length(inputDimension) == 4) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4],   sep = "")   
    } else if (length(inputDimension) == 5) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4], inputDimension[5],    sep = "")   
    } else if (length(inputDimension) == 6) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4], inputDimension[5], inputDimension[6],    sep = "")   
    } else if (length(inputDimension) == 7) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4], inputDimension[5], inputDimension[6], inputDimension[7],   sep = "")   
    } else if (length(inputDimension) == 8) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4], inputDimension[5], inputDimension[6], inputDimension[7], inputDimension[8],   sep = "")   
    } else if (length(inputDimension) == 9) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4], inputDimension[5], inputDimension[6], inputDimension[7], inputDimension[8], inputDimension[9] ,  sep = "")   
    } else if (length(inputDimension) == 10) {
      apiDimension <- paste(tolower(inputDimension[1]),inputDimension[2], inputDimension[3], inputDimension[4], inputDimension[5], inputDimension[6], inputDimension[7], inputDimension[8], inputDimension[9], inputDimension[10],   sep = "")   
    } 
    
    return(apiDimension)
  }
  
  #possibe alternative solution using switch for above - although a one-one mapping makes more sense than above (doesnt matter enough to change) 
}

event_conditions_from_goal <- function(goalInput) {
  # Takes a input_goal() (as in row) from ga_goal_list(.) and gives a list of event hierachy dimensions, operators and expressions 
  #ready to be used for event goal expressions in a ga API pull, for example
  conditions <- goalInput$eventDetails.eventConditions
  conditions <- unlist(strsplit(conditions, split = ","))
  if (length(conditions) == 3) {
    conditions <- list(
      dimensions = conditions[1],
      operators = conditions[2],
      expressions = conditions[3]
    )
    conditions$dimensions <- paste("event", simpleCap(tolower(conditions$dimensions)), sep="")
  } else if (length(conditions) == 6) {
    conditions <- str_extract(conditions, '"[A-z]+"|".*"')
    a <- str_extract(conditions, '[A-z]+')[1:4]
    b <- str_extract(conditions, '[[A-z]+\\s]+')[5:6]
    conditions <- append(a,b)
    conditions <- list(
      dimensions = conditions[1:2],
      operators = conditions[3:4],
      expressions = conditions[5:6]
    )
    conditions$dimensions <- tolower(conditions$dimensions)
    dimensions <- NULL
    for (dimension in 1:length(conditions$dimensions)) {
      if (is.null(dimensions)) {
        dimensions <- simpleCap(conditions$dimensions[dimension])
      } else {
        dimensions <- append(dimensions, simpleCap(conditions$dimensions[dimension]) )
      }
    }
    conditions$dimensions <- dimensions
    conditions$dimensions <- paste("event",conditions$dimensions, sep = "")
  } else if (length(conditions) == 9) {
    conditions <- str_extract(conditions, '"[A-z]+"|".*"')
    a <- str_extract(conditions, '[A-z]+')[1:6]
    b <- str_extract(conditions, '[[A-z]+\\s]+')[7:9]
    conditions <- append(a,b)
    conditions <- list(
      dimensions = conditions[1:3],
      operators = conditions[4:6],
      expressions = conditions[7:9]
    )
    conditions$dimensions <- tolower(conditions$dimensions)
    dimensions <- NULL
    for (dimension in 1:length(conditions$dimensions)) {
      if (is.null(dimensions)) {
        dimensions <- simpleCap(conditions$dimensions[dimension])
      } else {
        dimensions <- append(dimensions, simpleCap(conditions$dimensions[dimension]) )
      }
    }
    conditions$dimensions <- dimensions
    conditions$dimensions <- paste("event",conditions$dimensions, sep = "")
  }
  return(conditions)
}


######### User Interface ###########

withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

##### General ########

simpleCap <- function(string) {
  s <- strsplit(string, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

percentage_df <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, paste0(round(x*100L, 1), "%")) 
  } else x 
}

tidy_column_names <- function(df) {
  names <- names(df)
  names <- 
    names(df) <- 
    gsub('([[:upper:]])|\\.|\\_', ' \\1', names)
  cleanNames <- NULL
  for (name in 1:length(names)) {
    if(is.null(cleanNames)) {
      cleanNames <- simpleCap(names[name])
    } else {
      cleanNames <- append(cleanNames, simpleCap(names[name]))
    }
  }
  names(df) <- cleanNames
}

combine_lists <- function(list1, list2) {
  # Combine lists 'list1' and 'list2', giving precedence to elements found in 'list2':
  # that is, if $something is found in both 'list1' and 'list2',
  # the new (output) list will have the same values as 'list2' in $something
  
  list1.names <- names(list1)
  list2.names <- names(list2)
  
  new.list <- list1
  
  
  tmp <- match(list2.names, list1.names)
  w <- which(!is.na(tmp))
  
  if (length(w) > 0)
  {
    # take values from list2 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[[tmp]] <- list2[[w]]
    
    # append elements of 'list2' with unmatched names
    new.list <- c(new.list, list2[-w])
  }
  else
  {
    new.list <- c(new.list, list2)
  }
  
  new.list
} # end of combine.lists

'%ni%' <- Negate('%in%')
