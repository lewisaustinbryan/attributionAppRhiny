
#Load app_requirements package
source("app_requirements.R")

req <- app_requirements()

#req$default_channel_grouping$description

#SQL is taken as a string in R functions so it may be easier to use a text editor or test in bigquery whilst creating and then copy and paste back here.
custom_channel_grouping_case <- app$

#custom_channel_grouping_case

#The Dynamic SQL statements in the app need to know what dimensions to group by.
#For example, from the default channel grouping we would write:
custom_channel_grouping_group_by <- c("campaign", "medium", "source", "keyword", "adNetworkType")


content_group_and_dimension_name_map <-data.frame(
  content_group_index = c(
    "XX"
  ),
  content_group_name = c(
    "Name of Content Group"
  ),
  dimension_index = c(
    "XX",
  ),
  dimension_name = c(
    "Name of Custom Dimension",
  ),
  stringsAsFactors = FALSE
)


config <- list(
  general = list(
    client_name = "Client Name",
    authentication = list(
      service_account = "service_client_secret.json",
      client_token = "ouath_2_client_secret_id.json"
    ),
    builtin_dimension_list = list(
      "Bigquery" = c(
        "Default Channel Grouping",
        "Campaign"
      )
    ),
    with_secondary_dimension = NULL,
    favourite = list(
      property = NULL,
      dimension = "Default Channel Grouping",
      secondary_dimension = NULL,
      secondary_dimension_on = FALSE, #Indicates the starting logic of whether a secondary dimension is being used or not
      conversion_type = "Ecommmerce", 
      conversion = "Revenue",
      segment = list(
        on = TRUE,
        name = "segment Name",
        bigquery = list(
          all_values = TRUE,
          chosen_values = NULL
        ),
        ga = NULL
      )
      #segment_on = TRUE, ##Indicates the starting logic of whether a segment is being used or not
      #segment = "Product",
    ),
    #Match segment name with the type of filter
    segments = list(
      name = c(
        "Segment Name",
      ),
      type = c(
        "Standard"
      ), 
      #Use bqFileds and choose the tidy.name (s) you need to use
      bigquery = list(
        standard = list(
          names = c(
            "Custom Dimension XX"
          ), 
          with_built_in_expression_value = NULL
        ),    
        advanced = NULL
      ),
      ga = NULL
    )
  ),
  ga = list (
    account = "XXXXXXX",
    #Choose the dimensions from builtin_dimension_list that will be pulled via GA API -->
    dimensions = NULL,
    content_group_and_dimension_name_map = content_group_and_dimension_name_map 
  ),
  bq = list(
    project = "project-id",
    dataSets = "XXXXXXX",
    #Choose the dimensions from builtin_dimension_list that will be pulled via BQ API -->
    dimensions = c(
      "Default Channel Grouping",
      "Campaign",
      as.character(content_group_and_dimension_name_map$dimension_name)
    ),
    content_group_and_dimension_name_map = content_group_and_dimension_name_map,
    custom_channel_grouping_case = NULL,
    custom_channel_grouping_group_by = NULL
  )
)








