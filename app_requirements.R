library(googleAuthR)
library(googleAnalyticsR)
library(bigrquery) #Use bigquery 
#library(bigQueryR)
library(DBI)
library(lubridate)


app_requirements <- function() {
  #Here is an example of default channel Grouping Definition using an SQL CASE statement. You can Use this as a base to replicate the client's custom channel.
  default_channel_grouping <- '
  CASE 
      WHEN 
        trafficSource.source = "(direct)" 
      	AND 
      	(trafficSource.medium = "(not set)" OR trafficSource.medium = "(none)") 
      	THEN "Direct"
      WHEN 
      	trafficSource.medium = "organic"
      	THEN "Organic Search"
      WHEN (
          CASE
  		      WHEN
  			      COUNTIF(hits.social.hasSocialSourceReferral = "Yes") > 0
  		    THEN "Yes"
  		    ELSE "No"
  	     END
  	    ) = "Yes"
      	OR
      	REGEXP_CONTAINS(trafficSource.medium, r"^(social|social-network|social-media|sm|social network|social media)$")
      	THEN "Social"
      WHEN
      	trafficSource.medium = "email"
      	THEN "Email"
      WHEN trafficSource.medium = "affiliate" THEN "Affiliates"
      WHEN
      	trafficSource.medium = "referral"
      	THEN "Referral"
      WHEN
      	REGEXP_CONTAINS(trafficSource.medium, r"^(cpc|ppc|paidsearch)$")
      	AND    	
      	trafficSource.adwordsClickInfo.adNetworkType <> "Content"
      	THEN "Paid Search"
      WHEN
      	REGEXP_CONTAINS(trafficSource.medium, r" ^(cpv|cpa|cpp|content-text)$")
      	THEN "Other Advertising"
      WHEN
      	REGEXP_CONTAINS(trafficSource.medium, r"^(display|cpm|banner)$")
      	OR    	
      	trafficSource.adwordsClickInfo.adNetworkType = "Content"
      	THEN "Display"
      ELSE "(Other)"
    END
  '
  default_channel_grouping_group_by <- c("trafficSource.source, trafficSource.medium, hits.social.hasSocialSourceReferral, trafficSource.adwordsClickInfo.adNetworkType")
  #The App will look for these exact names. To choose less of these then them from the list 
  builtin_dimension_list <- c(
    "Default Channel Grouping",
    "Custom Channel Grouping",
    "Campaign",
    "Keyword",
    "CM Advertiser (GA Model)",
    "CM Campaign (GA Model)",
    "CM Placement (GA Model)",
    "CM Creative (GA Model)",
    "Page Path",
    "Page Path Level 1",
    "Page Path Level 2",
    "Page Path Level 3",
    "Page Path Level 4",
    "Custom Dimension Name",
    "Content Grouping Name"
    #Similarly to Cusotm Dimensions and content grouping defns -- XX represents the digits you want to use 
    #"Content Group XX",
    #"Dimension XX"
  )
  content_group_and_dimension_name_map <- 
    data.frame(
      content_group_index = c(
        "XX"
      ),
      content_group_name = c(
        "Conent Group Name"
      ),
      dimension_index = c(
        "XX"
      ),
      dimension_name = c(
        "Custom Dimension Name"
      )
    )
  return(
    list(
      default_channel_grouping = list(
        description = "This is an example of default channel grouping in SQL. You can use a text editor to remove \n, \t, \ and string literal -- use this SQL as a base for creating custom channnel groupings. IMPORTANT REGEXP NOTE: In GA channel def 'contains' means a non-case sensative regexp_contains(). This can be done as REGEXP_CONTAINS(column, r'(?i)expression'). So (?i) at the start of the regular expression will replicate contains.",
        example = default_channel_grouping
      ),
      default_channel_grouping_group_by = list(
        description = "This is a one element string literal of a GROUP BY statement you would use to Group the default channel grouping",
        example = default_channel_grouping_group_by
      ),
      builtin_dimension_list = list(
        description = "The App will look for these exact names with the exception of custom dimensions and content grouping -- you must define these in the format content_group_and_dimension_name_map. To choose less of these then remove them from the list", 
        example = builtin_dimension_list
      ),
      content_group_and_dimension_name_map = list(
        description = "Structure any custom dimensions or content grouping like in example. Then you can use the name table to add into builtin_dimension_list.",
        example = content_group_and_dimension_name_map
      ),
      empty_config = list(
        description = "This is an empty version of the configuration. Be careful not to remove elements from the configuration as it may break the app. Keep the NULL values and change NULL values to the clients requirements",
        example = NULL
      ),
      scheduled_query = list(
        description = "Finally we need to create a scheduled query in bigquery, which will unnest the GA table on a daily basis. If you run the bq_ga_unnest(config) function, which contains the configuration list then it will Create a table called ga_unnest for you in BQ. Then use the printed SQL, which changes on the situation depending on the config, to create a scheduled query. The reason we do this is because of bigquery's computational limitations -- It may not be possible to ORDER BY every single cookie ID and time over a large priod of time.",
        example = "bq_ga_unnest(config)"
      )
    )
  )
}


bq_and_ga_auth <- function(config) {
  
  #Set service account
  token_path_ga <- file.path(config$general$authentication$service_account)
  
  ##### Authenticate with GA API
  set_client_token <- file.path(config$general$authentication$client_token)
  googleAuthR::gar_set_client(json = set_client_token,
                              scopes = c("https://www.googleapis.com/auth/analytics.readonly",
                                         "https://www.googleapis.com/auth/analytics.edit"
                              )
  )
  gar_auth_service(json_file = token_path_ga)
  
  ###### Authentication BQ API
  
  token_path <- file.path(config$general$authentication$service_account)
  bq_auth(path = token_path)
  connection <- DBI::dbConnect(bigquery(), project = config$bq$project, )
  return(connection)
  
}



