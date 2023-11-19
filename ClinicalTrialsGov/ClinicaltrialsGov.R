
library(tidyverse)
library(data.table)

# Functions to parse XML files to R lists, using INDIVIDUAL XML files on the root folder ----------------------

gather_results <- function(parsed){

  check <- tryCatch(parsed[["//clinical_results"]],  error = function(e) {
    return(NULL)
  })
  if(is.null(check)) return(list(
    participant_flow = NULL,
    baseline_data = NULL,
    outcome_data = NULL
  ))

  this_nct_id <- XML::xmlValue(parsed[["//nct_id"]])

  ## participant flow


  gp_look <- get_group_lookup(parsed, "//participant_flow/group_list")
  period <- parsed["//period_list/period"]

  flow_table <- do.call(plyr::rbind.fill, XML::xmlApply(period, function(node){

    cbind(
      title = XML::xmlValue(node[["title"]]),
      do.call(plyr::rbind.fill, XML::xmlApply(node[["milestone_list"]], function(n0){

        cbind(status = XML::xmlValue(n0[["title"]]),
              data.frame(t(XML::xmlSApply(n0[["participants_list"]], XML::xmlAttrs)), stringsAsFactors = FALSE, row.names = 1:length(gp_look)))

      }))
    )

  }))



  flow_table$arm <- gp_look[flow_table$group_id]
  flow_table$nct_id <- this_nct_id


  ## baseline

  gp_look <- get_group_lookup(parsed, "//baseline/group_list")

  # measures <- parsed[["//baseline/measure_list"]]

  # Error catcher to pick up an NCT ID causing error that tracebacks to here:
  # check <- tryCatch(parsed[["//baseline/measure_list"]],  error = function(e) {
  #   print(paste("Error in //baseline/measure_list, NCT ID throwing error is: ", this_nct_id))
  #   return(NULL)
  # })
  # Result: "Error in //baseline/measure_list, NCT ID throwing error is:  NCT01602016"

  # Check to see if baseline measures exist using get_group_lookup function
  # If no, set baseline arm to NA and continue
  # If yes, parse baseline measures

  if (is.null(get_group_lookup(parsed, "//baseline/measure_list"))) {
    baseline_table <- data.frame("arm" = c(NA))
  } else {
    baseline_table <- do.call(plyr::rbind.fill, XML::xmlApply(parsed[["//baseline/measure_list"]], function(node){

      #outer most level: titles and units
      lank <- XML::xmlSApply(node, function(n){
        # category_list -> return sub-titles
        if(XML::xmlName(n) == "category_list"){

          do.call(plyr::rbind.fill, XML::xmlApply(n, function(n0){

            tmpRes <- XML::xmlApply(n0[["measurement_list"]], function(x){

              as.data.frame(t(XML::xmlAttrs(x)), stringsAsFactors = FALSE)

            })
            ResAdd <- do.call(plyr::rbind.fill, tmpRes)
            data.frame(
              cbind(
                subtitle = XML::xmlValue(n0),
                ResAdd,
                stringsAsFactors = FALSE),
              row.names = NULL, stringsAsFactors = FALSE)
          }))

        } else if(XML::xmlName(n) == "class_list"){

          do.call(plyr::rbind.fill, XML::xmlApply(n, function(n0){

            subtitle <- XML::xmlValue(n0[["title"]])
            tmpRes <- XML::xmlApply(n0[["category_list"]][["category"]][["measurement_list"]], function(x){

              as.data.frame(t(XML::xmlAttrs(x)), stringsAsFactors = FALSE)

            })
            ResAdd <- do.call(plyr::rbind.fill, tmpRes)
            data.frame(
              cbind(
                subtitle = subtitle,
                ResAdd,
                stringsAsFactors = FALSE),
              row.names = NULL, stringsAsFactors = FALSE)
          }))

        } else {

          XML::xmlValue(n)

        }
      })

      if (any(names(lank) == "class_list", na.rm = T)) {
        names(lank)[names(lank) == "class_list"] <- "category_list"
        target <- lank$category_list
        fillout <- lank[names(lank) != "category_list"]
        cbind(fillout, target)
      } else {
        data.frame(lank)
      }

    }))

    baseline_table$arm <- gp_look[baseline_table$group_id]

  }

  baseline_table$nct_id <- this_nct_id


  ## outcomes

  #parsed_out <- xml2::xml_find_all(x, ".//outcome")

  all_results_list <- XML::xmlApply(parsed[["//clinical_results/outcome_list"]], function(parsed_out){

    gp_look <- get_group_lookup(parsed_out, "group_list")

    measures <- parsed_out[["measure_list"]]
    analysis <- parsed_out[["analysis_list"]]

    results_titles <- XML::xmlApply(parsed_out, function(node){

      if(XML::xmlName(node) %in% c("group_list", "measure_list", "analysis_list")) return(NULL) else {

        XML::xmlValue(node)

      }

    })

    if(!is.null(measures)) {
      results_table <- do.call(plyr::rbind.fill, XML::xmlApply(measures, function(node){

        #outer most level: titles and units
        lank <- XML::xmlSApply(node, function(n){
          # category_list -> return sub-titles
          if(XML::xmlName(n) == "category_list"){

            do.call(plyr::rbind.fill, XML::xmlApply(n, function(n0){
              data.frame(
                cbind(
                  subtitle = XML::xmlValue(n0),
                  t(XML::xmlSApply(n0[["measurement_list"]], XML::xmlAttrs)),
                  stringsAsFactors = FALSE),
                row.names = NULL, stringsAsFactors = FALSE)
            }))

          } else {

            XML::xmlValue(n)

          }
        })

        if (is.recursive(lank)) {
          target <- lank$category_list
          fillout <- lank[names(lank) != "category_list"]
          cbind(fillout, target)
        } else {
          data.frame(lank)
        }

      }))

      results_table$arm <- gp_look[results_table$group_id]

      measures_table <- cbind(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")],
            results_table)

    } else measures_table <- data.frame(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")])

    if(!is.null(analysis)){

      analysis_table <- do.call(plyr::rbind.fill, XML::xmlApply(analysis, function(node){

        lank <- as.data.frame(XML::xmlApply(node, function(n){

          if(XML::xmlName(n) == "group_id_list"){

            data.frame(group_id = XML::xmlSApply(n, XML::xmlValue), stringsAsFactors = FALSE)

          } else {

            tmp <- data.frame(XML::xmlValue(n), stringsAsFactors = FALSE)
            colnames(tmp) <- XML::xmlName(n)
            tmp

          }
        }), stringsAsFactors = FALSE)

      }))

      analysis_table$arm <- gp_look[analysis_table$group_id]

      analysis_table <- cbind(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")],
            analysis_table)

    } else analysis_table <- data.frame(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")])


    if(is.null(analysis)){
      measures_table
    } else if(is.null(measures)){
      analysis_table
    } else {
      plyr::rbind.fill(measures_table, analysis_table)
    }


  })

  final_outcome_table <- do.call(plyr::rbind.fill, all_results_list)
  final_outcome_table$nct_id <- this_nct_id

  list(
    participant_flow = flow_table,
    baseline_data = baseline_table,
    outcome_data = final_outcome_table
  )

}

## group labels are stored as key: values but only referred to in results as
## keys. This makes a lookup vector.

get_group_lookup <- function(parsed, xpath){

  group_list <- tryCatch(parsed[[xpath]], error = function(e) NULL)
  if(is.null(group_list)) return(NULL)

  group_lookup <- as.data.frame(t(XML::xmlSApply(group_list,
                                                 function(node){
                                                   c(XML::xmlAttrs(node), XML::xmlValue(XML::xmlChildren(node)$title))
                                                 })), stringsAsFactors = FALSE)

  group_look <- group_lookup[,2]
  names(group_look) <- group_lookup$group_id
  group_look

}

## simple xml tables to dataframe

xmltodf <- function(parsed_xml, xpath){

  as.data.frame(do.call(plyr::rbind.fill, lapply(parsed_xml[xpath],
                                                 function(x) as.data.frame(XML::xmlToList(x),
                                                                           stringsAsFactors = FALSE))),
                stringsAsFactors = FALSE)

}






parse_study_xml <- function(file, include_textblocks = FALSE, include_results = FALSE){

  parsed <- XML::xmlParse(file)

  date_disclaimer <- XML::xmlValue(parsed[["//download_date"]])
  ids <- as.data.frame(XML::xmlToList(parsed[["//id_info"]])[c("org_study_id", "nct_id")], stringsAsFactors = FALSE)

  ## basic study info

  infoterms <- c("brief_title", "official_title", "overall_status", "start_date", "completion_date", "lead_sponsor/agency", "overall_official",
                 "phase", "study_type", "study_design", "enrollment", "primary_condition", "primary_outcome", "eligibility", "sponsors")

  study_info <- ids

  for(i in 1:length(infoterms)){

    if(infoterms[i] == "primary_condition"){

      infoterm <- "condition"
      innterm <- parsed[paste0("//", infoterm)]
      if(length(innterm) > 1) {
        study_info[infoterms[i]] <- paste(sapply(innterm, XML::xmlValue), collapse = "; ")
      } else {
        study_info[infoterms[i]] <- XML::xmlValue(innterm[[1]])
      }

    } else {


    infoterm <- infoterms[i]
    tmpField <- tryCatch(lapply(parsed[paste0("//", infoterm)], XML::xmlToList), error = function(e) NA)

    tmpField <- as.data.frame(tmpField, stringsAsFactors = FALSE)
    if(nrow(tmpField) == 0) next
    tmpField[["textblock"]] <- NULL
    if(ncol(tmpField) > 1) colnames(tmpField) <- paste(infoterm, colnames(tmpField), sep = ".") else
        colnames(tmpField) <- infoterm
    study_info <- cbind(study_info, tmpField, stringsAsFactors = FALSE)

    if(infoterm == "completion_date") study_info["completion_date_type"] <- tryCatch(XML::xmlAttrs(parsed[[paste0("//", infoterm)]])["type"], error = function(e) NA)

  }
  }


  study_info$date_disclaimer <- date_disclaimer

  interventions <- xmltodf(parsed, "//intervention")
  if(nrow(interventions) > 0){
    interventions$nct_id <- ids$nct_id
  }

  if(include_textblocks){

  ## big text fields

  textblocks <- xmltodf(parsed, "//textblock")
  if(nrow(textblocks) > 0){
    textblocks$nct_id <- ids$nct_id
  }

  } else textblocks <- NULL
  ## locations

  locations <- xmltodf(parsed, "//location")
  if(nrow(locations) > 0){
    locations$nct_id <- ids$nct_id
    colnames(locations) <- gsub("facility.", "", colnames(locations), fixed = TRUE)
  }

  ## arms

  arms <- xmltodf(parsed, "//arm_group")
  if(nrow(arms) > 0){
    arms$nct_id <- ids$nct_id
  }

  ## outcomes

  outcometerms <- c("primary_outcome", "secondary_outcome", "other_outcome")
  outcomes <- NULL

  for(i in 1:length(outcometerms)){

    outterm <- outcometerms[i]

      tmpField <- tryCatch(plyr::ldply(parsed[paste0("//", outterm)], function(x){

        as.data.frame(XML::xmlToList(x), stringsAsFactors = FALSE)

      }), error = function(e) data.frame(measure = NA))

      if(nrow(tmpField) == 0) next

      tmpField$type <- outterm
      outcomes <- plyr::rbind.fill(outcomes, tmpField)

      }

  if(!is.null(outcomes) && nrow(outcomes) > 0)
    outcomes$nct_id <- ids$nct_id


  list(study_info = study_info,
       locations = locations,
       arms = arms,
       interventions = interventions,
       outcomes = outcomes,
       textblocks = textblocks)

}





# -----------------------------------------------------------

# Process files check  --------------------------------------

# Sample

my_files <- list.files(full.names = TRUE)

temp <- parse_study_xml('./NCT00004340.xml', include_textblocks = TRUE, include_results = TRUE)

any(temp$interventions$intervention_type %in% "Drug")

paste(unique(temp$interventions$intervention_type), collapse = " + ")







my_files <- list.files(full.names = TRUE)

length(my_files)



# -----
# Study IDs --------------------------------

list_study_ids <- list()



extract_ids_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  try({list_study_ids <- append(list_study_ids, temp$study_info$nct_id)})
  
}


start <- Sys.time()

list_study_ids <- lapply(my_files[], extract_ids_function)

end <- Sys.time()

print(end - start)

list_study_ids <- data.frame(unlist(list_study_ids))





# Interventions Type -------------------------

list_interventions <- list()




extract_interventions_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (any(temp$interventions$intervention_type %in% "Drug")) {
    
    try({list_interventions <- append(list_interventions, 
                                      paste(unique(temp$interventions$intervention_type), collapse = " + "))})
    
  }else{
    
    try({list_interventions <- append(list_interventions, "none")})
    
  }
}




start <- Sys.time()

list_interventions <- lapply(my_files[], extract_interventions_function)

end <- Sys.time()

print(end - start)

list_interventions <- data.frame(unlist(list_interventions))






# Interventions  Name  -------------------------

list_interventions_name <- list()


extract_interventions_name_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$interventions$intervention_name) > 0) {
    
    try({list_interventions_name <- append(list_interventions_name, 
                                      paste(unique(temp$interventions$intervention_name), collapse = " + "))})
    
  }else{
    
    try({list_interventions_name <- append(list_interventions_name, "none")})
    
  }
}




start <- Sys.time()

list_interventions_name <- lapply(my_files[], extract_interventions_name_function)

end <- Sys.time()

print(end - start)

list_interventions_name <- data.frame(unlist(list_interventions_name))









merged <- list_study_ids %>% bind_cols(list_interventions)

merged <- merged %>% bind_cols(list_interventions_name)

names(merged)

unique(merged$unlist.list_interventions.)

merged %>% filter(grepl("Drug", unlist.list_interventions.)) %>%
  select(unlist.list_interventions_name.) %>% distinct()

merged %>% filter(grepl("Biological", unlist.list_interventions.)) %>%
  select(unlist.list_interventions_name.) %>% distinct()


fwrite(merged, "temp_merged_trial.txt", sep="\t")
merged <- fread("temp_merged_trial.txt", sep="\t")




# Country   -------------------------

list_country <- list()

temp$locations$address.country

extract_country_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$locations$address.country) > 0) {
    
    try({list_country <- append(list_country, 
                                      paste(unique(temp$locations$address.country), collapse = " + "))})
    
  }else{
    
    try({list_country <- append(list_country, "none")})
    
  }
}




start <- Sys.time()

list_country <- lapply(my_files[], extract_country_function)

end <- Sys.time()

print(end - start)

list_country <- data.frame(unlist(list_country))

merged <- merged %>% bind_cols(list_country)

unique(merged$unlist.list_country.)


# Phase   -------------------------

list_phase <- list()

temp$study_info$phase

extract_phase_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$study_info$phase) > 0) {
    
    try({list_phase <- append(list_phase, 
                                      paste(unique(temp$study_info$phase), collapse = " + "))})
    
  }else{
    
    try({list_phase <- append(list_phase, "none")})
    
  }
}




start <- Sys.time()

list_phase <- lapply(my_files[], extract_phase_function)

end <- Sys.time()

print(end - start)

list_phase <- data.frame(unlist(list_phase))

merged <- merged %>% bind_cols(list_phase)

unique(merged$unlist.list_phase.)

# Status   -------------------------

list_status <- list()

temp$study_info$overall_status

extract_status_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$study_info$overall_status) > 0) {
    
    try({list_status <- append(list_status, 
                                      paste(unique(temp$study_info$overall_status), collapse = " + "))})
    
  }else{
    
    try({list_status <- append(list_status, "none")})
    
  }
}




start <- Sys.time()

list_status <- lapply(my_files[], extract_status_function)

end <- Sys.time()

print(end - start)

list_status <- data.frame(unlist(list_status))

merged <- merged %>% bind_cols(list_status)

unique(merged$unlist.list_status.)

# Sponsor   -------------------------

list_Sponsor <- list()

temp$study_info$`lead_sponsor/agency`

extract_sponsor_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$study_info$`lead_sponsor/agency`) > 0) {
    
    try({list_Sponsor <- append(list_Sponsor, 
                                      paste(unique(temp$study_info$`lead_sponsor/agency`), collapse = " + "))})
    
  }else{
    
    try({list_Sponsor <- append(list_Sponsor, "none")})
    
  }
}




start <- Sys.time()

list_Sponsor <- lapply(my_files[], extract_sponsor_function)

end <- Sys.time()

print(end - start)

list_Sponsor <- data.frame(unlist(list_Sponsor))

merged <- merged %>% bind_cols(list_Sponsor)

unique(merged$unlist.list_Sponsor.)

# Condition   -------------------------

list_condition <- list()

temp$study_info$primary_condition

extract_condition_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$study_info$primary_condition) > 0) {
    
    try({list_condition <- append(list_condition, 
                                      paste(unique(temp$study_info$primary_condition), collapse = " + "))})
    
  }else{
    
    try({list_condition <- append(list_condition, "none")})
    
  }
}




start <- Sys.time()

list_condition <- lapply(my_files[], extract_condition_function)

end <- Sys.time()

print(end - start)

list_condition <- data.frame(unlist(list_condition))

merged <- merged %>% bind_cols(list_condition)

unique(merged$unlist.list_condition.)

# Start date   -------------------------

list_starts <- list()

temp$study_info$start_date

extract_starts_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$study_info$start_date) > 0) {
    
    try({list_starts <- append(list_starts, 
                                      paste(unique(temp$study_info$start_date), collapse = " + "))})
    
  }else{
    
    try({list_starts <- append(list_starts, "none")})
    
  }
}




start <- Sys.time()

list_starts <- lapply(my_files[], extract_starts_function)

end <- Sys.time()

print(end - start)

list_starts <- data.frame(unlist(list_starts))

merged <- merged %>% bind_cols(list_starts)

unique(merged$unlist.list_starts.)

# Enrollment   -------------------------

list_enrollment <- list()

temp$study_info$enrollment

extract_enrollment_function <- function(filename) {
  
  try({temp <- parse_study_xml(filename)})
  
  if (length(temp$study_info$start_date) > 0) {
    
    try({list_enrollment <- append(list_enrollment, 
                                      paste(unique(temp$study_info$enrollment), collapse = " + "))})
    
  }else{
    
    try({list_enrollment <- append(list_enrollment, "none")})
    
  }
}




start <- Sys.time()

list_enrollment <- lapply(my_files[], extract_enrollment_function)

end <- Sys.time()

print(end - start)

list_enrollment <- data.frame(unlist(list_enrollment))

merged <- merged %>% bind_cols(list_enrollment)

unique(merged$unlist.list_enrollment.)

fwrite(merged, "temp_merged_trial.txt", sep="\t")

merged <- fread("temp_merged_trial.txt", sep="\t")

# -----------------


# Manual filtering --------------------------
library(xml2)
library(XML)
library(jsonlite)
library(tidyverse)
library(data.table)

temp2 <- fromJSON("NCT00064753.json")
temp2$protocolSection$armsInterventionsModule$interventions
unique(temp2$protocolSection$armsInterventionsModule$interventions$type)
temp2$resultsSection$outcomeMeasuresModule$outcomeMeasures



temp$resultsSection$adverseEventsModule$seriousEvents

temp <- read_xml('./NCT00064753.xml')
temp <- xml2::as_list(temp) 





unique(temp$clinical_study$intervention$intervention_type)


my_files <- list.files(full.names = TRUE)

temp$clinical_study$intervention$intervention_type


# Study IDs

list_study_ids <- list()

extract_ids_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  try({list_study_ids <- append(list_study_ids, paste(unique(temp$clinical_study$id_info$nct_id), collapse = " + ") )})
}

start <- Sys.time()
list_study_ids <- lapply(my_files[], extract_ids_function)
end <- Sys.time()
print(end - start)

list_study_ids <- data.frame(unlist(list_study_ids))


# Interventions Type

list_interventions <- list()

extract_interventions_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (any(temp$clinical_study$intervention$intervention_type %in% "Drug")) {
    try({list_interventions <- append(list_interventions, 
                                      paste(unique(temp$clinical_study$intervention$intervention_type), collapse = " + "))})
  }else{
    try({list_interventions <- append(list_interventions, "none")})
  }
}


start <- Sys.time()
list_interventions <- lapply(my_files[], extract_interventions_function)
end <- Sys.time()
print(end - start)

list_interventions <- data.frame(unlist(list_interventions))



# Interventions  Name

list_interventions_name <- list()

extract_interventions_name_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (temp$clinical_study$intervention$intervention_name) > 0) {
    try({list_interventions_name <- append(list_interventions_name, 
                                      paste(unique(temp$clinical_study$intervention$intervention_name), collapse = " + "))})
  }else{
    try({list_interventions_name <- append(list_interventions_name, "none")})
  }
}


start <- Sys.time()
list_interventions_name <- lapply(my_files[], extract_interventions_name_function)
end <- Sys.time()
print(end - start)

list_interventions_name <- data.frame(unlist(list_interventions_name))









merged <- list_study_ids %>% bind_cols(list_interventions)

merged <- merged %>% bind_cols(list_interventions_name)

names(merged)

unique(merged$unlist.list_interventions.)

merged %>% filter(grepl("Drug", unlist.list_interventions.)) %>%
  select(unlist.list_interventions_name.) %>% distinct()

merged %>% filter(grepl("Biological", unlist.list_interventions.)) %>%
  select(unlist.list_interventions_name.) %>% distinct()


fwrite(merged, "temp_merged_trial.txt", sep="\t")
merged <- fread("temp_merged_trial.txt", sep="\t")




# --------------------------

# Processing Diabetes XML  --------------------------
library(xml2)
library(XML)
library(jsonlite)
library(tidyverse)
library(data.table)


temp <- read_xml('./NCT00696657.xml')
temp <- xml2::as_list(temp) 

temp$clinical_study$sponsors$lead_sponsor

my_files <- list.files(full.names = TRUE)
length(my_files)

# Study IDs

list_study_ids <- list()

extract_ids_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  try({list_study_ids <- append(list_study_ids, paste(unique(temp$clinical_study$id_info$nct_id), collapse = " + ") )})
}

start <- Sys.time()
list_study_ids <- lapply(my_files[], extract_ids_function)
end <- Sys.time()
print(end - start)

list_study_ids <- data.frame(unlist(list_study_ids))


# Interventions Type

list_interventions <- list()

extract_interventions_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$intervention$intervention_type) > 0) {
    try({list_interventions <- append(list_interventions, 
                                      paste(unique(temp$clinical_study$intervention$intervention_type), collapse = " + "))})
  }else{
    try({list_interventions <- append(list_interventions, "none")})
  }
}


start <- Sys.time()
list_interventions <- lapply(my_files[], extract_interventions_function)
end <- Sys.time()
print(end - start)

list_interventions <- data.frame(unlist(list_interventions))



# Interventions  Name

list_interventions_name <- list()

extract_interventions_name_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$intervention$intervention_name) > 0) {
    try({list_interventions_name <- append(list_interventions_name, 
                                      paste(unique(temp$clinical_study$intervention$intervention_name), collapse = " + "))})
  }else{
    try({list_interventions_name <- append(list_interventions_name, "none")})
  }
}


start <- Sys.time()
list_interventions_name <- lapply(my_files[], extract_interventions_name_function)
end <- Sys.time()
print(end - start)

list_interventions_name <- data.frame(unlist(list_interventions_name))



# Phase 

list_phase <- list()

extract_phase_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$phase) > 0) {
    try({list_phase <- append(list_phase, 
                                      paste(unique(temp$clinical_study$phase), collapse = " + "))})
  }else{
    try({list_phase <- append(list_phase, "none")})
  }
}


start <- Sys.time()
list_phase <- lapply(my_files[], extract_phase_function)
end <- Sys.time()
print(end - start)

list_phase <- data.frame(unlist(list_phase))




# Status 

list_status <- list()

extract_status_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$overall_status) > 0) {
    try({list_status <- append(list_status, 
                                      paste(unique(temp$clinical_study$overall_status), collapse = " + "))})
  }else{
    try({list_status <- append(list_status, "none")})
  }
}


start <- Sys.time()
list_status <- lapply(my_files[], extract_status_function)
end <- Sys.time()
print(end - start)

list_status <- data.frame(unlist(list_status))





# Start date 

list_start <- list()

extract_start_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$start_date) > 0) {
    try({list_start <- append(list_start, 
                                      paste(unique(temp$clinical_study$start_date), collapse = " + "))})
  }else{
    try({list_start <- append(list_start, "none")})
  }
}


start <- Sys.time()
list_start <- lapply(my_files[], extract_start_function)
end <- Sys.time()
print(end - start)

list_start <- data.frame(unlist(list_start))






# Completion date 

list_completion <- list()

extract_completion_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$primary_completion_date) > 0) {
    try({list_completion <- append(list_completion, 
                                      paste(unique(temp$clinical_study$primary_completion_date), collapse = " + "))})
  }else{
    try({list_completion <- append(list_completion, "none")})
  }
}


start <- Sys.time()
list_completion <- lapply(my_files[], extract_completion_function)
end <- Sys.time()
print(end - start)

list_completion <- data.frame(unlist(list_completion))



# Study type

list_study_type <- list()

extract_studytype_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$study_type) > 0) {
    try({list_study_type <- append(list_study_type, 
                                      paste(unique(temp$clinical_study$study_type), collapse = " + "))})
  }else{
    try({list_study_type <- append(list_study_type, "none")})
  }
}


start <- Sys.time()
list_study_type <- lapply(my_files[], extract_studytype_function)
end <- Sys.time()
print(end - start)

list_study_type <- data.frame(unlist(list_study_type))







# Mesh terms

list_mesh <- list()

extract_mesh_function <- function(filename) {
  try({temp <- read_xml(filename)})
  try({temp <- xml2::as_list(temp) })
  if (length(temp$clinical_study$intervention_browse$mesh_term) > 0) {
    try({list_mesh <- append(list_mesh, 
                                      paste(unique(temp$clinical_study$intervention_browse$mesh_term), collapse = " + "))})
  }else{
    try({list_mesh <- append(list_mesh, "none")})
  }
}


start <- Sys.time()
list_mesh <- lapply(my_files[], extract_mesh_function)
end <- Sys.time()
print(end - start)

list_mesh <- data.frame(unlist(list_mesh))








# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

merged <- list_study_ids %>% bind_cols(list_study_type)  %>% bind_cols(list_start)  %>% bind_cols(list_completion) %>% bind_cols(list_interventions) %>% 
  bind_cols(list_interventions_name) %>% bind_cols(list_phase) %>% bind_cols(list_status) 

fwrite(merged, "temp_merged_trial_DIA.txt", sep="\t")
merged <- fread("temp_merged_trial_DIA.txt", sep="\t")

#merged <- merged %>% bind_cols(list_mesh)

names(merged)

data.frame(merged %>% filter(unlist.list_phase.!="none"&unlist.list_phase.!="N/A" & !grepl("-", unlist.list_mesh.)) %>%
             mutate(unlist.list_phase.=ifelse(grepl("4", unlist.list_phase.), "Phase4",
                                                    ifelse(grepl("3", unlist.list_phase.), "Phase3",
                                                           ifelse(grepl("2", unlist.list_phase.), "Phase2", "Phase1")))) %>%
             group_by(unlist.list_mesh., unlist.list_phase.) %>% count()) %>%
  ungroup() %>% spread(key=unlist.list_phase., value=n) %>%
  filter(!is.na(Phase1)) %>% drop_na() filter(is.na(Phase2)&is.na(Phase3)&is.na(Phase4))

unique(merged$unlist.list_interventions_name.)

merged %>% filter(grepl("GLP", unlist.list_interventions_name.)|
                    grepl("glp", unlist.list_interventions_name.)|
                    grepl("Glp", unlist.list_interventions_name.)|
                    grepl("lucagon", unlist.list_interventions_name.)) %>%
  group_by(unlist.list_phase.) %>% count()


merged %>% filter(grepl("20", unlist.list_completion.) & 
                    unlist.list_interventions_name. != "none" & 
                    grepl("Drug", unlist.list_interventions.)) %>%
  select(unlist.list_interventions_name.) %>% distinct()

GLP1
SGLT2
DPP4
  

# ---------------------------------

# Loop through JSON files ----------------------------------------


# setwd("C:/Users/paulo/Desktop/Clinical Trials2")
# 
# parent.folder<-"C:/Users/paulo/Desktop/Clinical Trials2"
# 
# sub.folders1 <- list.dirs(parent.folder, recursive=TRUE)[-1]
# 
# sub.folders2 <- list.dirs(sub.folders1, recursive=FALSE)
# 
# r.scripts <- file.path(sub.folders2)
# 
# unlist(as.list(r.scripts))
# 
# files.v <- list()
# 
# for (j in seq_along(r.scripts)) {
#   files.v[[j]] <- dir(r.scripts[j],"\\.json$")
# }
# 
# files.v[1][1][1]



basename(
  list.files(
    pattern = ".json$", recursive = TRUE
    )
  )


all_paths <-   list.files(
    pattern = ".json$", recursive = TRUE
    )

Diabetes_NCT <- fread("Diabetes_Studies.tsv", quote="")[,2]

Diabetes_NCT <- unlist(Diabetes_NCT$`NCT Number`)

all_paths[all_paths %>% str_detect(trial_list)]

list <- list()

for(i in Diabetes_NCT) {

    list <- append(list, all_paths[all_paths %>% str_detect(i)])
    
  }



# ------------------------------------