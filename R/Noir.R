#' @title Noir
#' 
#' Make a call to API to find loaded workspaces.
#' 
#' @return The Noir code will make call to API to gather and then create data frame of connected
#' workspace names, samples, parameters, and populations. Otherwise returns error message.
#' @param character The url path to workspace via API
#' @note This code can be used to feed other plugins that would require this info.
#' This code could be modified to stream info directly from the API and not writing  
#' out the data frames.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @examples \dontrun{
#' urlWSP <- "http://localhost:4567/api/v1/workspaces/all"
#' getWorkspaces(urlWSP)
#' } 
#' @export
getWorkspaces <- function(character) {
  result <- tryCatch({
    response <-
      GET(url = urlWSP )
    response_status <- response$status_code
    if (response_status  == 400) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getWorkspaces Was A Sucess")
      workspaces <- rawToChar(response$content)
      dfWorkspaces <<- data.frame(fromJSON(workspaces))
      # colnames(dfWorkspaces) <- "Workspaces"
      return(dfWorkspaces)
    } else if (response_status == 404) {
      writeLines("No SeqGeqWorkspaces Active or Found. Please Load Workspace")
      return(NULL)
    }
  }, warning = function(w) {
    writeLines(paste0("Get Workspaces Generated a Warning", w))
  }, error = function(e) {
    writeLines(paste0("Get Workspaces Generated an Error", e))
  }, finally = {
    writeLines("Get Workspaces Finalized")
  })
  return(result)
}
#'
# urlWSP <- "http://localhost:4567/api/v1/workspaces/all"
# getWorkspaces(urlWSP)
#'
#' Set new url based on loaded workspace to find Samples loaded.
#' 
#' @param character This will become the new url to find samples based on loaded workspace.
#' @return This will be new url to find samples.
#' @importFrom urltools param_set url_decode
#' @examples \dontrun{
#' getWorkspaces(url)
#' urlSams <- buildSamURL("http://localhost:4567/api/v1/samples/all?workspaceid=")
#' } 
#' @export
buildSamURL <-function(character) {
  urlSams <- url
    urlSams <- param_set(urlSams, key = "workspaceid", value = dfWorkspaces[1,])
    urlSams <- url_decode(urlSams)
}
# url <- "http://localhost:4567/api/v1/samples/all?workspaceid="
# urlSams <- buildSamURL(url)
#'
#' Make call to API for Samples Names loaded into a workspace
#' 
#' @param character The path to workspace via API
#' @examples \dontrun{
#' urlSams <- buildSamURL("http://localhost:4567/api/v1/samples/all?workspaceid=")
#' getSamples(urlSams)
#' }
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @export
getSamples <- function(character) {
  result <- tryCatch({
    response <-
      GET(url = urlSams)
    response_status <- response$status_code
    if (response_status  == 400) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getSamples Was A Sucess")
      samples <- rawToChar(response$content)
      dfSamples <<- data.frame(fromJSON(samples))
      return(dfSamples)
    } else if (response_status == 404) {
      writeLines("No Samples Active or Found. Please Load Workspace and/or Samples")
      return(NULL)
    }
  }, warning = function(w) {
    writeLines(paste0("Get Samples Generated a Warning", w))
  }, error = function(e) {
    writeLines(paste0("Get Samples Generated an Error", e))
  }, finally = {
    writeLines("Get Samples Finalized")
  })
  return(result)
}
#'
# getSamples(urlSams)
#'
#' Set new url to find Parameters associated with Samples loaded into a workspace
#' 
#' @return This function will return the url needed to find samples based on loaded workspace.
#' @param character The start of the url needed to query the sample names loaded into workspace.
#' @importFrom urltools param_set url_decode
#' @examples \dontrun{
#' url <- "http://localhost:4567/api/v1/parameters/all?workspaceid="
#' urlParams <- buildParamURL(url)
#' }
#' @export
buildParamURL <- function(character) {
  urlParams <- url
   urlParams <- param_set(urlParams, key = "workspaceid", value = dfWorkspaces[1,])
   urlParams <- paste0(urlParams, "&sampleid=")
   urlParams <- param_set(urlParams, key = "sampleid", value = dfSamples[1,])
   urlParams <- url_decode(urlParams)
}
# url <- "http://localhost:4567/api/v1/parameters/all?workspaceid="
# urlParams <- buildParamURL(url)
#'
#' Make call to API for Parameters Associated with Samples loaded into a workspace
#' 
#' @param character The path to parameters via API
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @examples \dontrun{
#' urlParams <- buildParamURL(url)
#' getParameters(urlParams)
#' }
#' @export
getParameters <- function(character) {
  result = tryCatch({
    response <-
      GET(url = urlParams)
    response_status <- response$status_code
    if (response_status  == 400) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getParameters Was A Sucess")
      parameters <- rawToChar(response$content)
      dfParameters <<- data.frame(fromJSON(parameters))
      return(dfParameters)
    } else if (response_status == 404) {
      writeLines("No Parameters Active or Found. Please Load Workspace and/or Samples")
      return(NULL)
    }
  }, warning = function(w) {
    writeLines(paste0("Get Parameters Generated a Warning", w))
  }, error = function(e) {
    writeLines(paste0("Get Parameters Generated an Error", e))
  }, finally = {
    writeLines("Get Parameters Finalized")
  })
  return(result)
}
#'
# 
# getParameters(urlParams)
#'
#' Set new url to find Populations
#' 
#' @return This will return new url for finding populations
#' @param character the path to the populations via API
#' @examples \dontrun{
#' url <- "http://localhost:4567/api/v1/populations/all?workspaceid="
#' urlPops <- buildPopsURL(url)
#' } 
#' @importFrom urltools param_set url_decode
#' @export
buildPopsURL <- function(character) {
  urlPops <- url
   urlPops <- param_set(urlPops, key = "workspaceid", value = dfWorkspaces[1,])
   urlPops <- paste0(urlPops, "&sampleid=")
   urlPops <- param_set(urlPops, key = "sampleid", value = dfSamples[1,])
   urlPops <- url_decode(urlPops)
}
# url <- "http://localhost:4567/api/v1/populations/all?workspaceid="
# urlPops <- buildPopsURL(url)
#'
#' Make a call to API to get Populations associated with Samples
#' 
#' @return This will return Populations from a workspace
#' @param character the url path for finding populations in workspace
#' @examples \dontrun{
#' urlPops <- buildPopsURL(url)
#' getPopulations(urlPops)
#' } 
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @export
getPopulations <- function(character) {
  result = tryCatch({
    response <-
      GET(url = urlPops)
    response_status <- response$status_code
    if (response_status  == 400) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getPopulations Was A Sucess")
      populations <- rawToChar(response$content)
      dfPopulations <<- data.frame(fromJSON(populations))
      return(dfPopulations)
    } else if (response_status == 404) {
      writeLines("No Populations Active or Found. Please Load Workspace, Samples, and gate on a population")
      return(NULL)
    }
  }, warning = function(w) {
    writeLines(paste0("Get Populations Generated a Warning", w))
  }, error = function(e) {
    writeLines(paste0("Get Populations Generated an Error", e))
  }, finally = {
    writeLines("Populations Finalized")
  })
  return(result)
}
# 
# getPopulations(urlPops)
# 