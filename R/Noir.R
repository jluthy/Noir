#' Noir 
#'
#' Make a call to API to find loaded workspaces.
#' 
#' @return The Noir code will make call to API to gather and then create data frame of connected
#' workspace names, samples, parameters, and populations. Otherwise returns error message.
#' @param character The url path to workspace via API
#' @examples \dontrun dfWorkspaces <- getWorkspaces(url)
#' @note This code can be used to feed other plugins that would require this info.
#' @source This code could be made faster by not writing out the data frames and
#' perhaps by streaming directly from the API.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @export
getWorkspaces <- function(character) {
  result = tryCatch({
    response <-
      GET(url =  "http://localhost:4567/api/v1/workspaces/all")
    response_status <- response$status_code
    if (response_status  == 404) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getWorkspaces Was A Sucess")
      api_request_content <- rawToChar(response$content)
      api_request_content_list = data.frame(fromJSON(api_request_content))
      colnames(api_request_content_list) <- "Workspaces"
      return(api_request_content_list)
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
# dfWorkspaces <- getWorkspaces(url)
#'
#' Set new url based on loaded workspace to find Samples loaded.
#' 
#' @param character This will become the new url to find samples based on loaded workspace.
#' @return This will be new url to find samples.
#' @importFrom urltools param_set url_decode
#' @examples \dontrun urlSams <- buildSamURL()
#' @importFrom urltools url_decode param_set
#' @export
buildSamURL <-function(character) {
  urlSams <- "http://localhost:4567/api/v1/samples/all?workspaceid="
  urlSams <- param_set(urlSams, key = "workspaceid", value = dfWorkspaces[1,])
  urlSams <- url_decode(urlSams)
}
# urlSams <- buildSamURL()
#'
#' Make call to API for Samples Names loaded into a workspace
#' 
#' @param character The path to workspace via API
#' @examples \dontrun dfSamples <- getSamples(urlSams)
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @export
getSamples <- function(character) {
  result = tryCatch({
    response <-
      GET(url = urlSams)
    response_status <- response$status_code
    if (response_status  == 404) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getSamples Was A Sucess")
      api_request_content <- rawToChar(response$content)
      api_request_content_list = data.frame(fromJSON(api_request_content))
      colnames(api_request_content_list) <- "Samples"
      return(api_request_content_list)
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
# dfSamples <- getSamples(urlSams)
#'
#' Set new url to find Parameters associated with Samples loaded into a workspace
#' 
#' @return This function will return the url needed to find samples based on loaded workspace.
#' @param character The start of the url needed to query the sample names loaded into workspace.
#' @examples \dontrun urlParams <- buildParamURL()
#' @importFrom urltools param_set url_decode
#' @export
buildParamURL <- function(character) {
  urlParams <- "http://localhost:4567/api/v1/parameters/all?workspaceid="
  urlParams <- param_set(urlParams, key = "workspaceid", value = dfWorkspaces[1,])
  urlParams <- paste0(urlParams, "&sampleid=")
  urlParams <- param_set(urlParams, key = "sampleid", value = dfSamples[1,])
  urlParams <<- url_decode(urlParams)
}
# urlParams <- buildParamURL()
#'
#' Make call to API for Parameters Associated with Samples loaded into a workspace
#' 
#' @param character The path to parameters via API
#' @examples \dontrun dfParameters <- getParameters(urlParams)
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @export
getParameters <- function(character) {
  result = tryCatch({
    response <-
      GET(url = urlParams)
    response_status <- response$status_code
    if (response_status  == 404) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getParameters Was A Sucess")
      api_request_content <- rawToChar(response$content)
      api_request_content_list = data.frame(fromJSON(api_request_content))
      colnames(api_request_content_list) <- "Parameters"
      return(api_request_content_list)
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
# dfParameters <- getParameters(urlParams)
#'
#' Set new url to find Populations
#' 
#' @return This will return new url for finding populations
#' @param character the path to the populations via API
#' @examples \dontrun urlPops <- buildPopsURL()
#' @importFrom urltools param_set url_decode
#' @export
buildPopsURL <- function(character) {
  urlPops <- "http://localhost:4567/api/v1/populations/all?workspaceid="
  urlPops <- param_set(urlPops, key = "workspaceid", value = dfWorkspaces[1,])
  urlPops <- paste0(urlPops, "&sampleid=")
  urlPops <- param_set(urlPops, key = "sampleid", value = dfSamples[1,])
  urlPops <<- url_decode(urlPops)
}
# urlPops <- buildPopsURL()
#'
#' Make a call to API to get Populations associated with Samples
#' 
#' @return This will return Populations from a workspace
#' @param character the url path for finding populations in workspace
#' @examples \dontrun dfPopulations <- getPopulations(urlPops)
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET status_code
#' @export
getPopulations <- function(character) {
  result = tryCatch({
    response <-
      GET(url = urlPops)
    response_status <- response$status_code
    if (response_status  == 404) {
      warn_for_status("Error")
    } else if (response_status == 200) {
      writeLines("getPopulations Was A Sucess")
      api_request_content <- rawToChar(response$content)
      api_request_content_list = data.frame(fromJSON(api_request_content))
      colnames(api_request_content_list) <- "Populations"
      return(api_request_content_list)
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
# dfPopulations<- getPopulations(urlPops)