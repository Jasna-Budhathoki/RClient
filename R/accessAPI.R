library(R6)
library(httr)


baato_API <- R6Class(classname = "baato_API", #name of the class
                     public = list(
                       key = NULL,
                       #' initialize key 
                       #' @param key 
                       
                       initialize = function(key = NA){
                         self$key = key
                       },
                       
                       #' set key 
                       #' @param key 
                       
                       set_key = function(key){
                         self$key <- key 
                         cat("The key is ", self$key)
                       },
                       
                       #' display key 
                       #' @param key 
                       
                       display_key = function(){
                         cat(paste0("the key is", self$key, ".\n"))
                       },
                       
                       #'validate the response 
                       #' Check if the json object returned by API call is valid 
                       #'
                       #' @param req 
                       #'
                       #' @return error message 
                       
                       validate_resp = function(req){
                         if (http_type(req) != "application/json") {
                           stop("API did not return json", call. = FALSE)
                         }
                         
                         if (http_error(req)) {
                           stop(
                             sprintf(
                               "Baato API request failed [%s]\n%s\n>", 
                               status_code(req),
                               useful_response$message
                             ),
                             call. = FALSE
                           )
                         }
                         
                         if (req$status_code != 200) {
                           pull <- jsonlite::fromJSON(content)
                           stop(pull$message, call. = FALSE)
                         }
                         
                       },
                       
                       #' display the response from the API request 
                       #'
                       #' @param req 
                       #'
                       #' @return the API response in JSON format 
                       display_resp = function(req){
                         content <- httr::content(req, as = "text")
                         api_char <- base::rawToChar(req$content)
                         api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
                         api_JSON
                         
                       },
                       
                       #' Calls the reverse api 
                       #'
                       #' @param lat 
                       #' @param lon 
                       #'
                       #' @return the API response in JSON format 
                       rev_api_call = function(lat,lon){
                         reverse_url = "https://api.baato.io/api/v1/reverse"
                         req <- httr::GET(reverse_url, query = list(
                           key = self$key,
                           lat = lat,
                           lon = lon
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                       },

                       #' @param q 
                       #'
                       #' @param limit 
                       #'
                       #' @return the API response in JSON format 
                       search_api_call = function(q,limit) {
                         search_url <- "https://api.baato.io/api/v1/search"
                         req <- httr::GET(search_url, query = list(
                           key = self$key,
                           q = q,
                           limit = limit
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
                       #' Calls the places api 
                       #'
                       #' @param placeId 
                       #'
                       #' @return the API response in JSON format 
                       places_api_call = function(placeId){
                         place_url <- "https://api.baato.io/api/v1/places"
                         req <- httr::GET(place_url, query = list(
                           key = self$key,
                           placeId = placeId
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
  
                      #' Calls nearby api 
                      #'
                      #' @param type 
                      #' @param lat 
                      #' @param lon 
                      #' @param limit 
                      #' @return the API response in JSON format 
                       nearby_api_call = function(type,lat,lon,limit){
                         nearby_url <- "https://api.baato.io/api/v1/search/nearby"
                         req <- httr::GET(nearby_url, query = list(
                           key = self$key,
                           type = type,
                           lat = lat,
                           lon = lon, 
                           limit = limit
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
                       #' Calls the styles api 
                       #' @param style_name 
                       #'
                       #' @return the API response in JSON format 
                       styles_api_call = function(style_name){
                         styles_url <- "https://api.baato.io/api/v1/styles"
                         req <- httr::GET(styles_url, query = list(
                           style_name = style_name,
                           key = self$key
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       },
                       
                       #' Calls the directions api 
                       #' @param points 
                       #' @param mode 
                       #'
                       #' @return the API response in JSON format 
                       baato_directions_api = function(points,mode) {
                         directions_url <- "https://api.baato.io/api/v1/directions"
                         p1 = points[1]
                         p2 = points[2]
                         
                         req <- httr::GET(directions_url, query = list(
                           key = self$key,
                           "points[]" = p1,
                           "points[]" = p2,
                           mode = mode
                         ))
                         
                         self$validate_resp(req)
                         self$display_resp(req)
                         
                       }
                       
                       
                     )
)


