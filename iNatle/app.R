library(shiny)
library(htmltools)
library(bslib)
library(jsonlite)
library(i18n)

# Create list of language options
simplei18n <- grep('-', all_locales, invert = TRUE)
simplei18n <- sapply(simplei18n, \(x) locale_names[x,2][[1]][all_locales[[x]]])
locales_list <- setNames(names(simplei18n), simplei18n)

# Load initial placename data
preprocessed_bbox_data <- read.table("www/supernational_bounding_boxes.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Convert accented text to generic ASCII (won't handle completely non-latin data - for that we would need to go back to stringi)
normalize_string <- function(string) {
  
  # Convert to ASCII and transliterate
  normalized_string <- iconv(string, from = "UTF-8", to = "ASCII//TRANSLIT", sub='')
  
  # Remove any remaining or introduced non-ASCII characters
  normalized_string <- gsub("[^[:alnum:][:space:]]", "", normalized_string)
  
  return(normalized_string)
  
}

# Matches place name to preprocessed Natural Earth data and selects either the best match or the biggest match among ties
get_place_preprocessed <- function(input_name, data) {
  
  columns <- grep('name', colnames(data), value=TRUE)
  input_compare <- tolower(normalize_string(input_name))

  match_results <- list()

  # Check for matches in entire columns first
  for(i in seq_along(columns)) {
    col_compare <- tolower(normalize_string(data[[columns[i]]]))
    match_results[[i]] <- which(col_compare == input_compare)
  }
  
  # Use agrep for fuzzy matching with varying levels of differences
  if(length(unlist(match_results)) == 0) {
    # Define maximum allowed differences
    max_diff <- floor(nchar(input_name) * 0.1)
    for(diff in 0:max_diff) {
      for(i in seq_along(columns)) {
        col_compare <- tolower(normalize_string(data[[columns[i]]]))
        match_results[[i]] <- agrep(input_compare, col_compare, value = FALSE, max.distance = diff / nchar(input_name))
      }
      # Break out of the difference loop if matches are found
      if(length(unlist(match_results)) > 0) {
        break
      }
    }
  }
  
  # Filter out unique matches and their indices
  matched_indices <- unique(unlist(match_results))
  
  if(length(matched_indices) == 0) return(NULL)
  
  # Filter the original data for the matched rows
  matched_data <- data[matched_indices, ]
  
  # Calculate the area of the bounding boxes
  matched_data$area <- (matched_data$max_lat - matched_data$min_lat) * (matched_data$max_lon - matched_data$min_lon)
  
  # Return the row with the largest bounding box
  best_match <- matched_data[which.max(matched_data$area), ]
  return(best_match)
  
}

# Get iNat tax info from an arbitrary string, genus, or ID number
get_tax <- function(taxon_name = NULL, genus = FALSE, taxon_id = NULL, locale = NULL) {
  
  params <- list()
  if(!is.null(taxon_name)) params$q <- taxon_name
  if(genus) params$rank <- "genus"
  if(!is.null(taxon_id)) params$taxon_id <- taxon_id
  if(!is.null(locale)) params$locale <- locale
  
  query <- paste0(paste0(names(params), "=", sapply(as.character(params), URLencode), collapse = "&"), 
                  "&order=desc&order_by=observations_count&per_page=1")
  
  full_url <- paste0("https://api.inaturalist.org/v1/taxa?", query)
  response <- readLines(url(full_url), warn = FALSE)
  json_response <- paste(response, collapse = "")
  obj <- fromJSON(json_response, simplifyVector = FALSE)

  res <- obj$results[[1]]
  
  return(res)
  
}

# Get species counts or a specific observation
get_observations <- function(taxon_id   = NULL, 
                             year       = NULL, 
                             month      = NULL, 
                             day        = NULL, 
                             bounds     = NULL, 
                             user_login = NULL, 
                             locale     = NULL,
                             created_d2 = NULL,
                             counts     = FALSE,
                             page       = 1,
                             per_page   = 1,
                             id         = NULL) {
  
    # Base URL
    base_url <- "https://api.inaturalist.org/v1/observations/"
    if(counts) {
      base_url <- paste0(base_url, "species_counts/?")
    } else {
      base_url <- paste0(base_url, "?")
    }

    # Construct the query parameters
    params <- list(verifiable = 'true',
                   photos     = 'true',
                   hrank      = 'genus',
                   locale     = locale,
                   created_d2 = created_d2,
                   per_page   = per_page,
                   page       = page)

    if(!is.null(taxon_id)) params$taxon_id <- taxon_id
    if(!is.null(year)) params$year <- year
    if(!is.null(month)) {
      month <- as.numeric(month)
      if(month < 1 || month > 12) stop("Month must be between 1 and 12.")
      params$month <- month
    }
    if(!is.null(day)) {
      day <- as.numeric(day)
      if(day < 1 || day > 31) stop("Day must be between 1 and 31.")
      params$day <- day
    }
    if(!is.null(bounds)) {
      if(length(bounds) != 4) stop("Bounds must have 4 coordinates.")
      bounds <- unname(bounds)
      params$swlat <- bounds[1]
      params$swlng <- bounds[2]
      params$nelat <- bounds[3]
      params$nelng <- bounds[4]
    }
    if(!is.null(user_login)) params$user_login <- user_login
    if(!is.null(id)) params$id <- id

    # Create URL with query parameters
    query <- paste0(names(params), "=", sapply(as.character(params), URLencode), collapse = "&")
    full_url <- paste0(base_url, query)

    obj <- tryCatch({
      
      response <- readLines(url(full_url), warn = FALSE)
      json_response <- paste(response, collapse = "")
      fromJSON(json_response, simplifyVector=FALSE)
    
    }, error = \(e) NA)

    return(obj)
  
}

# Choose the word of the day
choose_taxon <- function(obj, maxchar = 100) {

  sp_counts <- sapply(obj$results, \(x) x$count)
  species <- sapply(obj$results, \(x) x$taxon$name)
  genera <- sapply(strsplit(species, ' '), \(x) x[[1]])
  
  gen_counts <- sapply(unique(genera), \(x) sum(sp_counts[genera == x]))
  gen_counts <- gen_counts[sapply(strsplit(unique(genera),''), length) <= maxchar]
  
  if(length(gen_counts) == 0) {
    return(NA)
  } else {
    target_genus <- sample(names(gen_counts), 1)
    return(target_genus)
  }
  
}

get_place_bb <- function(placename) {
  
  obj <- list()
  
  if(placename == '') {
    
    obj$bounds <- NULL
    obj$place_display_name <- NULL
    
  } else {
    
    preprocessed_place <- get_place_preprocessed(placename, preprocessed_bbox_data)
    
    if(!is.null(preprocessed_place)) {
      
      obj$place_display_name <- preprocessed_place$name
      obj$bounds <- as.numeric(preprocessed_place[c('min_lat', 'min_lon', 'max_lat', 'max_lon')])
      
      if(anyNA(obj)) obj <- NA
      
    } else {
  
      base_url <- "https://nominatim.openstreetmap.org/search"
      query <- paste0("?q=", URLencode(placename), "&format=json")
      full_url <- paste0(base_url, query)
  
      response <- readLines(url(full_url), warn = FALSE)
      response_list <- fromJSON(paste(response, collapse = ""), simplifyVector = FALSE)
      
      if(length(response_list) == 0) {
        
        obj <- NA
        
      } else {
        
        areas <- sapply(response_list, \(x) (as.numeric(x$boundingbox[[2]]) - as.numeric(x$boundingbox[[1]])) * (as.numeric(x$boundingbox[[4]]) - as.numeric(x$boundingbox[[3]])))
        biggest <- which.max(areas)
        
        obj$place_display_name <- response_list[[biggest]]$display_name
        obj$bounds <- as.numeric(response_list[[biggest]]$boundingbox)[c(1,3,2,4)]
        
      }
      
    }
  
  }
  
  return(obj)
  
}

# Find all 5-letter substrings with up to one mismatch between target and hint, and censor them so the hint doesn't make it too easy
censor_hints <- function(target, hint) {
  
  target_length <- nchar(target)
  target_segments <- sapply(1:(target_length-4), \(i) substr(tolower(normalize_string(target)), i, i+4))
  
  pattern <- paste(sapply(target_segments, \(x) sapply(1:5, \(y) paste0(substr(x, 1, y-1), '.?', substr(x, y+1, nchar(x))))), collapse='|')
  
  # Allow overlapping matches (adding 'PCRE' pattern...)
  pattern <- paste0('(?=(', pattern, '))')
  
  # Find matches in the original string, using normalized version
  matches <- gregexpr(pattern, tolower(normalize_string(hint)), perl = TRUE)
  
  # Get positions of matches
  match_positions <- unlist(matches)
  
  # Initialize modified string
  modified_string <- hint
  attr(modified_string, 'censored') <- FALSE
  
  # Replace matches in the original string at the identified positions
  if(length(match_positions) > 0 && match_positions[1] != -1) {
    for(i in 1:length(match_positions)) {
      # Replace the matching characters with dashes
      modified_string <- paste0(
        substr(modified_string, 1, match_positions[[i]] - 1), 
        paste(rep('-', attr(matches[[1]], 'capture.length')[[i]]), collapse = ''), 
        substr(modified_string, match_positions[[i]] + attr(matches[[1]], 'capture.length')[[i]], nchar(modified_string))
      )
    }
    
    attr(modified_string, 'censored') <- TRUE

  }
  
  return(modified_string)
  
}

# Create UI. Mostly a frame that is filled in by the server.
ui <- fluidPage(
  
  theme = bs_theme(version = 4),

  # Link to external CSS file
  tags$head(
    includeCSS("www/styles.css")
  ),

  div(id = 'mycontainer', 
    div(
      class = 'setup',
      uiOutput('setup_ui'),
      uiOutput('notices_ui')
    ),
    
    div(
      class = "guesses",
      uiOutput('game_ui')
    )
  ),

  # Link to external JS file
  includeScript("www/custom.js")
)

# Do all the real work
server <- function(input, output, session) {
  
  # Reactive Values Initialization
  r <- reactiveValues(is_random    = TRUE,
                      per_page     = 200,
                      rarity       = 1,
                      maxchar      = 10,
                      placename    = '',
                      input_taxon  = '',
                      user_login   = '',
                      locale       = 'en',
                      ready        = FALSE,
                      started      = FALSE,
                      target_word  = character(0),
                      ref_obs      = NULL,
                      tax_info     = NULL,
                      pretext      = NULL,
                      time_choice  = 1,
                      all_guesses  = list(),
                      notices      = '',
                      finished     = FALSE,
                      showimage    = FALSE,
                      showcommon   = FALSE,
                      current_guess_letters = character(0))
  
  observe({
    
    # If a link was used that has parameters, load them
    query <- parseQueryString(session$clientData$url_search)
    isolate({
      if('locale' %in% names(query)) r$locale <- query[['locale']]
      if('obs_id' %in% names(query)) updateTextInput(session, 'obs_id', value = query[['obs_id']])
      if('placename' %in% names(query)) r$placename <- query[['placename']]
      if('time_choice' %in% names(query)) r$time_choice <- query[['time_choice']]
      if('input_taxon' %in% names(query)) r$input_taxon <- query[['input_taxon']]
      if('user_login' %in% names(query)) r$user_login <- query[['user_login']]
      if('rarity' %in% names(query)) r$rarity <- query[['rarity']]
      if('maxchar' %in% names(query)) r$maxchar <- query[['maxchar']]
    })
    reset_game()
  
  })
  
  time_choices <- c('yesterday', 'on this date', 'in this month of the year', 'some time in the past')

  try_all_params <- function(place_obj, taxid) {
    
    # This should help stabilize the queries throughout the day
    created_d2 <- format(Sys.Date() - 1, "%Y-%m-%d")
    user_login <- if(r$user_login == '') NULL else r$user_login
    r$ref_obs <- NA
    
    if(r$time_choice < 2) {
      timeframe <- 1
      get_random_obs(
        taxid      = taxid,
        user_login = user_login,
        bounds     = place_obj$bounds,
        year       = format(Sys.Date() - 1, "%Y"),
        month      = format(Sys.Date() - 1, "%m"),
        day        = format(Sys.Date() - 1, "%d"),
        created_d2 = created_d2
      )
    }
    
    if(anyNA(r$ref_obs) & r$time_choice < 3) {
      timeframe <- 2
      get_random_obs(
        taxid      = taxid,
        user_login = user_login,
        bounds     = place_obj$bounds,
        month      = format(Sys.Date(), "%m"),
        day        = format(Sys.Date(), "%d"),
        created_d2 = created_d2
      )
    }
    
    if(anyNA(r$ref_obs) & r$time_choice < 4) {
      timeframe <- 3
      get_random_obs(
        taxid      = taxid,
        user_login = user_login,
        bounds     = place_obj$bounds,
        month      = format(Sys.Date(), "%m"),
        created_d2 = created_d2
      )
    }
    
    if(anyNA(r$ref_obs)) {
      timeframe <- 4
      get_random_obs(
        taxid      = taxid,
        user_login = user_login,
        bounds     = place_obj$bounds,
        created_d2 = created_d2
      )
    }
    
    r$pretext <- paste0('I was observed ', time_choices[[timeframe]], paste0(' in ', place_obj$place_display_name)[!is.null(place_obj$place_display_name)], '.')
    
  }
  
  get_specific_obs <- function(obs_id = NULL) {
    
    r$pretext <- '' # maybe should add info about time location drawn from observation instead of user inputs
    
    # Today's observation
    r$ref_obs <- get_observations(id = obs_id, locale = r$locale)
    
    if(!anyNA(r$ref_obs)) {
      
      # Today's target genus
      r$target_word <- tolower(strsplit(r$ref_obs$results[[1]]$taxon$name, ' ')[[1]][[1]])
      
      # Taxonomy info for today's target genus
      r$tax_info <- get_tax(r$target_word, TRUE, locale = r$locale)
      
    }

  }
  
  get_random_obs <- function(taxid = NULL, user_login = NULL, bounds = NULL, year = NULL, month = NULL, day = NULL, created_d2 = NULL) {
    
    # Observation counts fitting search criteria
    sc <- get_observations(
      taxon_id   = taxid,
      user_login = user_login,
      bounds     = bounds,
      year       = year,
      month      = month,
      day        = day,
      created_d2 = created_d2,
      locale     = r$locale,
      counts     = TRUE,
      page       = r$rarity,
      per_page   = r$per_page
    )

    if(anyNA(sc)) {r$ref_obs <- NA; return(FALSE)}
    if(sc$total_results == 0) {r$ref_obs <- NA; return(FALSE)}

    # If rarity is just the page number, then must be limited to number of pages
    max_rarity <- ceiling(sc$total_results / r$per_page)
    if(r$rarity > max_rarity) { 
      
      r$rarity <- max_rarity
      r$notices <- 'Rarity scaled down due to low number of species that match query'
      
      # Get the last page, since first attempt would have returned nothing
      sc <- get_observations(
        taxon_id   = taxid,
        user_login = user_login,
        bounds     = bounds,
        year       = year,
        month      = month,
        day        = day,
        created_d2 = created_d2,
        locale     = r$locale,
        counts     = TRUE,
        page       = r$rarity,
        per_page   = r$per_page
      )
      
    }
    
    # Today's target genus
    r$target_word <- tolower(choose_taxon(sc, r$maxchar))
        
    if(anyNA(r$target_word)) {r$ref_obs <- NA; return(FALSE)}
    
    # Taxonomy info for today's target genus
    r$tax_info <- get_tax(r$target_word, TRUE, locale = r$locale)
    
    # Today's observation
    r$ref_obs <- get_observations(taxon_id   = r$tax_info$id,
                                  user_login = user_login,
                                  bounds     = bounds,
                                  year       = year,
                                  month      = month,
                                  day        = day,
                                  created_d2 = created_d2,
                                  locale     = r$locale)

  }

  assemble_game <- function() {
    
    # Taxonomy info for today's observation
    obstax <- get_tax(taxon_id = r$ref_obs$results[[1]]$taxon$id, locale = r$locale)
    
    # Explanation of today's observation
    output$pretext <- renderText({ paste0('<p style="margin-bottom: 10px">', r$pretext, '</p>') })

    # HTML for image and link
    output$iurl <- renderText({
      c('<a href="', r$ref_obs$results[[1]]$uri, '" target="_blank"><img src="', paste0(dirname(r$ref_obs$results[[1]]$photos[[1]]$url), '/', sub('square', 'medium', basename(r$ref_obs$results[[1]]$photos[[1]]$url))), '" style="width: 330px; max-width: 330px; min-width: 330px;"></a>') # min-width based on keyboard size
    })

    # Contsruct common genus name hint
    if('preferred_common_name' %in% names(r$tax_info)) {
      
      intro_genus <- paste0("The common name for my genus in <b>", names(locales_list)[locales_list == r$locale], "</b> is <em>")
      common_genus <- censor_hints(r$target_word, r$tax_info$preferred_common_name)
      
    } else if('english_common_name' %in% names(r$tax_info)) {
      
      intro_genus <- paste0("The common name for my genus isn't available on iNaturalist in <b>", names(locales_list)[locales_list == r$locale], "</b>.<br>In English, it's <em>")
      common_genus <- censor_hints(r$target_word, r$tax_info$english_common_name)
      
    } else {
      
      intro_genus <- paste0("There doesn't seem to be a common name for my genus!")
      common_genus <- ''
      attr(common_genus, 'censored') <- FALSE

    }
    
    # Construct specific common name hint
    if('preferred_common_name' %in% names(obstax)) {
      
      intro_specific <- paste0("My specific common name in <b>", names(locales_list)[locales_list == r$locale], "</b> is <em>")
      common_specific <- censor_hints(r$target_word, obstax$preferred_common_name)
      
    } else if('english_common_name' %in% names(obstax)) {
      
      intro_specific <- paste0("My specific common name isn't available on iNaturalist in <b>", names(locales_list)[locales_list == r$locale], "</b>.<br>In English, it's <em>")
      common_specific <- censor_hints(r$target_word, obstax$english_common_name)
      
    } else {
      
      intro_specific <- paste0("I don't seem to have a specific common name!")
      common_specific <- ''
      attr(common_specific, 'censored') <- FALSE
      
    }
    
    censorednotice <- "<br>(Similarities to the target genus itself have been hidden)"
    missingnotice <- paste0('<br>Did you know you could <a href="https://www.inaturalist.org/taxa/',
                             if(!'preferred_common_name' %in% names(obstax)) obstax$id else r$tax_info$id, 
                            '" target="_blank">add</a> missing common names to iNaturalist?')
    
    eithercensored <- attr(common_genus, 'censored') | attr(common_specific, 'censored')
    preferredmissing <- (!'preferred_common_name' %in% names(r$tax_info)) | (!'preferred_common_name' %in% names(obstax))
    
    # Construct full HTML for common names hint
    output$common <- renderText(paste0(intro_genus, common_genus, ".</em>"[common_genus!=''], "<br>", intro_specific, common_specific, ".</em>"[common_specific!=''], censorednotice[eithercensored], missingnotice[preferredmissing]))

    # Begin the game!
    r$started <- TRUE

  }
  
  reset_game <- function() {
    r$ready <- FALSE
    r$pretext <- NULL
    r$showimage <- FALSE
    r$showcommon <- FALSE
    r$all_guesses <- list()
    r$finished <- FALSE
    r$started <- FALSE
  }
  
  output$setup_ui <- renderUI({
    req(!r$started & !r$ready)
    tagList(
      h1('iNatle Setup'), 
      hr(),
      
      fluidRow(
       column(6, HTML("<b>Enter the language of your common name hint</b>")),
       column(6, selectInput("locale", NULL, names(locales_list), names(locales_list)[locales_list == isolate(r$locale)], width = '100%'))
      ),
      hr(),
      
      HTML("<p><b>Enter an observation ID number</b><br>to accept someone's challenge</p>"),
      fluidRow(
        column(7, textInput('obs_id', NULL, value = '', width = '100%')),
        column(5, actionButton('submit_specific', 'Accept Challenge!', style = 'white-space: nowrap;'))
      ),
      hr(),
      
      HTML("<p><b>Or, generate a random game</b><br>iNatle will look for relevant observations in your chosen timeframe.<br>If there were none,<br>it will iterate through the list of broader timeframes.</p>"),
      selectInput("time_choice", HTML("<b>Enter a timeframe</b>"), time_choices, time_choices[isolate(r$time_choice)], width = '100%'),
      textInput('place',         HTML("<b>Enter a place name</b><br>or leave it blank"),      value = isolate(r$placename),   width = '100%'),
      textInput('taxon',         HTML("<b>Enter a taxonomic group</b><br>or leave it blank"), value = isolate(r$input_taxon), width = '100%'),
      textInput('user_login',    HTML("<b>Enter a user login name</b><br>or leave it blank"), value = isolate(r$user_login),  width = '100%'),
      HTML("<b>Enter difficulty parameters</b>"),
      fluidRow(
        column(6, numericInput('rarity', "Species rarity",   value = isolate(r$rarity),  min = 1, step = 1, width = '100%')),
        column(6, numericInput('maxchar', "Max word length", value = isolate(r$maxchar), min = 4, step = 1, width = '100%'))
      ),
      actionButton('submit_random', 'Random genus')
    )
  })
  
  observeEvent(input$submit_specific, {
    
    r$locale <- locales_list[[input$locale]]
    r$placename <- ''
    r$input_taxon <- ''
    r$user_login <- ''
    r$notices <- 'Loading challenge...'
    r$is_random <- FALSE
    r$ready <- TRUE
    
  })
  
  observeEvent(input$submit_random, {
    
    r$locale <- locales_list[[input$locale]]
    r$time_choice <- which(input$time_choice == time_choices)
    r$placename <- input$place
    r$input_taxon <- input$taxon
    r$user_login <- input$user_login
    r$rarity <- input$rarity
    r$maxchar <- input$maxchar
    r$notices <- 'Loading random genus...'
    r$is_random <- TRUE
    r$ready <- TRUE
    
  })
  
  output$notices_ui <- renderText({
    req(!r$started)
    HTML(paste0('<p style="margin-top: 10px">', r$notices, '</p>'))
  })
  
  observeEvent(r$ready, {
    
    if(r$ready) {

      if(!r$is_random) {
        
        if(input$obs_id != '') {
          get_specific_obs(input$obs_id)
          if(anyNA(r$ref_obs)) {
            r$notices <- 'ID number does not appear to be valid'
            reset_game()
          } else {
            assemble_game()
          }
        } else {
          r$notices <- 'Please enter an ID number'
          reset_game()
        }
        
      } else {
        
        # iNaturalist ID number for a taxon specified by the user
        if(r$input_taxon == '') {
          taxid <-  NULL
        } else {
          taxid <- tryCatch({
            get_tax(r$input_taxon)$id
          }, error = \(e) NA)
        }
        if(anyNA(taxid)) {
          r$notices <- 'Input taxon is not recognized'
          reset_game()
        } else {
          place_obj <- get_place_bb(r$placename)
          if(anyNA(place_obj)) {
            r$notices <- 'Place name not found'
            reset_game()
          } else {
            try_all_params(place_obj, taxid)
            if(anyNA(r$ref_obs)) {
              r$notices <- 'No observations match all inputs; try again'
              reset_game()
            } else {
              assemble_game()
            }
          }
        }
      }
    }
  })
  
  output$game_ui <- renderUI({
    req(r$started)
    tagList(
      h3("iNatle"),
      h4("What's my genus?"),
      uiOutput("pretext"),
      uiOutput("image_div"),
      uiOutput("common_div"),
      uiOutput("previous_guesses"),
      uiOutput("current_guess"),
      uiOutput("endgame"),
      uiOutput("new_game_ui"),
      uiOutput("keyboard")
    )
  })
  
  observeEvent(input$showimage, {
    r$showimage <- TRUE
  })
  
  observeEvent(input$hideimage, {
    r$showimage <- FALSE
  })
  
  output$image_div <- renderUI({
    if(r$showimage) {
      list(
        htmlOutput('iurl'),
        actionButton('hideimage', 'Hide image', style = "margin: 5px;")
      )
    } else {
      actionButton('showimage', 'Show image', style = "margin: 5px;")
    }
  })
  
  observeEvent(input$showcommon, {
    r$showcommon <- TRUE
  })
  
  observeEvent(input$hidecommon, {
    r$showcommon <- FALSE
  })
  
  output$common_div <- renderUI({
    if(r$showcommon) {
      list(
        htmlOutput('common'),
        actionButton('hidecommon', 'Hide common name', style = "margin: 5px;")
      )
    } else {
      actionButton('showcommon', 'Show common name', style = "margin: 5px;")
    }
  })

  observeEvent(input$new_game, {
    updateTextInput(session, 'obs_id', value = '')
    r$notices <- ''
    reset_game()
  })

  # Rendering UI Elements
  output$previous_guesses <- renderUI({
    res <- lapply(r$all_guesses, function(guess) {
      letters <- guess$letters
      row <- mapply(letters, guess$matches, SIMPLIFY = FALSE, USE.NAMES = FALSE, FUN = function(letter, match) {
        match_type <- match
        div(
          div(toupper(letter), 
              class = paste("letter", match_type)
          ),
        class = 'lettercontainer')
      })
      div(class = "word", row)
    })

    scroll_js <- "document.querySelector('.guesses').scrollTo(0, document.querySelector('.guesses').scrollHeight);"
    tagList(res, tags$script(HTML(scroll_js)))
  })

  output$current_guess <- renderUI({
    if(!r$started || r$finished) return()

    letters <- r$current_guess_letters
    target_length <- isolate(nchar(r$target_word))

    if(length(letters) < target_length) {
      letters[(length(letters) + 1):target_length] <- ""
    }

    div(
      class = "word",
      lapply(letters, function(letter) {
        div(
          div(toupper(letter), 
              class = "letter guess"),
        class = 'lettercontainer')
      })
    )
  })

  output$new_game_ui <- renderUI({
    list(
      HTML(paste0('<p>Disagree with this ID? <a href="', r$ref_obs$results[[1]]$uri,'" target="_blank">Add your own</a>!</p>')),
      actionButton("new_game", "New Game")
    )
  })

  used_letters <- reactive({
    letter_matches <- list()

    lapply(r$all_guesses, function(guess) {
      letters <- guess$letters
      mapply(letters, guess$matches, SIMPLIFY = FALSE, USE.NAMES = FALSE, FUN = function(letter, match) {
               prev_match <- letter_matches[[letter]]
               if(is.null(prev_match) ||
                  (match == "correct" && prev_match != "correct") ||
                  (match == "in-word" && prev_match == "not-in-word")) {
                 letter_matches[[letter]] <<- match
               }
             }
      )
    })

    letter_matches
  })

  keys <- list(
    c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"),
    c("A", "S", "D", "F", "G", "H", "J", "K", "L"),
    c("Enter", "Z", "X", "C", "V", "B", "N", "M", "Back")
  )

  renderKeyboard <- function(keys, prev_match_type) {
    lapply(keys, function(row) {
      row_keys <- lapply(row, function(key) {
        class <- "key"
        key_lower <- tolower(key)
        if(!is.null(prev_match_type[[key_lower]])) {
          class <- c(class, prev_match_type[[key_lower]])
        }
        if(key %in% c("Enter", "Back")) {
          class <- c(class, "wide-key")
        }
        actionButton(key, key, class = class)
      })
      div(class = "keyboard-row", row_keys)
    })
  }

  output$keyboard <- renderUI({
    prev_match_type <- used_letters()
    div(class = "keyboard", renderKeyboard(keys, prev_match_type))
  })

  observeKeyPress <- function(key) {
    observeEvent(input[[key]], {
      if(!r$started || r$finished) return()
      cur <- r$current_guess_letters
      if(length(cur) < isolate(nchar(r$target_word))) {
        r$current_guess_letters <- c(cur, tolower(key))
      }
    })
  }

  # Add listeners foreach key, except Enter and Back
  lapply(unlist(keys, recursive = FALSE), function(key) {
    if(key %in% c("Enter", "Back")) return()
    observeKeyPress(key)
  })

  observeEvent(input$Back, {
    if(length(r$current_guess_letters) > 0) {
      r$current_guess_letters <- r$current_guess_letters[-length(r$current_guess_letters)]
    }
  })

  observeEvent(input$Enter, {
    guess <- paste(r$current_guess_letters, collapse = "")

    all_guesses_new <- r$all_guesses
    check_result <- check_word(guess, r$target_word)
    all_guesses_new[[length(all_guesses_new) + 1]] <- check_result
    r$all_guesses <- all_guesses_new

    if(isTRUE(check_result$win)) r$finished <- TRUE

    r$current_guess_letters <- character(0)
  })

  renderEndgameUI <- function(guesses) {
    lines <- lapply(guesses, function(guess) {
      line <- vapply(guess$matches, function(match) {
        switch(match,
               "correct" = "🟩",
               "in-word" = "🟨",
               "not-in-word" = "⬜"
        )
      }, character(1))

      paste(line, collapse = "")
    })
    div(class = "endgame-content", 
      HTML(paste0('iNatle ID: ', r$ref_obs$results[[1]]$id,
                  if(r$placename != '')   '<br>Place: ' else NULL, r$placename,
                  if(r$input_taxon != '') '<br>Taxon: ' else NULL, r$input_taxon,
                  if(r$user_login != '')  '<br>User: ' else NULL,  r$user_login,
                  '<br>Hint language: ', names(locales_list)[locales_list == r$locale], '<br><br>')),
      HTML(paste(lines, collapse = '<br>')),
      HTML(paste0('<br><br>https://thecnidaegritty.org/iNatle/?obs_id=', r$ref_obs$results[[1]]$id))
    )
  }

  output$endgame <- renderUI({
    if(r$finished) {
      renderEndgameUI(r$all_guesses)
    }
  })

}

check_word <- function(guess_str, target_str) {
  
  target <- strsplit(target_str, "")[[1]]
  guess <- strsplit(guess_str, "")[[1]]
  guess <- c(guess, rep(' ', length(target) - length(guess)))

  result <- rep("not-in-word", length(guess))
  remaining <- character(0)

  for(i in seq_along(guess)) {
    if(guess[i] == target[i]) {
      result[i] <- "correct"
    } else {
      remaining <- c(remaining, target[i])
    }
  }

  for(i in seq_along(guess)) {
    if(guess[i] != target[i] && guess[i] %in% remaining) {
      result[i] <- "in-word"
      remaining <- remaining[-match(guess[i], remaining)]
    }
  }

  list(
    word = guess_str,
    letters = guess,
    matches = result,
    win = all(result == "correct")
  )

}

shinyApp(ui, server)

# Use my custom shinylive template to make sure there's jekyll frontmatter on the export
# Then make simple update to shinylive.js so the url parameters are forwarded to the iframe (sed command specific to macos)
# shinylive::export('~/scripts/iNatle/iNatle/', '~/scripts/thecnidaegritty/iNatle_raw/', template_dir = "~/scripts/thecnidaegritty/scripts/shinylive_embedded_jekyll_template", template_params = list(title = 'iNatle'))
# system("sed -i '' 's/viewerFrameRef.current.src = appInfo.urlPath/viewerFrameRef.current.src = appInfo.urlPath + window.location.search;/g' ~/scripts/thecnidaegritty/iNatle_raw/shinylive/shinylive.js")
# httpuv::runStaticServer("~/scripts/thecnidaegritty/iNatle_raw/")
