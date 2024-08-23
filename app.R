library(shiny)
library(htmltools)
library(bslib)
library(jsonlite)
library(i18n)
library(stringr)
library(stringi)

# Create list of language options
simplei18n <- grep('-', all_locales, invert = TRUE)
simplei18n <- sapply(simplei18n, \(x) locale_names[x,2][[1]][all_locales[[x]]])
locales_list <- setNames(names(simplei18n), simplei18n)

# Create UI. Mostly a frame that is filled in by the server.
ui <- fluidPage(
  
  theme = bs_theme(version = 4),
  tags$style(type='text/css', "#showimage { margin: 5px; }"),
  tags$style(type='text/css', "#hideimage { margin: 5px; }"),
  tags$style(type='text/css', "#showcommon { margin: 5px; }"),
  tags$style(type='text/css', "#hidecommon { margin: 5px; }"),
  
  title = "iNatle: Local Genera",

  # Link to external CSS file
  tags$head(
    includeCSS("www/styles.css")
  ),

  div(
    class = "guesses",
    uiOutput('allout')
  ),

  # Link to external JS file
  includeScript("www/custom.js")
)

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

# Get species counts
get_sc <- function(taxon_id   = NULL, 
                   year       = NULL, 
                   month      = NULL, 
                   day        = NULL, 
                   bounds     = NULL, 
                   user_login = NULL, 
                   locale     = NULL,
                   created_d2 = NULL,
                   page       = 1,
                   maxresults = 200) {
  
    # Base URL
    base_url <- "https://api.inaturalist.org/v1/observations/species_counts/?"

    # Construct the query parameters
    params <- list(verifiable = 'true',
                   photos     = 'true',
                   hrank      = 'genus',
                   locale     = locale,
                   created_d2 = created_d2,
                   per_page   = maxresults,
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

    # Create URL with query parameters
    query <- paste0(names(params), "=", sapply(as.character(params), URLencode), collapse = "&")
    full_url <- paste0(base_url, query)

    response <- readLines(url(full_url), warn = FALSE)
    json_response <- paste(response, collapse = "")
    obj <- fromJSON(json_response, simplifyVector=FALSE)

    return(obj)
  
}

# Choose the word of the day
choose_taxon <- function(obj, seed) {
  
  set.seed(seed)

  sp_counts <- sapply(obj$results, \(x) x$count)
  species <- sapply(obj$results, \(x) x$taxon$name)
  genera <- sapply(strsplit(species, ' '), \(x) x[[1]])
  
  gen_counts <- sapply(unique(genera), \(x) sum(sp_counts[genera == x]))

  target_genus <- sample(names(gen_counts), 1, prob = 1 / (gen_counts + 0.1))

  return(target_genus)
  
}

# Choose a specific observation to represent the word of the day
get_observation <- function(taxon_id   = NULL, 
                            year       = NULL, 
                            month      = NULL, 
                            day        = NULL, 
                            bounds     = NULL, 
                            user_login = NULL, 
                            locale     = NULL,
                            created_d2 = NULL) {
  
    # Base URL
    base_url <- "https://api.inaturalist.org/v1/observations/?"

    # Construct the query parameters
    params <- list(verifiable = 'true',
                   photos     = 'true',
                   hrank      = 'genus',
                   locale     = locale,
                   created_d2 = created_d2,
                   per_page   = 1,
                   page       = 1)

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

    # Create URL with query parameters
    query <- paste0(names(params), "=", sapply(as.character(params), URLencode), collapse = "&")
    full_url <- paste0(base_url, query)

    response <- readLines(url(full_url), warn = FALSE)
    json_response <- paste(response, collapse = "")
    obj <- fromJSON(json_response, simplifyVector=FALSE)

    return(obj)
  
}

# Find all 5-letter substrings with up to one mismatch between target and hint, and censor them so the hint doesn't make it too easy
censor_hints <- function(target, hint) {
  
  target_length <- nchar(target)
  target_segments <- sapply(1:(target_length-4), \(i) substr(stri_trans_general(tolower(target), "Latin-ASCII"), i, i+4))
  
  pattern <- paste(sapply(target_segments, \(x) sapply(1:5, \(y) paste0(substr(x, 1, y-1), '.?', substr(x, y+1, nchar(x))))), collapse='|')
  
  # Allow overlapping matches (adding 'PCRE' pattern...)
  pattern <- paste0('(?=(', pattern, '))')
  
  # Find matches in the original string, using normalized version
  matches <- gregexpr(pattern, stri_trans_general(tolower(hint), "Latin-ASCII"), perl = TRUE)
  
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

# Do all the real work
server <- function(input, output, session) {
  
  # Reactive Values Initialization
  r <- reactiveValues(submitted    = FALSE,
                      difficulty   = 1,  # not yet exposed - might not be as simple as page number (e.g. consider what do when fewer than per_page results!)
                      loadtext     = '',
                      started      = FALSE,
                      current_seed = NULL,
                      target_word  = character(0),
                      ref_obs      = NULL,
                      tax_info     = NULL,
                      locale       = 'en',
                      all_guesses  = list(),
                      error        = '',
                      finished     = FALSE,
                      showimage    = FALSE,
                      showcommon   = FALSE,
                      current_guess_letters = character(0),
                      current_placelevel    = 1)

  placelevels <- c('continent', 'region', 'country', 'settlement')

  try_place <- function(placename, placelevel) {

    base_url <- "https://nominatim.openstreetmap.org/search"
    query <- paste0("?q=", URLencode(placename), "&format=json&featuretype=", tolower(placelevel))
    full_url <- paste0(base_url, query)

    response <- readLines(url(full_url), warn = FALSE)
    json_response <- paste(response, collapse = "")
    obj <- fromJSON(json_response, simplifyVector=FALSE)[[1]]

    place_display_name <- obj$display_name
    bounds <- as.numeric(obj$boundingbox)[c(1,3,2,4)]
    
    # This should help stabilize the queries throughout the day
    created_d2 <- format(Sys.Date() - 1, "%Y-%m-%d")

    if(any(is.na(bounds))) {

      r$current_placelevel <- r$current_placelevel + 1
      try_place(input$place, placelevels[[r$current_placelevel]])

    } else {
      
      # iNaturalist ID number for a taxon specified by the user
      taxid <- if(input$taxon == 'anything') NULL else get_tax(input$taxon)$id
      user_login <- if(input$user_login == '') NULL else input$user_login

      tryCatch({
        assemble_game(
          taxid      = taxid,
          user_login = user_login,
          bounds     = bounds,
          year       = format(Sys.Date() - 1, "%Y"),
          month      = format(Sys.Date() - 1, "%m"),
          day        = format(Sys.Date() - 1, "%d"),
          created_d2 = created_d2,
          locale     = r$locale,
          pretext    = paste0('I was observed yesterday in ', place_display_name, '!')
        )
      }, error = function(e1) {
        print(e1)
        tryCatch({
          assemble_game(
            taxid      = taxid,
            user_login = user_login,
            bounds     = bounds,
            month      = format(Sys.Date(), "%m"),
            day        = format(Sys.Date(), "%d"),
            created_d2 = created_d2,
            locale     = r$locale,
            pretext    = paste0('I was observed on this date in previous years in ', place_display_name, '!')
          )
        }, error = function(e2) {
          print(e2)
          tryCatch({
            assemble_game(
              taxid      = taxid,
              user_login = user_login,
              bounds     = bounds,
              month      = format(Sys.Date(), "%m"),
              created_d2 = created_d2,
              locale     = r$locale,
              pretext    = paste0('I was observed in this month in previous years in ', place_display_name, '!')
            )
          }, error = function(e3) {
            print(e3)
            tryCatch({
              assemble_game(
                taxid      = taxid,
                user_login = user_login,
                bounds     = bounds,
                created_d2 = created_d2,
                locale     = r$locale,
                pretext    = paste0('I was observed at some time in the past in ', place_display_name, '!')
              )
            }, error = function(e4) {
              print(e4)
              r$error <- 'Not enough observations or species; try again'
              reset_game()
            })
          })
        })
      })

    }
  }

  assemble_game <- function(taxid = NULL, user_login = NULL, bounds = NULL, year = NULL, month = NULL, day = NULL, created_d2 = NULL, locale = NULL, pretext = NULL) {
    
    # Observation counts fitting search criteria
    sc <- get_sc(
      taxon_id   = taxid,
      user_login = user_login,
      bounds     = bounds,
      year       = year,
      month      = month,
      day        = day,
      created_d2 = created_d2,
      locale     = locale,
      page       = r$difficulty
    )

    # Today's target genus
    r$target_word <- tolower(choose_taxon(sc, r$current_seed))
    
    # Taxonomy infor for today's target genus
    r$tax_info <- get_tax(r$target_word, TRUE, locale = locale)
    
    # Today's observation
    r$ref_obs <- get_observation(taxon_id   = r$tax_info$id,
                                 user_login = user_login,
                                 bounds     = bounds,
                                 year       = year,
                                 month      = month,
                                 day        = day,
                                 created_d2 = created_d2,
                                 locale     = locale)

    # Taxonomy info for today's observation
    obstax <- get_tax(taxon_id = r$ref_obs$results[[1]]$taxon$id, locale = locale)
    
    # Explanation of today's observation
    output$pretext <- renderText({ paste0('<p style="margin-bottom: 10px">', pretext, '</p>') })

    # HTML for image and link
    output$iurl <- renderText({
      c('<a href="', r$ref_obs$results[[1]]$uri, '" target="_blank"><img src="', paste0(dirname(r$ref_obs$results[[1]]$photos[[1]]$url), '/', sub('square', 'medium', basename(r$ref_obs$results[[1]]$photos[[1]]$url))), '"></a>')
    })

    # Contsruct common genus name hint
    if('preferred_common_name' %in% names(r$tax_info)) {
      
      intro_genus <- paste0("The common name for my genus in <b>", names(locales_list)[locales_list == locale], "</b> is <em>")
      common_genus <- censor_hints(r$target_word, r$tax_info$preferred_common_name)
      
    } else if('english_common_name' %in% names(r$tax_info)) {
      
      intro_genus <- paste0("The common name for my genus isn't available on iNaturalist in <b>", names(locales_list)[locales_list == locale], "</b>.<br>In English, it's <em>")
      common_genus <- censor_hints(r$target_word, r$tax_info$english_common_name)
      
    } else {
      
      intro_genus <- paste0("There doesn't seem to be a common name for my genus!")
      common_genus <- ''
      attr(common_genus, 'censored') <- FALSE

    }
    
    # Construct specific common name hint
    if('preferred_common_name' %in% names(obstax)) {
      
      intro_specific <- paste0("My specific common name in <b>", names(locales_list)[locales_list == locale], "</b> is <em>")
      common_specific <- censor_hints(r$target_word, obstax$preferred_common_name)
      
    } else if('english_common_name' %in% names(obstax)) {
      
      intro_specific <- paste0("My specific common name isn't available on iNaturalist in <b>", names(locales_list)[locales_list == locale], "</b>.<br>In English, it's <em>")
      common_specific <- censor_hints(r$target_word, obstax$english_common_name)
      
    } else {
      
      intro_specific <- paste0("I don't seem to have a specific common name!")
      common_specific <- ''
      attr(common_specific, 'censored') <- FALSE
      
    }
    
    censorednotice <- "<br>(The target genus itself has been hidden)"
    missingnotice <- paste0("<br>Did you know you could <a href=\"https://www.inaturalist.org/taxa/",
                             if(!'preferred_common_name' %in% names(obstax)) obstax$id else r$tax_info$id, 
                            "\" target=\"_blank\">add</a> missing common names to iNaturalist?")
    
    eithercensored <- attr(common_genus, 'censored') | attr(common_specific, 'censored')
    preferredmissing <- (!'preferred_common_name' %in% names(r$tax_info)) | (!'preferred_common_name' %in% names(obstax))
    
    # Construct full HTML for common names hint
    output$common <- renderText(paste0(intro_genus, common_genus, ".</em>"[common_genus!=''], "<br>", intro_specific, common_specific, ".</em>"[common_specific!=''], censorednotice[eithercensored], missingnotice[preferredmissing]))

    # Begin the game!
    r$started <- TRUE

  }
  
  reset_game <- function() {
    r$submitted <- FALSE
    r$current_seed <- NULL
    r$showimage <- FALSE
    r$showcommon <- FALSE
    r$all_guesses <- list()
    r$current_placelevel <- 1
    r$finished <- FALSE
    r$started <- FALSE
  }
  
  output$allout <- renderUI({
    if(!r$started) {
      tagList(
        h3("Set up!"),
        HTML("<p>iNatle will look for any relevant observations yesterday.<br><br>If there were none,<br>it will look for observations on this day in previous years,<br> then this month in previous years,<br> then all observations from any time.</p>"),
        textInput('place', h3('Enter a place name'), value = 'Oregon', width = '100%'),
        textInput('taxon', div(h3('Enter a taxonomic group'), HTML("<p>or 'anything'</p>")), value = 'Plantae', width = '100%'),
        textInput('user_login', div(h3('Enter a user login name'), HTML("<p>or leave it blank</p>")), value = '', width = '100%'),
        selectInput("locale", h3('Enter the language of your common name hint'), names(locales_list), 'English', width = '100%'),
        actionButton('submit', 'Random genus'),
        actionButton('daily_stable', "Today's genus", inline = TRUE),
        uiOutput('loadtext_ui'),
        uiOutput('error_ui')
      )
    } else {
      tagList(
        h3("What's my genus?"),
        uiOutput("pretext"),
        uiOutput("image_div"),
        uiOutput("common_div"),
        uiOutput("previous_guesses"),
        uiOutput("current_guess"),
        uiOutput("endgame"),
        uiOutput("new_game_ui"),
        uiOutput("keyboard")
      )
    }
  })
  
  observeEvent(input$daily_stable, {
    r$loadtext <- "Loading today's genus..."
    r$current_seed <- format(Sys.Date(), '%Y%m%d')
    r$submitted <- TRUE
  })
  
  observeEvent(input$submit, {
    r$loadtext <- 'Loading random genus...'
    r$submitted <- TRUE
  })

  # Observing Events
  output$loadtext_ui <- renderUI({
    p(r$loadtext)
  })
  
   output$error_ui <- renderUI({
    p(r$error)
  })
  
  observeEvent(r$submitted, {
    if(r$submitted) {
      r$locale <- locales_list[[input$locale]]
      r$error <- ""
      try_place(input$place, placelevels[[r$current_placelevel]])
    }
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
        actionButton('hideimage', 'Hide image')
      )
    } else {
      actionButton('showimage', 'Show image')
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
        actionButton('hidecommon', 'Hide common name')
      )
    } else {
      actionButton('showcommon', 'Show common name')
    }
  })

  observeEvent(input$new_game, {
    r$error <- ''
    reset_game()
  })

  # Rendering UI Elements
  output$previous_guesses <- renderUI({
    res <- lapply(r$all_guesses, function(guess) {
      letters <- c(guess$letters, rep(' ', length(guess$matches) - length(guess$letters)))
      row <- mapply(letters, guess$matches, SIMPLIFY = FALSE, USE.NAMES = FALSE, FUN = function(letter, match) {
        match_type <- match
        div(toupper(letter), class = paste("letter", match_type))
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
        div(toupper(letter), class = "letter guess")
      })
    )
  })

  output$new_game_ui <- renderUI({
    if(r$finished) {
      actionButton("new_game", "New Game")
    }
  })

  used_letters <- reactive({
    letter_matches <- list()

    lapply(r$all_guesses, function(guess) {
      letters <- c(guess$letters, rep(' ', length(guess$matches) - length(guess$letters)))
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

      div(paste(line, collapse = ""))
    })
    div(class = "endgame-content", lines)
  }

  output$endgame <- renderUI({
    if(r$finished) {
      renderEndgameUI(r$all_guesses)
    }
  })

}

check_word <- function(guess_str, target_str) {

  guess <- strsplit(guess_str, "")[[1]]
  target <- strsplit(target_str, "")[[1]]

  result <- rep("not-in-word", length(target))
  remaining <- character(0)

  for(i in seq_along(target)) {
    if(i <= length(guess)) {
      if(guess[i] == target[i]) {
        result[i] <- "correct"
      } else {
        remaining <- c(remaining, target[i])
      }
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

#shinylive::export('~/scripts/iNatle/', '~/scripts/thecnidaegritty/iNatle/', template_params=list(title='iNatle'))
#httpuv::runStaticServer("~/scripts/thecnidaegritty/iNatle/")
