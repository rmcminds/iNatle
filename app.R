library(shiny)
library(htmltools)
library(bslib)
library(jsonlite)

ui <- fluidPage(
  theme = bs_theme(version = 4),
  title = "iNatle: Local Genera",

  # Link to external CSS file
  tags$head(
    includeCSS("www/styles.css")
  ),

  div(
    class = "guesses",
    conditionalPanel(
      condition = "!output.started",
      h3("Set up!"),
      HTML("<p>iNatle will look for any relevant observations yesterday.<br><br>If there were none,<br>it will look for observations on this day in previous years,<br> then this month in previous years,<br> then all observations from any time.</p>"),
      textInput('Place', h3('Enter a place name'), value = 'Oregon', width = '100%'),
      textInput('Taxon', div(h3('Enter a taxonomic group'), HTML("<p>or 'anything'")), value = 'Plantae', width = '100%'),
      actionButton('submit', 'Submit'),
      conditionalPanel(
        condition = "output.error",
        HTML('<br>Not enough observations or species; try again')
      )
    ),
    conditionalPanel(
      condition = "output.started",
      h3("What's my genus?"),
      conditionalPanel(
        condition = "!input.showimage",
        actionButton('showimage', 'Show image')
      ),
      conditionalPanel(
        condition = "input.showimage",
        uiOutput("image")
      ),
      conditionalPanel(
        condition = "!input.showcommon",
        actionButton('showcommon', 'Show common names')
      ),
      conditionalPanel(
        condition = "input.showcommon",
        h4("One of these is my common name"),
        textOutput('common')
      ),
      conditionalPanel(
        condition = "!input.showgenera",
        actionButton('showgenera', 'Show genus names')
      ),
      conditionalPanel(
        condition = "input.showgenera",
        h4("I'm one of these..."),
        uiOutput('genera')
      ),
      uiOutput("previous_guesses"),
      uiOutput("current_guess"),
      uiOutput("endgame"),
      uiOutput("endgame2"),
      uiOutput("new_game_ui"),
      uiOutput("keyboard")
    )
  ),

  # Link to external JS file
  includeScript("www/custom.js")
)

get_inat_obs_nocurl <- function(query = NULL, taxon_name = NULL, taxon_id = NULL,
  place_id = NULL, quality = NULL, geo = NULL, annotation = NULL, year = NULL,
  month = NULL, day = NULL, bounds = NULL, maxresults = 100, meta = FALSE)
{
    # Base URL
    base_url <- "http://www.inaturalist.org/"

    # Construct the query parameters
    params <- list()

    if(!is.null(query)) params$query <- query
    if(!is.null(taxon_name)) params$taxon_name <- taxon_name
    if(!is.null(taxon_id)) params$taxon_id <- taxon_id
    if(!is.null(place_id)) params$place_id <- place_id
    if(!is.null(quality)) {
      if(!quality %in% c("casual", "research")) {
        stop("Invalid quality flag. Use 'casual' or 'research'.")
      }
      params$quality_grade <- quality
    }
    if(!is.null(geo) && geo) params$has <- "geo"
    if(!is.null(annotation)) {
      if(length(annotation) != 2 || !all(grepl("\\d+", annotation))) {
          stop("annotation needs to be a vector of length 2 with numeric IDs.")
      }
      params$term_id <- annotation[1]
      params$term_value_id <- annotation[2]
    }
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

    # Limit maxresults to 10000
    if(maxresults > 10000) stop("maxresults must be <= 10000.")

    # Create URL with query parameters
    search <- paste0(names(params), "=", sapply(as.character(params), URLencode), collapse = "&")

    # Function to perform GET request and handle response
    get_data <- function(url, maxresults) {

      # Fetch results
      q_path <- "observations.csv"
      page_url <- paste0(base_url, q_path, '?', search, "&page=1&per_page=200")
      data_out <- read.csv(page_url, stringsAsFactors = FALSE)

      if(maxresults > 200) {
          for(i in 2:ceiling(maxresults / 200)) {
              page_url <- paste0(base_url, q_path, '?', search, "&page=", i, "&per_page=200")
              data_out <- rbind(data_out, read.csv(page_url, stringsAsFactors = FALSE))
              if(nrow(data_out) <= maxresults) break
          }
      }

      return(data_out)

    }

    # Fetch data
    data_out <- get_data(query_url, maxresults)

    # Return results
    if(meta) {
      return(list(meta = list(found = nrow(data_out), returned = nrow(data_out)),
                  data = data_out))
    } else {
      return(data_out)
    }
}

server <- function(input, output, session) {

  words <- read.table('www/words.txt', sep='\t')
  words[, 1] <- tolower(words[, 1])

  # Reactive Values Initialization
  endExplain            <- reactiveVal(character(0))
  target_word           <- reactiveVal(character(0))
  all_guesses           <- reactiveVal(list())
  started               <- reactiveVal(FALSE)
  error                 <- reactiveVal(FALSE)
  finished              <- reactiveVal(FALSE)
  current_guess_letters <- reactiveVal(character(0))
  current_placelevel    <- reactiveVal(1)
  setplace              <- reactiveVal(matrix(0, nrow = 2, ncol = 2))

  placelevels <- c('continent', 'region', 'country', 'settlement')

  # Observing whether the game has started
  output$started <- reactive({started()})
  outputOptions(output, "started", suspendWhenHidden = FALSE)

  output$error <- reactive({error()})
  outputOptions(output, "error", suspendWhenHidden = FALSE)

  # Function Definitions
  common_names <- function(words_today, obs) {
    out <- unique(unlist(sapply(words_today,function(x) obs$common_name[grepl(x,tolower(obs$scientific_name))])))
    out <- out[out != '']
    return(out)
  }

  try_date <- function(obs) {
    words_today <- unique(sapply(obs$scientific_name, \(x) strsplit(x, ' ')[[1]][1]))
    words_today <- tolower(words_today)
    words_today <- words_today[words_today %in% words[, 1]]
    words_today <- words_today[order(words[match(words_today, words[, 1]), 2], decreasing = TRUE)]

    if(length(words_today) <= 5) stop(simpleError('No observations'))

    weights <- sapply(words_today, \(x) sum(grepl(x, tolower(obs$scientific_name))))
    common <- common_names(words_today, obs)

    list(
      words_today = words_today,
      weights     = weights,
      common      = common,
      obs         = obs
    )
  }

  try_place <- function(placename, placelevel, words) {

    base_url <- "https://nominatim.openstreetmap.org/search"
    query <- paste0("?q=", URLencode(placename), "&format=json&featuretype=", tolower(placelevel))
    full_url <- paste0(base_url, query)

    response <- readLines(url(full_url), warn = FALSE)
    json_response <- paste(response, collapse = "")
    obj <- fromJSON(json_response, simplifyVector=FALSE)

    bn <- as.numeric(obj[[1]]$boundingbox)
    placeBB <- matrix(c(bn[3:4], bn[1:2]), nrow = 2, byrow = TRUE)
    dimnames(placeBB) <- list(c("x", "y"), c("min", "max"))

    if(any(is.na(placeBB))) {

      current_placelevel(placelevels[[current_placelevel() + 1]])
      try_place(input$Place, current_placelevel(), words)

    } else {

      tryCatch({
        obs <- get_inat_obs_nocurl(
          taxon_name = if(input$Taxon == 'anything') NULL else input$Taxon,
          bounds = c(placeBB[2:1, 1], placeBB[2:1, 2]),
          year   = as.numeric(format(Sys.Date() - 1, "%Y")),
          month  = as.numeric(format(Sys.Date() - 1, "%m")),
          day    = as.numeric(format(Sys.Date() - 1, "%d"))
        )
        assemble_game(c(try_date(obs), pretext = paste0('Organisms observed in ', placename, ' yesterday: ')))
        started(TRUE)
        output$started <- reactive({started()})

      }, error = function(e1) {
        tryCatch({
          obs <- get_inat_obs_nocurl(
            taxon_name = if(input$Taxon == 'anything') NULL else input$Taxon,
            bounds = c(placeBB[2:1, 1], placeBB[2:1, 2]),
            month  = as.numeric(format(Sys.Date(), "%m")),
            day    = as.numeric(format(Sys.Date(), "%d"))
          )
          assemble_game(c(try_date(obs), pretext = paste0('Organisms observed in ', placename, ' on this date in all previous years: ')))
          started(TRUE)
          output$started <- reactive({started()})

        }, error = function(e2) {
          tryCatch({
            obs <- get_inat_obs_nocurl(
              taxon_name = if(input$Taxon == 'anything') NULL else input$Taxon,
              bounds = c(placeBB[2:1, 1], placeBB[2:1, 2]),
              month = as.numeric(format(Sys.Date(), "%m"))
            )
            assemble_game(c(try_date(obs), pretext = paste0('Organisms observed in ', placename, ' in this month in all previous years: ')))
            started(TRUE)
            output$started <- reactive({started()})

          }, error = function(e3) {
            tryCatch({
              obs <- get_inat_obs_nocurl(
                taxon_name = if(input$Taxon == 'anything') NULL else input$Taxon,
                bounds = c(placeBB[2:1, 1], placeBB[2:1, 2]))
              assemble_game(c(try_date(obs), pretext = paste0('Organisms ever observed in ', placename, ': ')))
            }, error = function(e4) {error(TRUE); reset_game()})
          })
        })
      })

    }
  }

  assemble_game <- function(placeRes) {
    newtarget <- sample(placeRes$words_today, 1, prob = 1 / (placeRes$weights + 0.1))
    refObs <- grep(newtarget, placeRes$obs$scientific_name, ignore.case = TRUE)

    if(length(refObs) > 1) refObs <- sample(refObs, 1)

    output$iurl <- renderText({
      c('<a href="', placeRes$obs$url[refObs], '" target="_blank"><img src="', placeRes$obs$image_url[refObs], '"></a>')
    })

    output$common <- renderText(paste0(placeRes$pretext, paste(placeRes$common, collapse = ', ')))

    endExplain(paste0(tools::toTitleCase(newtarget), ' is the genus name of the ', placeRes$obs$common_name[refObs]))

    wordsRightLength <- words[nchar(words[, 1]) == nchar(newtarget), 1]
    hamm <- stringdist::stringdist(newtarget, wordsRightLength, method = 'hamming') + 0.01

    notInToday <- !wordsRightLength %in% placeRes$words_today
    closeByHamm <- sample(wordsRightLength[notInToday], min(25, sum(notInToday)), prob = 1 / hamm[notInToday]^4)

    qgram <- stringdist::stringdist(newtarget, wordsRightLength, method = 'qgram', q = 1) + 0.01
    notInEither <- !wordsRightLength %in% c(placeRes$words_today, closeByHamm)
    closeByQ <- sample(wordsRightLength[notInEither], min(25, sum(notInEither)), prob = (hamm[notInEither] / qgram[notInEither])^4)

    notInAny <- !wordsRightLength %in% c(placeRes$words_today, closeByHamm, closeByQ)
    farByQ <- sample(wordsRightLength[notInAny], min(50, sum(notInAny)), prob = qgram[notInAny]^4)

    output$genera <- renderUI(HTML(
      paste('<i>', tools::toTitleCase(sample(c(placeRes$words_today, closeByHamm, closeByQ, farByQ))),  '</i>',
            collapse = ', ')
    ))

    target_word(newtarget)
  }

  reset_game <- function() {
    all_guesses(list())
    current_placelevel(1)
    finished(FALSE)
  }

  # Observing Events
  observeEvent(input$submit, {
    print("Submit button pressed")
    print(paste("Place entered:", input$Place))
    try_place(input$Place, current_placelevel(), words)
  })

  observeEvent(input$new_game, {
    started(FALSE)
    error(FALSE)
    output$started <- reactive({started()})
    output$error <- reactive({error()})
    reset_game()
  })

  # Rendering UI Elements
  output$previous_guesses <- renderUI({
    res <- lapply(all_guesses(), function(guess) {
      letters <- guess$letters
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
    if(!started() || finished()) return()

    letters <- current_guess_letters()
    target_length <- isolate(nchar(target_word()))

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
    if(finished()) {
      actionButton("new_game", "New Game")
    }
  })

  observeEvent(input$new_game, {
    started(FALSE)
    output$started <- reactive({ started() })
    reset_game()
  })

  used_letters <- reactive({
    letter_matches <- list()

    lapply(all_guesses(), function(guess) {
      mapply(guess$letters, guess$matches, SIMPLIFY = FALSE, USE.NAMES = FALSE,
             FUN = function(letter, match) {
               prev_match <- letter_matches[[letter]]
               if(is.null(prev_match) || (match == "correct" && prev_match != "correct") ||
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
      if(!started() || finished()) return()
      cur <- current_guess_letters()
      if(length(cur) < isolate(nchar(target_word()))) {
        current_guess_letters(c(cur, tolower(key)))
      }
    })
  }

  # Add listeners foreach key, except Enter and Back
  lapply(unlist(keys, recursive = FALSE), function(key) {
    if(key %in% c("Enter", "Back")) return()
    observeKeyPress(key)
  })

  observeEvent(input$Back, {
    if(length(current_guess_letters()) > 0) {
      current_guess_letters(current_guess_letters()[-length(current_guess_letters())])
    }
  })

  observeEvent(input$Enter, {
    guess <- paste(current_guess_letters(), collapse = "")

    if(!guess %in% words[, 1]) return()

    all_guesses_new <- all_guesses()
    check_result <- check_word(guess, target_word())
    all_guesses_new[[length(all_guesses_new) + 1]] <- check_result
    all_guesses(all_guesses_new)

    if(isTRUE(check_result$win)) finished(TRUE)

    current_guess_letters(character(0))
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

  output$image <- renderUI({
    htmlOutput('iurl')
  })

  output$endgame <- renderUI({
    if(finished()) {
      renderEndgameUI(all_guesses())
    }
  })

  output$endgame2 <- renderUI({
    if(finished()) {
      div(
        endExplain()
      )
    }
  })
}

check_word <- function(guess_str, target_str) {
  guess <- strsplit(guess_str, "")[[1]]
  target <- strsplit(target_str, "")[[1]]

  if(length(guess) != length(target)) {
    stop("Word lengths don't match.")
  }

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

#shinylive::export('~/scripts/iNatle/', '~/scripts/thecnidaegritty/iNatle/', template_params=list(title='iNatle'))
#httpuv::runStaticServer("~/scripts/thecnidaegritty/iNatle/")
