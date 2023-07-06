library(shiny)
library(htmltools)

words <- read.table('words.txt', sep='\t')
words[,1] <- tolower(words[,1])

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4),
  title = "iNatle: Local Genera",
  tags$style(HTML("
    .container-fluid {
        text-align: center;
        vertical-align: top;
        height: calc(100vh - 30px);
        display: grid;
        grid-template-rows: 1fr auto;
        overflow-y: scroll;
    }
    .guesses {
        overflow-y: scroll;
        height: 100%;
    }
    .guesses.finished {
        overflow-y: scroll;
    }
    .guesses .word {
        margin: 5px;
    }
    .guesses .word > .letter {
        display: inline-block;
        width: 50px;
        height: 50px;
        text-align: center;
        vertical-align: middle;
        border-radius: 3px;
        line-height: 50px;
        font-size: 32px;
        font-weight: bold;
        vertical-align: middle;
        user-select: none;
        color: white;
        font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
    }
    .guesses .word > .correct {
        background-color: #6a5;
    }
    .guesses .word > .in-word {
        background-color: #db5;
    }
    .guesses .word > .not-in-word {
        background-color: #888;
    }
    .guesses .word > .guess {
        color: black;
        background-color: white;
        border: 1px solid black;
    }
    .keyboard {
        height: 240px;
        user-select: none;
    }
    .keyboard .keyboard-row {
        margin: 3px;
    }
    .keyboard .keyboard-row .key {
        display: inline-block;
        padding: 0;
        width: 30px;
        height: 50px;
        text-align: center;
        vertical-align: middle;
        border-radius: 3px;
        line-height: 50px;
        font-size: 18px;
        font-weight: bold;
        vertical-align: middle;
        color: black;
        font-family: 'Clear Sans', 'Helvetica Neue', Arial, sans-serif;
        background-color: #ddd;
        touch-action: none;
    }
    .keyboard .keyboard-row .key:focus {
        outline: none;
    }
    .keyboard .keyboard-row .key.wide-key {
        font-size: 15px;
        width: 50px;
    }
    .keyboard .keyboard-row .key.correct {
        background-color: #6a5;
        color: white;
    }
    .keyboard .keyboard-row .key.in-word {
        background-color: #db5;
        color: white;
    }
    .keyboard .keyboard-row .key.not-in-word {
        background-color: #888;
        color: white;
    }
    .endgame-content {
        font-family: Helvetica, Arial, sans-serif;
        display: inline-block;
        line-height: 1.4;
        letter-spacing: .2em;
        margin: 20px 8px;
        width: fit-content;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 4px 4px 19px rgb(0 0 0 / 17%);
    }
  ")),
  div(
    class = "guesses",
    h2("iNatle"),
    conditionalPanel(
      condition = "!output.started",
      textInput('Place', h3('Enter a place name'), value='Oregon', width='100%'),
      actionButton('submit','Submit')
    ),
    conditionalPanel(
      condition = "output.started",
      h3("The target genus is drawn from these local organisms"),
      textOutput('common'),
      h3("And is equal to one of these genera"),
      textOutput('genera'),
      uiOutput("previous_guesses"),
      uiOutput("current_guess"),
      uiOutput("endgame"),
      uiOutput("endgame2"),
      uiOutput("new_game_ui"),
      uiOutput("keyboard")
    )
  ),
  # div(
  #   style="display: inline-block;",
  #   checkboxInput("hard", "Hard mode")
  # ),
  tags$script(HTML("
    const letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
    const all_key_ids = [ ...letters, 'Enter', 'Back'];
    document.addEventListener('keydown', function(e) {
      let key = e.code.replace(/^Key/, '');
      if (letters.includes(key)) {
        document.getElementById(key).click();
      } else if (key == 'Enter') {
        document.getElementById('Enter').click();
      } else if (key == 'Backspace') {
        document.getElementById('Back').click();
      }
    });

    // For better responsiveness on touch devices, trigger a click on the button
    // when a touchstart event occurs; don't wait for the touchend event. So
    // that a click event doesn't happen when the touchend event happens (and
    // cause the letter to be typed a second time), we set the 'pointer-events'
    // CSS property to 'none' on the button. Then when there's _any_ touchend
    // event, unset the 'pointer-events' CSS property on all of the buttons, so
    // that the button can be touched again.
    let in_button_touch = false;
    document.addEventListener('touchstart', function(e) {
        if (all_key_ids.includes(e.target.id)) {
            e.target.click();
            e.target.style.pointerEvents = 'none';
            e.preventDefault();   // Disable text selection
            in_button_touch = true;
        }
    });
    document.addEventListener('touchend', function(e) {
        all_key_ids.map((id) => {
            document.getElementById(id).style.pointerEvents = null;
        });
        if (in_button_touch) {
            if (all_key_ids.includes(e.target.id)) {
                // Disable text selection and triggering of click event.
                e.preventDefault();
            }
            in_button_touch = false;
        }
    });
  "))
)


server <- function(input, output) {

  endExplain <- reactiveVal(character(0))
  target_word <- reactiveVal(character(0))
  all_guesses <- reactiveVal(list())
  started <- reactiveVal(FALSE)
  output$started <- reactive({started()})
  outputOptions(output, "started", suspendWhenHidden = FALSE)
  finished <- reactiveVal(FALSE)
  current_guess_letters <- reactiveVal(character(0))

  common_names <- function(words_today, obs) {
    out <- unique(unlist(sapply(words_today,function(x) obs$common_name[grepl(x,tolower(obs$scientific_name))])))
    out <- out[out != '']
    return(out)
  }

  try_date <- function(obs) {

      words_today <- unique(sapply(obs$scientific_name, \(x) strsplit(x,' ')[[1]][1]))
      words_today <- tolower(words_today)
      words_today <- words_today[words_today %in% words[,1]]
      words_today <- words_today[order(words[match(words_today, words[,1]),2], decreasing = TRUE)]

      if(length(words_today) <= 5) simpleError()

      weights <- sapply(words_today, \(x) sum(grepl(x, tolower(obs$scientific_name))))
      common <- common_names(words_today, obs)

      return(list(words_today = words_today,
                  weights     = weights,
                  common      = common,
                  obs         = obs))

  }

  ##might want to add an option to draw a boundary using leaflet
  try_place <- function(placename, placelevel, words) {

    placeBB <- osmdata::getbb(placename, featuretype = placelevel)
    if(any(is.na(placeBB))) simpleError()

    tryCatch({

      obs <- rinat::get_inat_obs(bounds = c(placeBB[2:1,1], placeBB[2:1,2]),
                                 year   = as.numeric(format(Sys.Date() - 1, "%Y")),
                                 month  = as.numeric(format(Sys.Date() - 1, "%m")),
                                 day    = as.numeric(format(Sys.Date() - 1, "%d")))

      return(c(try_date(obs), pretext = paste0('Organisms observed in ', placename, ' yesterday: ')))

    },
      error = function(e1) {
        tryCatch({

          obs <- rinat::get_inat_obs(bounds = c(placeBB[2:1,1], placeBB[2:1,2]),
                                     month  = as.numeric(format(Sys.Date(), "%m")),
                                     day    = as.numeric(format(Sys.Date(), "%d")))

          return(c(try_date(obs), pretext = paste0('Organisms observed in ', placename, ' on this date in all previous years: ')))

        },
        error = function(e2) {
          tryCatch({

            obs <- rinat::get_inat_obs(bounds = c(placeBB[2:1,1], placeBB[2:1,2]),
                                       month = as.numeric(format(Sys.Date(), "%m")))

            return(c(try_date(obs), pretext = paste0('Organisms observed in ', placename, ' in this month in all previous years: ')))

          },
          error=function(e3) {

            obs <- rinat::get_inat_obs(bounds = c(placeBB[2:1,1], placeBB[2:1,2]))


            return(c(try_date(obs), pretext = paste0('Organisms ever observed in ', placename, ': ')))

          }
          )
        }
        )
      }
    )
  }

  get_place <- function() {

    output$common <- renderText('Getting organisms...')

    placeRes <- tryCatch(try_place(input$Place, 'continent', words),
      error = function(e1) {
        tryCatch(try_place(input$Place, 'region', words),
          error = function(e2) {
            tryCatch(try_place(input$Place, 'country', words),
               error = function(e2) {
                 try_place(input$Place, 'settlement', words)
               }
            )
          }
        )
      }
    )

    newtarget <- sample(placeRes$words_today, 1, prob = 1 / (placeRes$weights + 0.1))

    refObs <- grep(newtarget, placeRes$obs$scientific_name, ignore.case = TRUE)

    if(length(refObs) > 1) refObs <- sample(refObs,1)

    output$iurl <- renderText({c('<a href="', placeRes$obs$url[refObs], '" target="_blank"><img src="', placeRes$obs$image_url[refObs], '"></a>')})
    output$common <- renderText(paste0(placeRes$pretext, paste(placeRes$common, collapse = ', ')))

    endExplain(paste0(paste0(tools::toTitleCase(newtarget), ' is the genus name of the '), placeRes$obs$common_name[refObs]))

    wordsRightLength <- words[nchar(words[,1]) == nchar(newtarget),1]

    hamm <- stringdist::stringdist(newtarget,wordsRightLength,method='hamming') + 0.01
    notInToday <- !wordsRightLength %in% placeRes$words_today
    closeByHamm <- sample(wordsRightLength[notInToday], min(25,sum(notInToday)), prob = 1 / hamm[notInToday]^4)

    qgram <- stringdist::stringdist(newtarget, wordsRightLength, method = 'qgram', q = 1) + 0.01
    notInEither <- !wordsRightLength %in% c(placeRes$words_today,closeByHamm)
    closeByQ <- sample(wordsRightLength[notInEither], min(25,sum(notInEither)), prob = (hamm[notInEither] / qgram[notInEither])^4)

    notInAny <- !wordsRightLength %in% c(placeRes$words_today, closeByHamm, closeByQ)
    farByQ <- sample(wordsRightLength[notInAny], min(50,sum(notInAny)), prob = qgram[notInAny]^4)

    output$genera <- renderText(paste0('Genera of above, plus a random sample of global genera: ',
                                paste(tools::toTitleCase(sample(c(placeRes$words_today, closeByHamm, closeByQ, farByQ))),
                                      collapse = ', ')))
    target_word(newtarget)

  }

  reset_game <- function() {
    get_place()
    all_guesses(list())
    finished(FALSE)
  }

  observeEvent(input$submit, {
      get_place()
      started(TRUE)
      output$started <- reactive({started()})
  })

  observeEvent(input$Enter, {
    guess <- paste(current_guess_letters(), collapse = "")

    if (! guess %in% words[,1])
      return()

    # if (input$hard) {
    # # Letters in the target word that the player has previously
    # # guessed correctly.
    # matched_letters = used_letters().intersection(set(target_word()))
    # if not set(guess).issuperset(matched_letters):
    #     return
    # }

    all_guesses_new <- all_guesses()

    check_result <- check_word(guess, target_word())
    all_guesses_new[[length(all_guesses_new) + 1]] <- check_result
    all_guesses(all_guesses_new)

    if (isTRUE(check_result$win)) {
        finished(TRUE)
    }

    current_guess_letters(character(0))
  })

  output$previous_guesses <- renderUI({
    res <- lapply(all_guesses(), function(guess) {
      letters <- guess$letters
      row <- mapply(
        letters,
        guess$matches,
        FUN = function(letter, match) {
          # This will have the value "correct", "in-word", or "not-in-word", and
          # those values are also used as CSS class names.
          match_type <- match
          div(toupper(letter), class = paste("letter", match_type))
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      div(class = "word", row)
    })

    scroll_js <- "
        document.querySelector('.guesses')
          .scrollTo(0, document.querySelector('.guesses').scrollHeight);
    "
    tagList(res, tags$script(HTML(scroll_js)))
  })

  output$current_guess <- renderUI({
    if (!started()) return()
    if (finished()) return()

    letters <- current_guess_letters()

    # Fill in blanks for letters up to length of target word. If letters is:
    #   "a" "r"
    # then result is:
    #   "a" "r" "" "" ""
    target_length <- isolate(nchar(target_word()))
    if (length(letters) < target_length) {
      letters[(length(letters)+1) : target_length] <- ""
    }

    div(
      class = "word",
      lapply(letters, function(letter) {
        div(toupper(letter), class ="letter guess")
      })
    )
  })

  output$new_game_ui <- renderUI({
    if (!finished())
      return()
    actionButton("new_game", "New Game")
  })

  observeEvent(input$new_game, {
    reset_game()
  })

  used_letters <- reactive({
    # This is a named list. The structure will be something like:
    # list(p = "not-in-word", a = "in-word", e = "correct")
    letter_matches <- list()

    # Populate `letter_matches` by iterating over all letters in all the guesses.
    lapply(all_guesses(), function(guess) {
      mapply(guess$letters, guess$matches, SIMPLIFY = FALSE, USE.NAMES = FALSE,
        FUN = function(letter, match) {
          prev_match <- letter_matches[[letter]]
          if (is.null(prev_match)) {
            # If there isn't an existing entry for that letter, just use it.
            letter_matches[[letter]] <<- match
          } else {
            # If an entry is already present, it can be "upgraded":
            # "not-in-word" < "in-word" < "correct"
            if (match == "correct" && prev_match %in% c("not-in-word", "in-word")) {
              letter_matches[[letter]] <<- match
            } else if (match == "in-word" && prev_match == "not-in-word") {
              letter_matches[[letter]] <<- match
            }
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

  output$keyboard <- renderUI({
    prev_match_type <- used_letters()
    keyboard <- lapply(keys, function(row) {
      row_keys <- lapply(row, function(key) {
        class <- "key"
        key_lower <- tolower(key)
        if (!is.null(prev_match_type[[key_lower]])) {
          class <- c(class, prev_match_type[[key_lower]])
        }
        if (key %in% c("Enter", "Back")) {
          class <- c(class, "wide-key")
        }
        actionButton(key, key, class = class)
      })
      div(class = "keyboard-row", row_keys)
    })

    div(class = "keyboard", keyboard)
  })

  # Add listeners for each key, except Enter and Back
  lapply(unlist(keys, recursive = FALSE), function(key) {
    if (key %in% c("Enter", "Back")) return()
    observeEvent(input[[key]], {
      if (!started()) return()
      if (finished()) return()
      cur <- current_guess_letters()
      if (length(cur) >= isolate(nchar(target_word())))
        return()
      current_guess_letters(c(cur, tolower(key)))
    })
  })

  observeEvent(input$Back, {
    if (length(current_guess_letters()) > 0) {
      current_guess_letters(current_guess_letters()[-length(current_guess_letters())])
    }
  })


  output$endgame <- renderUI({
    if (!finished())
      return()

    lines <- lapply(all_guesses(), function(guess) {
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
  })

  output$endgame2 <- renderUI({
    if (!finished())
      return()
    div(
      endExplain(),
      htmlOutput('iurl')
    )
  })
}

check_word <- function(guess_str, target_str) {
  guess <- strsplit(guess_str, "")[[1]]
  target <- strsplit(target_str, "")[[1]]
  remaining <- character(0)

  if (length(guess) != length(target)) {
    stop("Word lengths don't match.")
  }

  result <- rep("not-in-word", length(guess))

  # First pass: find matches in correct position. Letters in the target that do
  # not match the guess are added to the remaining list.
  for (i in seq_along(guess)) {
    if (guess[i] == target[i]) {
      result[i] <- "correct"
    } else {
      remaining <- c(remaining, target[i])
    }
  }

  for (i in seq_along(guess)) {
    if (guess[i] != target[i] && guess[i] %in% remaining) {
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
