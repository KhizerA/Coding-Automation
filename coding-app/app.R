### Coding Application ###
### Purpose: Automation for coding qualitative survey responses 
### Input: Survey data (.csv or .xlsx)
### Output: Coded .xlsx file

## Setup ##
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(sortable)
library(bslib)
library(tidyverse)
library(tidytext)
library(openxlsx)
library(fuzzyjoin)
library(readxl)
options(shiny.maxRequestSize = 10 * 1024^2) #Can be flexible to upload bigger files

## UI Function ##
ui <- page_navbar(
  id = 'app',
  title = tags$b('Coding Dashboard'),
  theme = bs_theme(bootswatch = 'journal'), 
  #Sidebar
  sidebar = sidebar(
    open = 'always',
    width = 400,
    useShinyjs(),
    # Reset app button
    layout_column_wrap(actionBttn(inputId = 'reset', label = 'Reset', style = 'material-flat', 
                                  color = 'danger', size = 'sm'), width = 1/3),
    # Upload folder
    fileInput('file', 'Choose file to upload (.csv or .xlsx)',
              accept = c('.csv', '.xlsx')),
    uiOutput('variables') #Placeholder for rendering in server
  ),
  #Main body
  #First page of app
    nav_panel_hidden(
      value = 'page1',
      uiOutput('boxes'), #Placeholder for rendering in server
      layout_columns(col_widths = c(1,1,-7,3),
        shinyjs::hidden(actionBttn(inputId = 'add_btn', label = 'Add', icon = icon('plus'),
                                   style = "material-circle", color = 'success')), #To add codebook
        shinyjs::hidden(actionBttn(inputId = 'rm_btn', label = 'Remove', icon = icon('minus'),
                   style = "material-circle", color = 'danger')), #To remove codebook
        shinyjs::hidden(actionBttn(inputId = 'code', label = 'Code', icon = icon('chevron-right'),
                                   style = 'gradient', color = 'primary'))) #To execute program
      ),
  #Second page
    nav_panel_hidden(
      value = 'page2',
      shinyjs::hidden(downloadBttn(outputId = 'download', label = 'Download Data', 
                   style = 'gradient')), #Download final data button
      #Card tabs for printing frequency tables
      navset_card_tab(
        id = 'p2_tabs',
        title = tags$b('Data Summary')
        )
    )
)

## Server Function ##
server <- function(input, output, session) { 
  # Initializing of reset button
  observeEvent(input$reset, {session$reload()})
  # Codebook counter variable to be used later
  counter <- reactiveValues(n = 1)
  # Will execute following file upload
  observeEvent(input$file, {
    #Reading in file
    ext <- tools::file_ext(input$file$name)
    upload_file <- switch(ext,
                          csv = read_csv(input$file$datapath, 
                                         show_col_types = FALSE,
                                         locale = locale(encoding = "UTF-8")),
                          xlsx = read_xlsx(input$file$datapath,
                                           sheet = 1),
                          validate('Invalid file, please upload a .csv or .xlsx file'))
    #Creating a sortable list of all variable names
    vars <- names(upload_file)
    master_list <- rank_list(input_id = 'vars',
                             text = 'Variables',
                             labels = vars, 
                             options = sortable_options(sort = FALSE,
                                                        group = "mygroup"))
    output$variables <- renderUI({master_list})
    #Loading in add and remove buttons  
    shinyjs::show('add_btn')
    shinyjs::show('rm_btn')
    
    ## Codebook manipulation functions
    # function to save prior inputs to work around reactive resetting
    AllInputs <- reactive({
      x <- reactiveValuesToList(input)
    })
    #Add codebook button functionality 
    observeEvent(input$add_btn, {
      counter$n <- counter$n + 1})
    #Remove codebook button functionality 
    observeEvent(input$rm_btn, {
      if (counter$n >1) counter$n <- counter$n -1
    })
    # Reactive function to dynamically create codebook parameters  
    num_codebooks <- reactive({
      n <- counter$n
      if (n > 0) { 
          lapply(seq_len(n), function(i) { 
            isolate({ #workaround to stop reactive resetting of inputs
              card(card_header(tags$b(paste('Codebook', i))),
                   layout_columns(
                     col_widths = c(8,4), #splitting in to two columns
                     rank_list(input_id = paste0('cb', i),
                               labels = AllInputs()[[paste0('cb', i)]], #Saving options that are dragged in
                               options = (sortable_options(group = "mygroup",
                                                            sort = FALSE))),
                     card_body(
                      numericInput(paste0('num_codes', i), 'Minimum number of mentions', 3,
                                   min = 1, max = 100, step = 1), #Input for defining minimum number of codes
                      numericInputIcon(paste0('param', i), 'Match strength', 90,
                                   min = 1, max = 100, step = 1, icon = icon('percent'))))) #Input for match strength
            })
          })
      }
    })
    output$boxes <- renderUI({
      num_codebooks()
      })
    })

  #Function to make sure the execute button doesn't appear before options are all populated
  observeEvent({
    lapply(seq_len(counter$n), function(i) {input[[paste0('cb', i)]]}) #Calling all codebook input fields
  }, {
    eval <- lapply(
      seq_len(counter$n), function(i) {!is_empty(input[[paste0('cb', i)]])}) #Making sure none of them are mepty
    #If non-empty, display the execute button
    if (all(eval == TRUE)) {
      shinyjs::show('code')} else {
        shinyjs::hide('code')}
  })
  
  #Program execution
  observeEvent(input$code, { 
    #Pulling up the loading screen
    showModal(modalDialog('Loading...', footer = NULL))
    #Going to the next page
    nav_select(id = 'app', selected = 'page2')
    #Re-reading the file for this chunk
    ext <- tools::file_ext(input$file$name)
    upload_file <- switch(
      ext,
      csv = read_csv(input$file$datapath, 
                     show_col_types = FALSE, 
                     locale = locale(encoding = "UTF-8")),
      xlsx = read_xlsx(input$file$datapath,
                       sheet = 1))
    #Pre-defining output variables before loop
    output_data <- upload_file %>%
      rename(any_of(c(id = "ID"))) %>%
      mutate(ID = seq_len(nrow(.)))
    output_codebook <- data.frame()
    frequency_tables <- list()
    highlight_vars <- c()
    ## The big one
    # Looping through each variable grouping to execute coding
    for (cbs in 1:counter$n) {
      input_ref <- input[[paste0('cb',cbs)]] #selected variables
      data <- output_data %>%
        select(ID, input_ref) %>%
        mutate_at(input_ref, as.character)
      old_names <- names(data)
      #Part 1: Producing a codebook
      pivoted <- data %>%
        pivot_longer(-ID) %>% #Pivoting to long for ease of summation
        filter(value != "")
      min_mentions <- input[[paste0('num_codes',cbs)]] #user inputed minumum number of mentions
      #Creating a df of undesirable stop words
      my_stopwords <- stop_words %>% 
        filter(lexicon == 'snowball')
      #A function to detect n-grams comprised solely of stop words
      detect_stopwords_ngram <- function(ngram_col) {
        result <- c()
        for (x in ngram_col) { 
          temp <- str_split(x, " ") %>%
            unlist()
          result <- result %>%
            append(!all(temp %in% my_stopwords$word))
        }
        result
      }
      #Producing a df of ngrams and filtering out stop words
      ngrams <- pivoted %>%
        unnest_ngrams(output = word, input = value, n_min = 1) %>%
        anti_join(stop_words, by = "word") %>%
        filter(detect_stopwords_ngram(word)) 
      freq_table <- ngrams %>% #frequency table of n_grams
        count(word, sort = T) %>%
        filter(n >= min_mentions) %>%
        na.omit(word)
      adj_table <- freq_table
      #Adjusting the frequency table by striking down the frequency of sub-grams that appear in n-grams
      for (x in unique(ngrams$ID)) {
        temp <- ngrams %>%
          filter(ID == x)
        for (i in 1:nrow(freq_table)) {
          head_bool <- str_detect(freq_table$word, paste0(' ', freq_table$word[i])) %>% as.integer()
          tail_bool <- str_detect(freq_table$word, paste0(freq_table$word[i], ' ')) %>% as.integer()
          bool_sum <- head_bool + tail_bool
          bool_sum <- ifelse(bool_sum > 0, TRUE, FALSE)
          phrases <- freq_table$word[bool_sum]

          if (any(phrases %in% temp$word)) {
            adj_table$n[i] <- adj_table$n[i] - 1
          }
        }
      }
      #Adjusted table for n-gram frequency 
      adj_table <- adj_table %>%
        filter(n >= min_mentions) %>%
        arrange(desc(n)) %>%
        rename(Response = word,
               Frequency = n) 
      #Defining the codebook
      codebook <- data.frame(
        Response = adj_table$Response,
        Code = c(1:nrow(adj_table)))
      #Part 2: coding execution
      #fuzzy matching n-gram data with codebook
      fuzzy_ngrams <- ngrams %>%
        stringdist_join(
          codebook,
          by = c(word = 'Response'),
          mode = 'left',
          method = 'jw', 
          max_dist = 1 - (0.01 * input[[paste0('param',cbs)]]), #Using the user inputted match strength
          ignore_case = TRUE,
          distance_col = 'distance') %>%
        distinct(ID, Code, .keep_all = TRUE) %>% #Keeping one occurrence of a code per person
        arrange(distance) %>%
        distinct(ID, word, .keep_all = TRUE) #Keeping one occurrence of an n-gram per person
      fuzzy_ngrams <- fuzzy_ngrams %>%
        drop_na(Code) %>%
        arrange(ID, name) %>%
        mutate(title = NA)
      #Loop to populate the question code number (for variable name)
      x <- 0
      for (i in 1:nrow(fuzzy_ngrams)) {
        prior <- fuzzy_ngrams %>%
          select(ID, name) %>%
          slice(i-1)
        current <- fuzzy_ngrams %>%
          select(ID, name) %>%
          slice(i)
        fuzzy_ngrams$ID[i-1]
        if (identical(prior, current)) {
          x <- x + 1
        } else {
          x <- 1}
        fuzzy_ngrams$title[i] <- paste0(fuzzy_ngrams$name[i], '_', 'code', x)
      }
      #Dataframe with coded data
      df <- fuzzy_ngrams %>%
        select(ID, word, Code, title) %>%
        pivot_wider(id_cols = ID, names_from = title, values_from = Code) %>%
        select(gtools::mixedsort(colnames(.)))
      ## Part 3: merges
      #merging codebooks in to master
      coded_var_names <- old_names[!old_names %in% 'ID']
      codebook <- codebook %>%
        mutate(Response = str_to_sentence(Response)) 
      names(codebook)[1] <- coded_var_names %>%
        paste(collapse = " ") %>%
        paste0("_", "Response")
      names(codebook)[2] <- coded_var_names %>%
        paste(collapse = " ") %>%
        paste0("_", "Code")
      output_codebook <- output_codebook %>%
        rownames_to_column() %>%
        full_join(codebook %>% rownames_to_column, by = "rowname") %>%
        select(-rowname)
      #merging survey data to master
      output_data <- output_data %>%
        left_join(df, by = 'ID') %>%
        relocate(starts_with(coded_var_names), .after = coded_var_names) #moving codes next to their respective question
      #Frequency tables for printing
      table_to_add <- fuzzy_ngrams %>%
        count(Response) %>%
        rename(Frequency = n) %>%
        mutate(Response = str_to_sentence(Response), 
               Frequency = as.integer(Frequency)) %>%
        arrange(desc(Frequency))
      frequency_tables <- frequency_tables %>%
        append(list(table_to_add)) 
      #Variables to highlight in excel output 
      highlight_vars <- highlight_vars %>%
        append(names(df)) %>%
        str_subset('ID', negate = TRUE) #keeping all variables except ID 
    }
    ##Coding done
    
    #function to print out frequency tables for review
    lapply(seq_len(length(frequency_tables)), function(i) {
      nav_insert(id = 'p2_tabs', select = TRUE,
                 nav_panel(title = paste0("Codebook", " ", i), 
                           layout_columns(
                             col_widths = c(-1, 10, -1),
                             renderTable(frequency_tables[[i]],
                                         striped = T, bordered = T, hover = T,
                                         spacing = 'm'))))
      })
    
    ##Preparing data for export
    output_data <- output_data %>%
      select(-ID) 
    # Hotfix I don't understand to prevent encoding errors
    is_chr <- vapply(output_data, inherits, what = "character", NA)
    output_data[is_chr] <- lapply(output_data[is_chr], iconv, to = "UTF-8")
    #Formating openxlsx workbook for output
    output_file <- createWorkbook()
    addWorksheet(output_file, 'Survey')
    addWorksheet(output_file, 'Codebook')
    writeData(output_file, 'Survey', output_data)
    writeData(output_file, 'Codebook', output_codebook)
    highlight <- createStyle(
      fontColour = "#000000",
      fgFill = "yellow")
    addStyle(output_file,
             sheet = 'Survey',
             style = highlight,
             cols = match(highlight_vars, names(output_data)), #all variables to highlight
             rows = 1:nrow(output_data),
             gridExpand = TRUE)
    #Download button functionality 
    output$download <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$file$name), "_coded.xlsx")
      },
      content = function(file) {
        saveWorkbook(output_file, file, overwrite = TRUE)
      }
    )
    shinyjs::show('download_bttn')
    # Remove loading screen
    removeModal() 
    })
  }

shinyApp(ui, server)

## References ##
#https://stackoverflow.com/questions/31454185/how-to-add-remove-input-fields-dynamically-by-a-button-in-shiny
#https://stackoverflow.com/questions/39276632/shiny-r-how-to-disable-checkboxgroup-input-after-it-is-clicked#:~:text=You%20can%20use%20the%20shinyjs,useShinyjs%20in%20your%20app's%20ui.
#https://mastering-shiny.org/action-transfer.html#upload
#https://rstudio.github.io/bslib/articles/dashboards/index.html
#https://shinyapps.dreamrs.fr/shinyWidgets/
