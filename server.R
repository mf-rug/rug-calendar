knownGoodValue <- " 22nd Century Economy (300) - CFBGR02705"
knownGoodValue2 <- " Current Topics in Microeconomics - EBM228A05"
knownBadValue <- " academic advisement y2"

encodeForURL <- function(string) {
  # Replace '+' with '%2B' first
  string <- gsub("\\+", "%2B", string, fixed = TRUE)
  
  # Then use URLencode for the rest
  URLencode(string, reserved = TRUE)
}


is_valid <- function(x) {
  # Check if x is NULL
  if (is.null(x)) {
    return(FALSE)
  }
  
  # If x is a vector, check each element
  if (length(x) > 1) {
    return(all(x != "" & !is.na(x)))
  }
  
  # For a single element, check directly
  return(x != "" & !is.na(x))
}

# Function to check for all NAs
is_all_na <- function(x) {
  all(is.na(x))
}

# Function to check for all empty strings
is_all_empty <- function(x) {
  if (is.character(x)) {
    all(x %in% c('', ' ', ',', 'UNKNOWN', 'OTHER', 'Unknown'))
  } else {
    FALSE
  }
}

is_all_emptylist <- function(x) {
  all(length(x[[1]]) == 0)
} 

# Function to unnest a column if it exists
unnest_if_exists <- function(df, col) {
  if (col %in% names(df)) {
    df %>% unnest(cols = all_of(col), names_sep = ';', keep_empty = TRUE)
  } else {
    df
  }
}

get_colors_hcl <- function(n, start=40, luminance=85, shuffle=TRUE) {
  # Generate colors in HCL space
  # Adjusting chroma and luminance for pastel tones
  h <- seq(start, start + 360, length.out = n + 1)
  c <- rep(35, n)  # Lower chroma for softer tones
  l <- rep(luminance)  # Higher luminance for lighter tones
  colors <- hcl(h, c, l)
  if (shuffle) {
    colors <- colors[sample(1:n, n)]
  }
  colors[1:n]
}

get_academic_year <- function() {
  current_date <- Sys.Date()
  year <- as.integer(format(current_date, "%Y"))
  month <- as.integer(format(current_date, "%m"))
  
  # Academic year starts in September
  if (month >= 9) {
    return(paste(year, year + 1, sep = "-"))
  } else {
    return(paste(year - 1, year, sep = "-"))
  }
}

pull_courses <- function(year) {
  course.df <- fromJSON(content(GET(paste0('https://rooster.rug.nl/api/course/', year)), "text", encoding = "UTF-8")) %>% unnest(cols = c(), keep_empty = TRUE, names_sep = '_') %>% as.data.frame()
  course.df$name$nl <- str_extract(course.df$name$nl, '[^\\s].*$')
  course.df <- course.df[!is.na(course.df$name$en) & !is.na(course.df$name$nl),]
  course.df$name_code <- paste0(course.df$name$nl, ' - ', course.df$code)
  courses <- course.df$name_code[course.df$name$nl != ''] %>% unlist() %>% as.character()
  list(courses, course.df)
}

pull_progs <- function(year) {
  progs.df <- fromJSON(content(GET(paste0('https://rooster.rug.nl/api/programme/', year)), "text", encoding = "UTF-8")) %>% unnest(cols = c(), keep_empty = TRUE, names_sep = '_') %>% as.data.frame()
  progs.df$name$nl <- str_extract(progs.df$name$nl, '[^\\s].*$')
  progs.df <- progs.df[!is.na(progs.df$name$en) & !is.na(progs.df$name$nl),]
  progs.df$name_code <- paste0(progs.df$name$nl, ' - ', progs.df$code)
  progs <- progs.df$name_code[progs.df$name$nl != ''] %>% unlist() %>% as.character()
  list(progs, progs.df)
}

parse_course_df <- function(input, all_course_codes, year, course_names, cur_sepsel) {
  full_df <- data.frame()
  for (i in seq_along(all_course_codes)) {
    course <- all_course_codes[i]
    if (input$output == 'Courses') {
      url <- paste0('https://rooster.rug.nl/api/v2/', year, '/activity/by/course/', course)
    } else {
      url <- paste0('https://rooster.rug.nl/api/v2/', year, '/activity/by/programme/', URLencode(course), '?onlyPublished=true')
    }
    if (is.na(course) || is.na(year)) {
      full_df <- data.frame(id = 0, course = input$courses[i], error = '<i><font style="color:red">invalid course</font></i>')
      next
    } else {
      response <- GET(url)
      
      # Check if the request was successful
      if (http_status(response)$category == "Success") {
        # Extract content from the response
        content_data <- content(response, "text", encoding = "UTF-8")
        
        if (content_data ==  '[]') {
          if (!exists('full_df')) {
            full_df <- data.frame()
          }
          full_df <- bind_rows(full_df, data.frame(id = 0, course = input$courses[i], error = paste0('<i><font style="color:red">no activities (no data from </font>',
                                                                                             '<a href="', url, '" target="_blank">this url</a></i><i><font style="color:red">)</font></i>')))
          next
        }
        # Convert JSON content to a data.frame
        df <- fromJSON(content_data)
        
        df <- df[, !(sapply(df, is_all_empty) | sapply(df, is_all_emptylist))]
        
        df <- reduce(.x = c("courses", "name", "activityType", "activityType;name", "locationUnits", "staff", "staff;name", "locationUnits;name"), 
                     .f = unnest_if_exists, 
                     .init = df)
        
        # Apply the functions to each column and subset the data frame
        df <- df[, !(sapply(df, is_all_empty) | sapply(df, is_all_emptylist))]
        df <-  df[!duplicated(as.list(df))]
        
        if ("locationUnits;url" %in% colnames(df)) {
          df$location <- paste('<a href="', df$`locationUnits;url`, '" target="_blank">', df$`locationUnits;name;nl`, '</a>', sep = "")
        }
        
        df <- df %>%
          mutate(start = as.character(start)) %>% 
          mutate(end = as.character(end))
        
        df <- df %>%
          mutate(date = format(ymd_hm(str_replace_all(start, ",", " ")), "%d %b %Y")) %>%
          mutate(`start time` = format(ymd_hm(str_replace_all(start, ",", " ")), "%H:%M")) %>%
          mutate(`end time` = format(ymd_hm(str_replace_all(end, ",", " ")), "%H:%M"))
        
        old_names <- c('activityType;name;en', 'recordingMode', 'staff;name;en', 'locationUnits;capacity', 'courses;code', 'courses;facultyCode', 'staff;concept', 'plannedSize', 'name;nl', 'courses;concept', 'academicYear')
        new_names <- c('type',                'recording',      'staff name',    'capacity',                'course code',   'faculty',             'staff concept',  'size',        'info',    'course concept', 'academic year')
        colnames(df) <- stringi::stri_replace_all_fixed(colnames(df), old_names, new_names, vectorize_all = FALSE)
        df$course <- course_names[i]
        df$info <- df$info %>% str_remove(., '^ | $') %>% stringr::str_squish()
        df$info <- str_remove(df$info, fixed(df$course))
        df <- df[, !(sapply(df, is_all_empty))]
        
        df$`course code` <- paste0('<a href="https://ocasys.rug.nl/', df$`academic year`, '/catalog/course/', df$`course code`, '" target="_blank">', df$`course code`, '</a>', sep = "")
        df$day <- df$date %>% as.Date(format = '%d %b %Y') %>% weekdays(abbreviate = TRUE)
        df$week <- strftime(as.Date(df$date, format = '%d %b %Y'), "%V") %>% as.numeric()
        
        df <- df[, !(sapply(df, is_all_na))]
        
        df <- df[, !(names(df) %in% c('id', 'hostKey', 'activityIndex', 'staff;code', 'courses;link', 'staff;facultyCode', 'activityType;name;nl', 'activityType;info', 'activityType;course', 'activityName', 'activityType;syllabusName', 'capacity', 'locationUnits;code', 'locationUnits;info', 'locationUnits;course', 'locationUnits;allocation', "locationUnits;name;en", "locationUnits;address", "locationUnits;seats", "locationUnits;hidden", 'activityTypeCategory', 'locationUnits;avoidConcurrency', 'locationUnits;avoidConcurrency;name', 'locationUnits;name;nl', 'locationUnits;url', 'courses;name'))]
        df$id <- rownames(df) %>% as.numeric()
        
        courseInfo <- grep(x = colnames(df), pattern = 'info', value = T)
        
        
        # Create a temporary DataFrame excluding the column to ignore
        temp_df <- df[, !(names(df) %in% c('id', 'course code'))]
        
        # Find duplicated rows (excluding the first occurrence)
        duplicated_rows <- duplicated(temp_df)
        
        # Remove duplicated rows from the original DataFrame
        df <- df[!duplicated_rows, ]
        
        if (input$unique) {
          # Create a temporary DataFrame excluding the column to ignore
          temp_df <- df[, !(names(df) %in% c('id', 'location', 'course code', 'size'))]
          
          # Find duplicated rows (excluding the first occurrence)
          duplicated_rows <- duplicated(temp_df)
          
          rle_vec <- rle(duplicated_rows)
          rle_vec <- rep(rle_vec$lengths, times=rle_vec$lengths)
          rle_vec[duplicated_rows == FALSE] <- 0
          rle_vec[which(rle_vec != 0) -1] <- rle_vec[which(rle_vec != 0)]
          rle_vec <- rle_vec +1
          df$`parallel sessions` <- rle_vec
          
          # Remove duplicated rows from the original DataFrame
          df <- df[!duplicated_rows, ]
          
          
        } 
        if (input$unique2) {
          # Create a temporary DataFrame excluding the column to ignore
          temp_df <- df[, c('course code')]
          
          # Find duplicated rows (excluding the first occurrence)
          duplicated_rows <- duplicated(temp_df)
          
          rle_vec <- rle(duplicated_rows)
          rle_vec <- rep(rle_vec$lengths, times=rle_vec$lengths)
          rle_vec[duplicated_rows == FALSE] <- 0
          rle_vec[which(rle_vec != 0) -1] <- rle_vec[which(rle_vec != 0)]
          rle_vec <- rle_vec +1
          df$`parallel sessions` <- rle_vec
          
          # Remove duplicated rows from the original DataFrame
          df <- df[!duplicated_rows, ]
          
        }
        firstcols <- c('id', 'week', 'day', 'date', 'start time', 'end time', 'course code', 'course', courseInfo, 'type', 'location', 'parallel sessions', 'staff name', 'academic year')
        firstcols <- firstcols[firstcols %in% colnames(df)]
        firstcols_ext <- c(firstcols, 'end', 'start')
        
        df <- df[c(firstcols, names(df)[!names(df) %in% firstcols_ext])]
        full_df <- bind_rows(full_df, df)
      }
    }
  }
  if (!identical(colnames(full_df), c("id", "course", "error"))) {
    if ('date' %in% colnames(full_df)) {
      full_df <- full_df[order(full_df$date %>% as.Date(format = '%d %b %Y')),]
    }
    if (input$seprows) {
      full_df[is.na(full_df)] <- ''
      # add break points
      if (cur_sepsel == 'days') {
        change_points <- c(TRUE, diff(as.Date(full_df$date, format = "%d %b %Y") %>% as.numeric()) != 0)
      } else if (cur_sepsel == 'weeks') {
        change_points <- c(TRUE, diff(as.numeric(full_df$week)) != 0)
      } else {
        change_points <- c(TRUE, diff(as.Date(full_df$date, format = "%d %b %Y") %>% format("%m") %>% as.numeric()) != 0)
      }
      change_points[is.na(change_points)] <- FALSE
      
      empty_row <- full_df[1, ]
      empty_row[] <- NA
      
      result_df <- full_df[0, ]
      
      for (i in 1:nrow(full_df)) {
        if (change_points[i]) {
          result_df <- rbind(result_df, empty_row, full_df[i, ])
        } else {
          result_df <- rbind(result_df, full_df[i, ])
        }
      }
      # Reset row names
      if (cur_sepsel == 'days') {
        result_df[is.na(result_df$course), 'date'] <- result_df[which(is.na(result_df$course)) + 1, 'date']
      } else if (cur_sepsel == 'weeks') {
        result_df[is.na(result_df$course), 'week'] <- result_df[which(is.na(result_df$course)) + 1, 'week']
      } else {
        result_df[is.na(result_df$course), 'date'] <- paste0('<font style="color:black">', result_df[which(is.na(result_df$course)) + 1, 'date'], '</font>') %>% str_replace(., ' ([A-Z][a-z][a-z]) ', ' <font style=\"color:white\">\\1<font style=\"color:black\"> ')
      }
      rownames(result_df) <- NULL
      full_df <- result_df
      cal_url <- paste0('https://www.rug.nl/feb/education/academic-calendar/', year, '-academic-calendar.pdf')
      if (http_status(HEAD(cal_url))$category == "Success") {
        full_df[!is.na(full_df$week),'week'] <- paste0('<a href="', cal_url, '" target="_blank">', full_df[!is.na(full_df$week),'week'], '</a>')
      }
      
      full_df[!is.na(full_df$course), 'id'] <- 1:nrow(full_df[!is.na(full_df$course),])
      full_df[which(is.na(full_df$course)), 'id'] <- full_df[which(is.na(full_df$course)) + 1, 'id'] %>% as.numeric() - 0.1
      
    } else {
      full_df$id <- 1:nrow(full_df)
    }
    if (!input$allcols) {
      full_df <- full_df[, firstcols[firstcols != 'academic year']]
    }
  }
  # fdf <<- full_df
  full_df
}


vals_and_cols_to_style <- function(df, colorselection, gradient, fcolorselection, fgradient) {
  bvals <- unique(df[,colorselection]) %>% as.vector() %>% unlist()
  bvals <- bvals[!is.na(bvals)]
  bcols <- get_colors_hcl(length(bvals), shuffle = gradient == 'shuffle')
  fvals <- unique(df[,fcolorselection]) %>% as.vector() %>% unlist()
  fvals <- fvals[!is.na(fvals)]
  fcols <- get_colors_hcl(length(fvals), start=80,luminance = 100, shuffle = fgradient == 'shuffle')
  list(bvals = bvals, bcols = bcols, fvals = fvals, fcols = fcols)
}

##### Server #####
server <- function(input, output, session) {
  
  get_sensible_sel <- function(df, cols, updatefromURL, forced=TRUE) {
    if (length(unique(df[!is.na(df$course), 'course'])) > 1 && 'type' %in% colnames(df)) {
      #print(1)
      col_sel <- 'course'
      fcol_sel <- 'type'
    } else if (any(c('type', 'info') %in% colnames(df)) && 'staff name' %in% colnames(df)) {
      #print(2)
      type_or_info <- cols[match(c('type', 'info'), cols)]
      col_sel <- type_or_info[!is.na(type_or_info)][1]
      fcol_sel <- 'staff name'
    } else {
      #print(3)
      col_sel <- 'course'
      fcol_sel <- cols[1]
    }
    if (!updatefromURL || forced) {
      cur_col_r(col_sel)
      #print(paste('1 changed cur_col_r()', cur_col_r()))
      cur_fcol_r(fcol_sel)
      cur_sepsel_r('weeks')
    } else {
      updatefromURL(FALSE)
    }
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = 'colorselection', selected = cur_col_r(), choices = cols)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = 'fcolorselection', selected = cur_fcol_r(), choices = cols)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = 'sepsel', selected = cur_sepsel_r(), choices = c('days', 'weeks', 'months'))
    years <- fromJSON(content(GET('https://rooster.rug.nl/api/year/all'), "text", encoding = "UTF-8"))$year
    if (!is_valid(cur_year_r())) {
      #print('no cur_year_r() set yet, will use input')
      cur_year_r(input$years)
    }
    #print(paste('setting sensible, so first year init UNCHANGED and setting selected to', cur_year_r(), 'cur_col_r()', cur_col_r(), 'cur_fcol_r()', cur_fcol_r()))
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = 'years', selected = cur_year_r(), choices = c('current', years))
  }
  
  # initiate reactiveVals
  updatefromURL <- reactiveVal(FALSE)
  course_df <- reactiveVal(NULL)
  cur_year_r <- reactiveVal('')
  cur_col_r <- reactiveVal('')
  cur_fcol_r <- reactiveVal('')
  cur_sepsel_r <- reactiveVal('')
  
  # enable/disable input elements depending on checkbox
  observeEvent(input$colorby, {
    for (id in c('colorselection', 'fcolorselection', 'highlightby','gradient', 'fgradient')) {
      if (input$colorby) {
        shinyjs::enable(id)
      } else {
        shinyjs::disable(id)
      }
    }
  })
  
  first_year_init <- reactiveVal(FALSE)
  
  observeEvent(input$years,{
    #print('input$years changed')
    if (is_valid(input$years)) {
      cur_year_r(input$years)
      #print('input$years changed is valid')
      if (first_year_init()) {
        #print('input$years changed and is year init, setting now to false')
        first_year_init(FALSE)
      } else {
          if (input$output == 'Courses') {
          #print('input$years changed and is not year init, lets get to work and change courses')
          courses_pulled <- pull_courses(input$years)
          courses_pulled_df <- data.table::data.table('start typing to filter courses' = courses_pulled[[1]])
          course <- input$courses
          if (!is_valid(course) || !all(course %in% courses_pulled[[1]])) {
            #print('no course or invalid course was selected, changing just choices')
            course <- NULL
          } else {
            #print('valid course, keeping it and changing choices')
          }
          updateSelectizeInput(session, 'courses', selected = course, choices = courses_pulled_df, options = list(placeholder = 'start typing course name or code'), server = FALSE)
          shinyjs::enable('courses')
          course_df(list(courses_pulled[[2]], courses_pulled_df))
          } else  {
            progs_pulled <- pull_progs(input$years)
            progs_pulled_df <- data.table::data.table('start typing to filter progs' = progs_pulled[[1]])
            prog <- input$courses
            if (!is_valid(prog) || !all(prog %in% progs_pulled[[1]])) {
              #print('no prog or invalid prog was selected, changing just choices')
              prog <- NULL
            } else {
              #print('valid prog, keeping it and changing choices')
            }
            updateSelectizeInput(session, 'courses', selected = prog, choices = progs_pulled_df, options = list(placeholder = 'start typing program name or code'), server = FALSE)
            shinyjs::enable('progs')
            course_df(list(progs_pulled[[2]], progs_pulled_df))        }
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # this is supposed to only trigger on app startup
  # add courses to selectize and pull years
  observe({ 
    if (input$output == 'Courses') {
      #print('adding courses and years, that will set year to current')
      years <- fromJSON(content(GET('https://rooster.rug.nl/api/year/all'), "text", encoding = "UTF-8"))$year
      first_year_init(TRUE)
      updateSelectInput(session, 'years', choices = c('current', years), selected = 'current')
      shinyjs::enable('years')
      courses_pulled <- pull_courses(get_academic_year())
      courses_pulled_df <- data.table::data.table('start typing to filter courses' = courses_pulled[[1]])
      updateSelectizeInput(session, 'courses', choices = courses_pulled_df, options = list(placeholder = 'start typing course name or code'), server = FALSE)
      shinyjs::enable('courses')
      course_df(list(courses_pulled[[2]], courses_pulled_df))
    } else {
      years <- fromJSON(content(GET('https://rooster.rug.nl/api/year/all'), "text", encoding = "UTF-8"))$year
      first_year_init(TRUE)
      updateSelectInput(session, 'years', choices = c('current', years), selected = 'current')
      shinyjs::enable('years')
      progs_pulled <- pull_progs(get_academic_year())
      progs_pulled_df <- data.table::data.table('start typing to filter progs' = progs_pulled[[1]])
      updateSelectizeInput(session, 'courses', choices = progs_pulled_df, options = list(placeholder = 'start typing program name or code'), server = FALSE)
      shinyjs::enable('courses')
      course_df(list(progs_pulled[[2]], progs_pulled_df))
    }
  })
  

  # Observe changes in the URL and update the input
  observe({ 
    query <- parseQueryString(session$clientData$url_search)
    if (is_valid(query$course)) {
      courses_in_url <- sapply(strsplit(query$courses, "\\|\\|")[[1]], URLdecode)
      valid_courses_in_url <- courses_in_url[courses_in_url %in% course_df()[[2]][[1]]]
      updateSelectizeInput(session, "courses", selected = valid_courses_in_url)
      updatefromURL(TRUE)
    }
    df <- isolate(df_react())
    if (is.null(colnames(df))) {
      if (is_valid(c(query$fcol, query$col))) {
        cur_col_r(query$col)
        #print(paste('2 changed cur_col_r()', cur_col_r()))
        cur_fcol_r(query$fcol)
        cur_year_r(query$year)
        cur_sepsel_r(query$sepsel)
      }
      return()
    }
    cols <- colnames(df)[colnames(df) != 'id']
    updateCheckboxInput(session, 'colorby', value = as.logical(query$colorby))
    updateCheckboxInput(session, 'hlby', value = as.logical(query$highlightby))
    updateCheckboxInput(session, 'seprows', value = as.logical(query$seprows))
    updateCheckboxInput(session, 'unique', value = as.logical(query$unique))
    updateCheckboxInput(session, 'allcols', value = as.logical(query$allcols))
    if (is_valid(c(query$fcol, query$col)) && query$col %in% cols && query$fcol %in% cols) {
      cur_col_r(query$col)
      #print(paste('3 changed cur_col_r()', cur_col_r()))
      cur_fcol_r(query$fcol)
      cur_year_r(query$year)
      cur_sepsel_r(query$sepsel)
      updateSelectInput(session, 'col', selected = cur_col_r(), choices = cols)
      updateSelectInput(session, 'fcol', selected = cur_fcol_r(), choices = cols)
      updateSelectInput(session, 'sepsel', selected = cur_sepsel_r(), choices = c('days', 'weeks', 'months'))
      years <- fromJSON(content(GET('https://rooster.rug.nl/api/year/all'), "text", encoding = "UTF-8"))$year
      first_year_init(TRUE)
      #print(paste('setting first year init to true and setting selected to', cur_year_r()))
      updateSelectInput(session, 'years', choices = c('current', years), selected = cur_year_r())
    }
    updateSelectInput(session, 'sepsel', selected = query$sepsel)

  })

  
  # Observe changes in the input and update the URL
  observe({ 
    input_values <- sapply(names(input)[!str_detect(names(input), 'table_')], function(x) input[[x]], simplify = FALSE)
      if (is_valid(input$courses)) {
        url_add = paste0("?courses=", encodeForURL(paste(input$courses, collapse = "||")))
      } else {
        url_add = "?courses=NULL"
      }
      if (!updatefromURL()) {
        if (input$colorby && is_valid(input$colorselection) && is_valid(input$fcolorselection) && input$colorselection %in% colnames(df_react()) && input$fcolorselection %in% colnames(df_react())) {
          cur_col_r(input$colorselection)
          #print(paste('4 changed cur_col_r()', cur_col_r()))
          cur_fcol_r(input$fcolorselection)
          cur_sepsel_r(input$sepsel)
          cur_year_r(input$years)
          url_add <- paste0(url_add, "&year=", input$years, "&colorby=", input$colorby, "&col=", cur_col_r(), "&hlby=", input$highlightby, "&fcol=", cur_fcol_r(),  "&grad=", input$gradient, "&fgrad=", 
                            input$fgradient, "&sepsel=", cur_sepsel_r(),  "&seprows=", input$seprows,  "&unique=", input$unique, "&allcols=", input$allcols)
        }
        updateQueryString(url_add, mode = "replace")
      }

  }, priority = 1)


  # preselect sensible choices for coloring scheme depending on number of courses and available columns
  observeEvent(input$courses, {
    df <- df_react()
    if (!is.null(colnames(df))) {
      shinyjs::show('hideme')
      cols <- colnames(df)[colnames(df) != 'id']
      get_sensible_sel(df, cols, updatefromURL(), forced = FALSE)
    }
  }, priority = 10)
  
  # get the data for the course(s) and process the data frame
  df_react <- reactive({
    if (is_valid(input$courses) && !is.null(course_df()[[1]])) {
      all_course_codes <- lapply(str_split(input$courses, ' - '), function(x) rev(x)[1]) %>% unlist()
      df <- course_df()[[1]] %>% unnest(cols = 'name', names_sep = ';')
      course_names <- df[match(all_course_codes, df$code),"name;nl"] %>% as.vector() %>% unlist() %>% as.character() %>% str_remove(., '^ | $') %>% stringr::str_squish()
      if (input$years == 'current') {
        selected_year <- get_academic_year()
      } else {
        selected_year <- input$years
      }
      parse_course_df(input, all_course_codes,  selected_year, course_names, cur_sepsel_r())
    }
  })
  
  # style the output table
  output$table <- renderDT({
    trigger_trick <- c(input$colorselection, input$fcolorselection)
    df <- df_react()
    if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        if (!identical(colnames(df), c("id", "course", "error"))) {
          out <- datatable(df, escape = FALSE, rownames= FALSE, 
                           options = list(pageLength = 50, lengthMenu = list(c(20, 50, 100, 200, -1), c('20', '50', '100', '200', 'All'))))
          if (input$colorby) {
            if (cur_col_r() == '' || !cur_col_r() %in% colnames(df) || !cur_fcol_r() %in% colnames(df)) {
              #print('no good')
              #print(c(cur_col_r() == '' , !cur_col_r() %in% colnames(df) , !cur_fcol_r() %in% colnames(df)))
              #print(c(cur_col_r()  , cur_col_r() , cur_fcol_r() ))
              columns <- colnames(df)[colnames(df) != 'id']
              get_sensible_sel(df, columns, updatefromURL())
            } 
            vc <- vals_and_cols_to_style(df, cur_col_r(), input$gradient, cur_fcol_r(), input$fgradient)
            # out <- out %>% formatStyle(cur_col_r(), target = 'row', backgroundColor = styleEqual(vc[['bvals']], vc[['bcols'
            
            # styling by row instead of styleEqual, because it somehow goes wrong for columns with HTML strings
            # sep_rows <- str_detect(as.character(df$id), '\\.')
            # rows_not_seperator <- seq_len(nrow(df))[!sep_rows]
            # rows_uniq_vals <- match(df[[cur_col_r()]], vc[['bvals']])
            # out <- out %>% formatStyle(cur_col_r(), target = 'row', backgroundColor = styleRow(rows_not_seperator,
            #                                                                   vc[['bcols']][rows_uniq_vals][!sep_rows]))
            out <- out %>% formatStyle(cur_col_r(), target = 'row', backgroundColor = styleEqual(as.list(vc[['bvals']]), vc[['bcols']]))
            if (input$highlightby) {
              # styling by row instead of styleEqual, because it somehow goes wrong for columns with HTML strings
              # rows_uniq_vals <- match(df[[cur_fcol_r()]], vc[['fvals']])
              # out <- out %>% formatStyle(cur_fcol_r(), backgroundColor = styleRow(rows_not_seperator,
              #                                                                              vc[['fcols']][rows_uniq_vals][!sep_rows]))
              out <- out %>% formatStyle(cur_fcol_r(), backgroundColor = styleEqual(as.list(vc[['fvals']]), vc[['fcols']]))
            } 
          }
          if (input$seprows && cur_fcol_r() != ''){ 
            out <- out %>% formatStyle('course', target = 'row', color = styleEqual(NA, 'white'), backgroundColor = styleEqual(NA, 'black')) %>% 
                           formatStyle(cur_fcol_r(), backgroundColor = styleEqual(NA, 'black')) %>% 
                           formatStyle('id', color = 'black')
          }
        } else {
          out <- datatable(df, options = list(dom = 't'), escape = FALSE)
        }
      out
    }
  })
}
