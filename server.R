server <- function(input, output, session) {
  output$table <- renderDT({

    if (!is.null(input$file) && !is.null(input$file$datapath)) {
    
      cal <- readLines(input$file$datapath, warn = FALSE)
      cal <- paste(cal, collapse = '\n')
      cal <- gsub("\n ", "", cal)
      cal <- str_split(cal, '\n')[[1]]
      stopifnot(!any(grepl("^\\s+", cal))) # disregarding value fields that have linefeeds for the sake of simplicity 
      keyval <- do.call(rbind, regmatches(cal, regexpr(":", cal, fixed = TRUE), invert = TRUE))
      keyval <- keyval[which.max(keyval[,1]=="BEGIN" & keyval[,2]=="VEVENT"):tail(which(keyval[,1]=="END" & keyval[,2]=="VEVENT"), 1),]
      keyval <- cbind.data.frame(keyval, id=cumsum(keyval[,1]=="BEGIN" & keyval[,2]=="VEVENT"))
      cal.df <- reshape(keyval, timevar="1", idvar="id", direction = "wide")
      
      cal.df$`2.DTEND` <- ymd_hms(cal.df$`2.DTEND`)
      cal.df$`2.DTEND` <- format(cal.df$`2.DTEND`, "%d %B, %Y %H:%M")
      cal.df$`2.DTSTART` <- ymd_hms(cal.df$`2.DTSTART`)
      cal.df$`2.DTSTART` <- format(cal.df$`2.DTSTART`, "%d %B, %Y %H:%M")
      
      cal.df <- cal.df %>%
        mutate(
          Name = str_extract(`2.SUMMARY`, ".*(?= )"),
          Type = str_extract(`2.SUMMARY`, "[^ ]+$")
        )

      cal.df <- cal.df[,colnames(cal.df)[!colnames(cal.df) %in% c('2.DTSTAMP','2.BEGIN', '2.END', '2.UID', '2.SUMMARY', 'id')]]
      
      print(table(cal.df$Type))
      
      cal.df
    }
  })
}
