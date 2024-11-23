# ----- setup ------------------------------------------------------------------
required <- c("dplyr", "stringr", "ggplot2", "shiny", "shinyjs")
to_be_installed <- required[!(required %in% installed.packages()[,"Package"])]
if(length(to_be_installed)){
  install.packages(to_be_installed)
}
lapply(required, require, character.only = TRUE)

Sys.setlocale("LC_ALL", "en_US.UTF-8")


# ------------------------------------------------------------------------------

# ----- GLOBAL FINCTIONS -------------------------------------------------------
# test addition

# get input in the form of "XXh XXm XXs",
# and convert into duration in minutes rounded to 2 decimals
convert_time_to_min_en <- function(h_m_s){
  hms <- strsplit(h_m_s, split = " ")
  minutes <- 0
  
  for(i in hms[[1]]){
    t_type <- substr(i, nchar(i), nchar(i))
    numbers <- as.numeric(substr(i, 1, nchar(i) - 1))
    
    if (t_type == "h"){
      minutes <- minutes + numbers * 60
    } else if (t_type == "m"){
      minutes <- minutes + numbers
    }else if (t_type == "s"){
      minutes <- minutes + numbers / 60
    }
  }
  return(round(minutes, 2))
} 


# get input in the form of "XX sa XX dk XX sn"
# and convert into duration in minutes rounded to 2 decimals
convert_time_to_min_tr <- function(sa_dk_sn){
  sadksn <- strsplit(sa_dk_sn, split = " ")
  sadksn <- lapply(sadksn, 
                   function(x) str_replace_all(x, 
                                               setNames(c("h", "m", "s"), 
                                                        c("sa", "dk", "sn"))))
  
  sadksn <- paste(sadksn[[1]][seq(1, length(sadksn[[1]]), 2)],
                  sadksn[[1]][seq(2, length(sadksn[[1]]), 2)], 
                  sep = "", collapse = " ")
  
  minutes <- convert_time_to_min_en(sadksn)
  return(minutes)
  
}


# get date, time in the form of "mm/dd/yy, hh:mm:ss"
# in 12 hours format
# return time in the form of "hh:mm:ss"
# in 24 hours format
get_time_from_date_comma_time <- function(date_comma_time){
  time_part <- strsplit(strsplit(date_comma_time, ",")[[1]][2], " ")[[1]]
  
  if (time_part[3] == "PM"){
    time_part <- strsplit(time_part[2], ":")[[1]]
    time_part[1] <- as.character(as.numeric(time_part[1]) + 12) 
    time_part <- paste(time_part, collapse = ":")
  }else{
    time_part <- time_part[2]
  }
  return(time_part)
}


# get two time points in the form of "hh:mm:ss"
# return the difference between time2 - time1
# in the form of "XXh XXm XXs"
time_difference_as_duration <- function(time1, time2, next_day = T){
  time1 <- strsplit(time1, ":")[[1]]
  time2 <- strsplit(time2, ":")[[1]]
  
  # convert time to seconds
  time1_secs <- (as.numeric(time1[1]) * 3600) + (as.numeric(time1[2]) * 60) + as.numeric(time1[3])
  time2_secs <- (as.numeric(time2[1]) * 3600) + (as.numeric(time2[2]) * 60) + as.numeric(time2[3])
  
  # account for the next day cases 
  if ((time1_secs > time2_secs) & (next_day)){
    time2_secs <- time2_secs + (24 * 3600)
  }
  
  # calculate difference
  time_diff_secs <- time2_secs - time1_secs
  
  # convert back to hh:mm:ss
  hours  <- time_diff_secs %/% 3600
  remainder <- time_diff_secs %% 3600
  minutes <- remainder %/% 60
  seconds <- remainder %% 60
  
  # format result to fit "XXh XXm XXs"
  time_difference <- paste(paste(hours, "h", sep = ""), 
                           paste(minutes, "m", sep = ""), 
                           paste(seconds, "s", sep = ""), 
                           sep = " ")
  return(time_difference)
}


# detect language based on the first title
detect_lang <- function(att_file){
  lang <- strsplit(strsplit(att_file, "\n")[[1]][1], " ")[[1]][2]
  return(lang)
}


# get class duration
get_duration_time <- function(att_file){
  if (detect_lang(att_file) == "Summary"){
    duration <- str_match(att_file, "Meeting duration\t(.*)\n")[[2]]
    duration <- convert_time_to_min_en(duration)
  }else if (detect_lang(att_file) == "Özet"){
    duration <- str_match(att_file, "Toplantı süresi\t(.*)\n")[[2]]
    duration <- convert_time_to_min_tr(duration)
  }
  return(duration)
}

# get start and end hours
get_start_end <- function(att_file){
  if (detect_lang(att_file) == "Summary"){
    start <- str_match(att_file, "Start time\t(.*)\n")[[2]]
    start <- get_time_from_date_comma_time(start)
    
    end <- str_match(att_file, "End time\t(.*)\n")[[2]]
    end <- get_time_from_date_comma_time(end)
  }else if (detect_lang(att_file) == "Özet"){
    start <- str_match(att_file, "Başlangıç saati\t(.*)\n")[[2]]
    start <- get_time_from_date_comma_time(start)
    
    end <- str_match(att_file, "Bitiş saati\t(.*)\n")[[2]]
    end <- get_time_from_date_comma_time(end)
  }
  return(c(start, end))
}

# extract the table from unstructured data
extract_attendance_table <- function(att_file){
  if (detect_lang(att_file) == "Özet"){
    attendance_raw <- str_match(att_file, "2. Katılımcılar\n([\\S\\s]*)3. Toplantı İçi Etkinlikler")[[2]]
    attendance_raw <- strsplit(attendance_raw, "\n")[[1]]
    attendance_raw <- read.table(text = attendance_raw, header = T, 
                                 sep = "\t", comment.char = "")
    
    colnames(attendance_raw) <- c("Name", "First.Join", "Last.Leave", 
                                  "In.Meeting.Duration", "Email", 
                                  "Participant.ID.UPN.", "Role")
    
    
    meeting_start <- get_start_end(att_file)[1]
    meeting_end <- get_start_end(att_file)[2]
    
    attendance_processed <- attendance_raw %>% 
      mutate("duration(min)" = sapply(In.Meeting.Duration, convert_time_to_min_tr),
             "join(min after start)" = sapply(First.Join, function (x) 
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x)))),
             "leave(min after start)" = sapply(Last.Leave, function (x) 
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x)))),
             "leave(min before end)" = sapply(Last.Leave, function (x) 
               convert_time_to_min_en(time_difference_as_duration(get_time_from_date_comma_time(x), 
                                                                  meeting_end))),
             Role = ifelse(Role == "Düzenleyici", "Organiser", 
                           ifelse(Role == "Sunucu", "Presenter", Role)))
    
    if ("Organiser" %in% attendance_processed$Role){
      organiser_start <-
        get_time_from_date_comma_time(attendance_processed[which(attendance_processed$Role == "Organiser"),
        ]$First.Join)
      
      organiser_end <-
        get_time_from_date_comma_time(attendance_processed[which(attendance_processed$Role == "Organiser"),
        ]$Last.Leave)
      
      
      attendance_processed <- attendance_processed %>%
        mutate("leave(min before Organiser)" = sapply(Last.Leave, function (x) 
          convert_time_to_min_en(time_difference_as_duration(get_time_from_date_comma_time(x),
                                                             organiser_end, F))),
          "join(min after Organiser)" = sapply(First.Join, function (x)
            convert_time_to_min_en(time_difference_as_duration(organiser_start,
                                                               get_time_from_date_comma_time(x), F))))
    }else{
      attendance_processed <- attendance_processed %>%
        mutate("leave(min before Organiser)" = NA,
               "join(min after Organiser)" = NA)
    }
    
    # multi join part
    multi_join <- str_match(att_file, "3. Toplantı İçi Etkinlikler\n([\\S\\s]*)$")[[2]]
    multi_join <- strsplit(multi_join, "\n")[[1]]
    multi_join <- read.table(text = multi_join, header = T,
                             sep = "\t", comment.char = "")
    
    colnames(multi_join) <- c("Name", "Join", "Leave",
                              "Duration", "Email", "Role")
    
    multi_join_processed <- multi_join %>%
      mutate("multi_duration(min)" = sapply(Duration, convert_time_to_min_tr),
             "multi_join(min after start)" = sapply(Join, function (x)
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x)))),
             "multi_leave(min after start)" = sapply(Leave, function (x)
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x))))) %>%
      select(Email, "multi_join(min after start)", "multi_leave(min after start)", "multi_duration(min)") %>% 
      group_by(Email) %>% 
      summarize(across(c("multi_join(min after start)", "multi_leave(min after start)", "multi_duration(min)"), 
                       list), .groups = "drop")
    
  
    attendance_processed <- attendance_processed %>% 
      left_join(multi_join_processed, by = "Email")
    
    return(attendance_processed)
    
    }else if (detect_lang(att_file) == "Summary"){
    attendance_raw <- str_match(att_file, "2. Participants\n([\\S\\s]*)3. In-Meeting")[[2]]
    attendance_raw <- strsplit(attendance_raw, "\n")[[1]]
    attendance_raw <- read.table(text = attendance_raw, header = T, 
                                 sep = "\t", comment.char = "")
  
    colnames(attendance_raw) <- c("Name", "First.Join", "Last.Leave", 
                                  "In.Meeting.Duration", "Email", 
                                  "Participant.ID.UPN.", "Role")
    
    
    meeting_start <- get_start_end(att_file)[1]
    meeting_end <- get_start_end(att_file)[2]
    
    attendance_processed <- attendance_raw %>% 
      mutate("duration(min)" = sapply(In.Meeting.Duration, convert_time_to_min_en),
             "join(min after start)" = sapply(First.Join, function (x) 
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x)))),
             "leave(min after start)" = sapply(Last.Leave, function (x) 
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x)))),
             "leave(min before end)" = sapply(Last.Leave, function (x) 
               convert_time_to_min_en(time_difference_as_duration(get_time_from_date_comma_time(x), 
                                                                  meeting_end))),
             Role = ifelse(Role == "Düzenleyici", "Organiser", 
                           ifelse(Role == "Sunucu", "Presenter", Role)))
    
    if ("Organiser" %in% attendance_processed$Role){
      organiser_start <-
        get_time_from_date_comma_time(attendance_processed[which(attendance_processed$Role == "Organiser"),
        ]$First.Join)
      
      organiser_end <-
        get_time_from_date_comma_time(attendance_processed[which(attendance_processed$Role == "Organiser"),
        ]$Last.Leave)
      
      
      attendance_processed <- attendance_processed %>%
        mutate("leave(min before Organiser)" = sapply(Last.Leave, function (x) 
          convert_time_to_min_en(time_difference_as_duration(get_time_from_date_comma_time(x),
                                                             organiser_end, F))),
          "join(min after Organiser)" = sapply(First.Join, function (x)
            convert_time_to_min_en(time_difference_as_duration(organiser_start,
                                                               get_time_from_date_comma_time(x), F))))
    }else{
      attendance_processed <- attendance_processed %>%
        mutate("leave(min before Organiser)" = NA,
               "join(min after Organiser)" = NA)
    }
    
    # multi join part
    multi_join <- str_match(att_file, "3. In-Meeting Activities\n([\\S\\s]*)$")[[2]]
    multi_join <- strsplit(multi_join, "\n")[[1]]
    multi_join <- read.table(text = multi_join, header = T,
                             sep = "\t", comment.char = "")
    
    colnames(multi_join) <- c("Name", "Join", "Leave",
                              "Duration", "Email", "Role")
    
    multi_join_processed <- multi_join %>%
      mutate("multi_duration(min)" = sapply(Duration, convert_time_to_min_en),
             "multi_join(min after start)" = sapply(Join, function (x)
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x)))),
             "multi_leave(min after start)" = sapply(Leave, function (x)
               convert_time_to_min_en(time_difference_as_duration(meeting_start,
                                                                  get_time_from_date_comma_time(x))))) %>%
      select(Email, "multi_join(min after start)", "multi_leave(min after start)", "multi_duration(min)") %>% 
      group_by(Email) %>% 
      summarize(across(everything(), list), .groups = "drop")
    
    
    attendance_processed <- attendance_processed %>% 
      left_join(multi_join_processed, by = "Email")
    
    return(attendance_processed)
    }
  
  
}


# read the teams record csv file
read_teams_record_csv <- function(file_path){
  t_file <- scan(file_path, what = "", sep = "\n", fileEncoding = "UTF-16")
  t_file <- paste(t_file, collapse = "\n")
  return(t_file)
}

# plot total number of attendees at each time point
plot_attendees_by_time <- function(att_table, att_raw, 
                                   start_point, end_point, min_duration){
  
  att_segment <- att_table %>% 
    rowwise() %>%
    mutate(intervals = list(data.frame(
      x_start = `multi_join(min after start)`,
      x_end = `multi_leave(min after start)`,
      y = Email
    ))) %>%
    unnest(intervals)
  
  meeting_duration <- get_duration_time(att_raw)
  n_attendees <- c()
  time_points <-  seq(0, meeting_duration, 1)
  for(i in time_points){
    n_attendees <- c(n_attendees, nrow(att_table %>% 
                                         filter(`join(min after start)`<i & 
                                                  `leave(min after start)`>i)))
  }
  attendees <- data.frame(cbind(time_points, n_attendees))
  
  ggplot(data=attendees, aes(x=time_points, y=n_attendees))+
    geom_line(linewidth=1, aes(linetype = "n attendees", color="n attendees"))+
    geom_vline(linewidth=1, 
               aes(xintercept = start_point,
                   linetype="organiser joined", color="organiser joined"))+
    geom_vline(linewidth=1, 
               aes(xintercept = end_point,
                   linetype="organiser left", color="organiser left"))+
    scale_colour_manual(values=c("black", "green", "red"))+
    scale_linetype_manual(values=c("longdash", "solid", "solid"))+
    labs(title = "# Attendees by Time", 
         x="Time Points (min after meeting start)", y="# attendees",
         color="nani", linetype="nani")+
    theme_bw()+
    # coord_fixed()+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.background = element_blank(),
      legend.title = element_blank()
    )+
    geom_segment(data=att_segment, aes(y = as.numeric(as.factor(y)),
                                       yend = as.numeric(as.factor(y)),
                                       x = x_start,
                                       xend = x_end), 
                 inherit.aes = FALSE, size = 1, alpha = 0.2,
                 color = as.factor(ifelse(
                   as.logical((att_segment$`duration(min)` > min_duration) *
                                (att_segment$`join(min after start)` < start_point) *
                                (att_segment$`leave(min after start)` > end_point)),
                   "blue",
                   "black"))
    ) # reconsider colors
}

# from all recards uploaded, form a list that holds all names and Emails
form_attendance_list <- function(all_records){
  full_attendee <- all_records[[1]] %>%
    select(Name, Email) 
  
  if(length(all_records) > 1){
    for (i in 2:length(all_records)){
      curr_attendee <- all_records[[i]] %>%
        select(Name, Email)
      
      missed <- !(curr_attendee$Email %in% full_attendee$Email)
      
      if(any(missed)){
        full_attendee <- data.frame(rbind(full_attendee, curr_attendee[missed,]))
      }
    }
  }
  row.names(full_attendee) <- NULL
  return(full_attendee)
}


# add new attendance entry
expand_attendance <- function(all_name_email_att_df, 
                              curr_name_email_att_df,
                              new_col_name){
  # here it is assumed that the first argument is a data frame
  # with its last two columns being total and percent attendance
  tot_col <- all_name_email_att_df[,(ncol(all_name_email_att_df)-1)] # the total is 2 from last
  per_col <- all_name_email_att_df[,ncol(all_name_email_att_df)] # the percent is last
  tp_idx <- (ncol(all_name_email_att_df)-1):ncol(all_name_email_att_df) # vector of total and percent column indices

  anead <- all_name_email_att_df[,-tp_idx]
  
  # anead <- all_name_email_att_df
  anead[,new_col_name] <- "-"
  
  for(i in 1:nrow(curr_name_email_att_df)){
    if(!is.null(curr_name_email_att_df$Email[i])){
      if(sum(anead$Email %in% curr_name_email_att_df$Email[i]) == 1 &&
         curr_name_email_att_df$att[i] == TRUE){
        anead[anead$Email %in% curr_name_email_att_df$Email[i], new_col_name] <- "+"
        tot_col[anead$Email %in% curr_name_email_att_df$Email[i]] <-
        tot_col[anead$Email %in% curr_name_email_att_df$Email[i]] + 1 # increase total when + added to table
        tot_col <- as.integer(tot_col) # reassign class of tot_col
        per_col <- tot_col/(ncol(anead)-2) # recalculate percentage
         }
      else if(sum(anead$Email %in% curr_name_email_att_df$Email[i]) > 1){
        anead[anead$Email %in% curr_name_email_att_df$Email[i], new_col_name] <- NA
      }
    }  else{
      if(sum(anead$Name %in% curr_name_email_att_df$Name[i]) == 1 &&
         curr_name_email_att_df$att[i] == TRUE){anead[anead$Name %in% curr_name_email_att_df$Name[i], new_col_name] <- "+"
         tot_col[anead$Name %in% curr_name_email_att_df$Name[i]] <-
         tot_col[anead$Name %in% curr_name_email_att_df$Name[i]] + 1 # increase total when + added to table
         tot_col <- as.integer(tot_col) # reassign class of tot_col
         per_col <- tot_col/(ncol(anead)-2) # recalculate percentage
         }
      else if(sum(anead$Name %in% curr_name_email_att_df$Name[i]) > 1){
        anead[anead$Name %in% curr_name_email_att_df$Name[i], new_col_name] <- NA
      }
    }
  }
  
  anead <- data.frame(cbind(anead, "total"=tot_col, "percent"=per_col))
  
  return(anead)
}
# ------------------------------------------------------------------------------



# 
# file_names <- list.files(path="./new/", pattern="*.csv", full.names=TRUE, recursive=FALSE)
# 
# # import all files as tables, the tables are stored in a list
# files <- lapply(file_names, read_teams_record_csv)
# 
# 
# records <- lapply(files, function(x) extract_attendance_table(unlist(x)))
# 
# names(files) <- gsub("./new/", "", file_names)
# names(records) <- gsub("./new/", "", file_names)
# 
# 
# 
# 
# dat_r <- files["lab2.csv"][[1]]
# dat_t <- records["lab2.csv"][[1]]
# 
# 
# al <- form_attendance_list(records)
# 
# idx <- sample(1:nrow(al), 20, replace = F)
# idx <- c(idx, 112)
# samp <- al[idx,]
# samp$att <- sample(c(TRUE, FALSE), 21, replace = T)
# samp
# 
# al[,"att"] <- FALSE
# 
# 
# 
# 
# expand_attendance(al, samp, "att_x")
# 
# print("a")
# 
# 
# 
# 













# ----- SHINY APP --------------------------------------------------------------


# ui part
ui <- fluidPage(
  
  useShinyjs(),
  # spinner css / loading screen when the server is busy
  # locks the inputs and shows a loading spinner
  tags$head(
    tags$style(HTML("
  #loadmessage {
  position:fixed; z-index:8; top:50%; left:50%; padding:10px;
  text-align:center; font-weight:bold; color:#000000; background-color:#CCFF66;
  }
  
  .loader {
  position:fixed; z-index:8; border:16px solid #999999;
  border-top: 16px solid #ADD8E6; border-radius: 50%;
  width: 120px; height: 120px; top:45%; left:45%;
  animation: spin 2s linear infinite;
  }

  .prevent_click{
  position:fixed; 
  z-index:9;
  width:100%;
  height:100vh;
  background-color: transpare'nt;   
  }

  @keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
  }"))
  ),
  
  # display loading screen when shiny is busy
  conditionalPanel(
    condition = "$(\'html\').hasClass(\'shiny-busy\')",
    tags$div(class = "loader"),
    tags$div(class = "prevent_click")
  ),
  # actual UI
  navbarPage(
    "Teams Attendance Handler", id="ah",
    # navbar 1
    tabPanel("Uplad and Process Files", value="upf",
             sidebarLayout(sidebarPanel(
               # sidebar content 1
               uiOutput("file_upload"),
               # processing
               uiOutput("org_warn"),
               uiOutput("select_record"),
               uiOutput("organiser"),
               uiOutput("adj_start"),
               uiOutput("adj_arb_start"),
               uiOutput("adj_end"),
               uiOutput("adj_arb_end"),
               # hr(),
               # actionButton("test_button", "click to test")
             ), 
             mainPanel(
               # main content 1
               # uiOutput("test_text"),
               uiOutput("tabs")
             )) 
    ),
    # tab 2
    tabPanel("Final Attendance Table", value="ft",
             sidebarLayout(sidebarPanel(
               textOutput("ph_nb2"),
               # actionButton("test_button", "click to test"),
               uiOutput("final_table_ui")
               
             ), 
             mainPanel(
               # main content 1,
               tableOutput("att_table")
               
             )) 
    )
  )
)



# server part
server <- function(input, output, session) {
  # NAVBAR 1
  # ---------* everything below is for importing and readying the tables *------
  
  # first render the file upload
  output$file_upload <- renderUI({
    if(!is.null(input$tabsets) && input$tabsets != default_tabset){return()}
      list(fileInput("file", "Upload the file", multiple = TRUE, accept = ".csv"),
           hr())
      })
  
  
  raw_files <- reactiveVal()
  records <- reactiveVal()
  rec_edit <- reactiveVal() # this vector holds how many times each record was edited in plot tab
  
  # wait for file upload to update, and if the files are actually imported...
  observeEvent(input$file, {
    if(!is.null(input$file)){
      # this output keeps the file information table
      output$tb <- renderTable({input$file})
      
      # from imported files,
      # read the files
      fil <- lapply(input$file$datapath, read_teams_record_csv)
      names(fil) <- input$file$name
      raw_files(fil)
      
      # extract the tables
      rec <- lapply(raw_files(), 
                    function(x) extract_attendance_table(unlist(x)))
      names(rec) <- input$file$name
      records(rec)
      }})
  # to get all attendees across all records
  all_nea <- reactiveVal(NULL)
  observeEvent(records(), {
    if(!is.null(records())){
      att_list <- form_attendance_list(records())
      att_list <- data.frame(cbind(att_list,
                                   data.frame("total" = as.integer(rep(0, nrow(att_list)))),
                                   data.frame("percent" = rep(0, nrow(att_list)))))
      all_nea(att_list)
      
      
      rec_edit(rep(0, length(names(records())))) # this vector holds how many times each record was edited in plot tab
    }
  })
  
  # show the drop down menu to select a specific record
  output$select_record <- renderUI({
    if(is.null(input$tabsets) || input$tabsets == default_tabset){return()}
    if(!is.null(records()) && length(records()) == 1){
      selectInput("selected_record", "Select A Record:", choices = names(records()),
                  selected = selected_record_name())
    }else if(length(records()) > 1){
      if(sum(checkOrg()) <= 1){
        list(selectInput("selected_record", "Select A Record:", choices = names(records()),
                         selected = selected_record_name()),
             actionButton("previous", "< Previous"),
             actionButton("next", "Next >"),
             hr()
        )
      }else{
        list(selectInput("selected_record", "Select A Record:", choices = names(records()),
                         selected = selected_record_name()),
             actionButton("previous", "< Previous"),
             actionButton("next", "Next >"),
             actionButton("next_nor", "Next with No Organiser >>"),
             hr()
        )
      } 
    }
  })
  
  selected_record_name <- reactiveVal(NULL)
  selected_record_index <- reactiveVal(NULL)
  
  # assign the name and the index of the selected record
  observeEvent(input$selected_record, {
    selected_record_index(which(names(records()) == input$selected_record))
    selected_record_name(input$selected_record)
  })
  
  # functionality for the button that moves to previous record
  observeEvent(input$previous, {
    val <- selected_record_index() - 1
    if(val < 1){val = 1}
    
    selected_record_index(val)
    selected_record_name(names(records())[[val]])
    updateSelectInput(inputId = "selected_record", 
                      selected = selected_record_name())
  })
  # functionality for the button that moves to next record
  observeEvent(input$`next`, {
    val <- selected_record_index() + 1
    if(val > length(names(records()))){
      val <- length(names(records()))}
    
    selected_record_index(val)
    selected_record_name(names(records())[[val]])
    updateSelectInput(inputId = "selected_record", 
                      selected = selected_record_name())
  })
  
  
  # functionality for the button jumps between records with no organizer
  observeEvent(input$next_nor, {
    norganiser_idx <- which(checkOrg())
    diff_to_nor <- norganiser_idx - selected_record_index()
    
    if(any(diff_to_nor > 0)){
      val <- selected_record_index() + min(diff_to_nor[diff_to_nor > 0])
    }else{
      val <- selected_record_index() + min(diff_to_nor[diff_to_nor < 0])
    }
    
    selected_record_index(val)
    selected_record_name(names(records())[[val]])
    updateSelectInput(inputId = "selected_record",
                      selected = selected_record_name())
  })

  
  # srt is the selected record table, based on the drop down menu selection
  srt <- eventReactive(c(selected_record_name(), input$applyOrganiser), 
                       {
                         records()[which(names(records()) == 
                                           selected_record_name())][[1]]
                         })

  # srf is the selected record file, based on the drop down menu selection
  srf <- eventReactive(selected_record_name(),
                       {
                         raw_files()[which(names(raw_files()) == 
                                             selected_record_name())][[1]]
                         })
  
  
  
  # check if the selected record has an Organiser
  isOrganiser <- reactive({
    "Organiser" %in% srt()$Role
  })

  # also check if all the records have Organiser, to be able to warn user
  # note: FALSE entered if there is Organiser, because the inverted vector
  # was needed for later but it was not possible to easily invert the vector
  # which was saved as a reactive value
  checkOrg <- reactive({
    sapply(records(), function(x) ifelse("Organiser" %in% x$Role, FALSE, TRUE))
  })

  # if at least 1 record lacks an Organiser, warn user to check the single 
  # records and assign an organiser to missing ones
  output$org_warn <- renderUI({
    if(is.null(input$file)){return()}
    if (any(checkOrg())){
      to_check <- paste(names(records())[checkOrg()], collapse = ", ")
      if(!is.null(input$tabsets) && input$tabsets != default_tabset){
        list(
          helpText("These records do not have an Organiser labeled:"),
          helpText(to_check),
          helpText("Select and Apply Organisers before proceeding."),
          hr()
        )
      }else{
        list(
          helpText("These records do not have an Organiser labeled:"),
          helpText(to_check),
          helpText("Select and Apply Organisers before proceeding."),
          actionButton("go_organiser", "Proceed"),
          hr()
        )
      }
    }
  })
  
  
  # if the current record does not have an Organised assigned,
  # warn user and show drop down menu,
  # also show a button to apply changes
  output$organiser <- renderUI({
    if(is.null(input$tabsets) || input$tabsets == default_tabset){return()}
    if (sum(checkOrg()) == 0 && input$tabsets == "Individual Record Data"){
      return(
        list(
          helpText("You can now see the attendance plots for individual meeting records."),
          actionButton("go_att_plt", "Proceed"),
          hr()))
    }
    if (isOrganiser() || is.null(input$file)){return()}
    list(helpText("No Organiser detected!"),
         helpText("Select an attendee as Organiser to calculate related
                        variables"),
         selectizeInput("select_organiser", "Select Organiser:", 
                        multiple = FALSE, choices = srt()$Name),
         actionButton("applyOrganiser", "Apply Organiser"),
         hr())
  })
  
  
  # adjust the selected record so that the name selected from the drop down menu 
  # is the new Organiser
  new_srt <- reactive({
    organiser_end <-
      get_time_from_date_comma_time(srt()[which(srt()$Name ==
                                                  input$select_organiser),
      ]$Last.Leave)
    organiser_start <- get_time_from_date_comma_time(srt()[which(srt()$Name ==
                                                                   input$select_organiser),
    ]$First.Join)
    
    srt() %>%
      mutate("leave(min before Organiser)" =
               sapply(Last.Leave, function (x)
                 convert_time_to_min_en(time_difference_as_duration(get_time_from_date_comma_time(x),
                                                                    organiser_end, F))),
             "join(min after Organiser)" =
               sapply(First.Join, function (x)
                 convert_time_to_min_en(time_difference_as_duration(organiser_start,
                                                                    get_time_from_date_comma_time(x), F))),
             Role = ifelse(row_number() == which(Name == input$select_organiser),
                           "Organiser", Role))
    
  })
  
  # this output keeps the selected record table after selecting an Organiser
  output$single_record_table <- renderDataTable({
    if(is.null(input$file)){return()}
    srt()
  })
  
  
  # apply the changes in the srt when the button clicked
  observeEvent(input$applyOrganiser, {
    current_records <- records()
    current_new_srt <- new_srt()

    current_records[which(names(records()) == selected_record_name())][[1]] <-
      current_new_srt

    records(current_records)
    
    if(sum(checkOrg()) >= 1){
      norganiser_idx <- which(checkOrg())
      diff_to_nor <- norganiser_idx - selected_record_index()
      
      if(any(diff_to_nor > 0)){
        val <- selected_record_index() + min(diff_to_nor[diff_to_nor > 0])
      }else{
        val <- selected_record_index() + min(diff_to_nor[diff_to_nor < 0])
      }
      
      selected_record_index(val)
      selected_record_name(names(records())[[val]])
      updateSelectInput(inputId = "selected_record",
                        selected = selected_record_name())
    }
    
    
  })
  
  # keep track of the last active tabset
                    # change this if you change tab names
  default_tabset <- "Uploaded Records"
  selected_tabset <- reactiveVal({default_tabset}) # default tab
  observeEvent(input$tabsets, {
    selected_tabset(input$tabsets)
  })
  
  # the functionality for the button under the no organiser warning
  observeEvent(input$go_organiser, {
    updateTabsetPanel(inputId = "tabsets", selected = "Individual Record Data")
                                          # change this if you change tab names
    selected_record_index(which(names(records()) == 
                                names(records())[checkOrg()][[1]]))
    selected_record_name(names(records())[checkOrg()][[1]])
    })

  # the functionality for the button that takes you to attendance plot tab
  observeEvent(input$go_att_plt, {
    updateTabsetPanel(inputId = "tabsets", selected = "Individual Record Plot")
    # change this if you change tab names
  })  

  
  # ---------* everything above was for importing and readying the tables *-----
  # ---------* everything below is for adjusting the attendance parameters *----
  # related ariables
  start_type <- reactiveVal(NULL)
  end_type <- reactiveVal(NULL)
  minimum_duration <- reactiveVal(NULL)
  meeting_start <- 0
  meeting_end <- reactive({get_duration_time(srf())})
  organiser_join <- reactive({srt()[srt()$Role=="Organiser","join(min after start)"][[1]]})
  organiser_leave <- reactive({srt()[srt()$Role=="Organiser","leave(min after start)"][[1]]})
  current_nea <- reactiveVal(NULL)
  
  # ui elements for adjusting parameters
  output$adj_start <- renderUI({
    if(is.null(input$file) || is.null(input$tabsets)){return()}
    if(input$tabsets == "Individual Record Plot"){
      # list(
      #   selectInput("start_type_i", "Start:", choices= c("Meeting Time 0",
      #                                                    "Organiser Join",
      #                                                    "Arbitrary")),
      #   selectInput("end_type_i", "End:", choices= c("Meeting Time Final",
      #                                                "Organiser Leave",
      #                                                "Arbitrary")),
      #   sliderInput("arb_start", "Select End Time (minutes after start)",
      #               min = 0, max = get_duration_time(srf()),
      #               step = 1, value = c(meeting_start, meeting_end())),
      #   sliderInput("min_dur", "Minimum Attendance Duration:", min = 0,
      #               max = get_duration_time(srf()), step = 1, value = 60),
      #   helpText("Press 'DONE!' to add this file to Attendance Table"),
      #   actionButton("apply_att", "DONE!"),
      #   hr()
      # )
      if(rec_edit()[selected_record_index()] <= 0){
        list(
          selectInput("start_type_i", "Start:", choices= c("Meeting Time 0",
                                                           "Organiser Join",
                                                           "Arbitrary")),
          selectInput("end_type_i", "End:", choices= c("Meeting Time Final",
                                                       "Organiser Leave",
                                                       "Arbitrary")),
          sliderInput("arb_start", "Select End Time (minutes after start)",
                      min = 0, max = get_duration_time(srf()),
                      step = 1, value = c(meeting_start, meeting_end())),
          sliderInput("min_dur", "Minimum Attendance Duration:", min = 0,
                      max = get_duration_time(srf()), step = 1, value = 60),
          helpText("Press 'DONE!' to add this file to Attendance Table"),
          actionButton("apply_att", "DONE!"),
          hr()
        )
      }else{
        list(
          selectInput("start_type_i", "Start:", choices= c("Meeting Time 0",
                                                           "Organiser Join",
                                                           "Arbitrary")),
          selectInput("end_type_i", "End:", choices= c("Meeting Time Final",
                                                       "Organiser Leave",
                                                       "Arbitrary")),
          sliderInput("arb_start", "Select End Time (minutes after start)",
                      min = 0, max = get_duration_time(srf()),
                      step = 1, value = c(meeting_start, meeting_end())),
          sliderInput("min_dur", "Minimum Attendance Duration:", min = 0,
                      max = get_duration_time(srf()), step = 1, value = 60),
          helpText("Press 'DONE!' to add this file to Attendance Table"),
          actionButton("apply_att", "DONE!"),
          helpText(paste("This record was tweaked",
                         rec_edit()[selected_record_index()], "times.")),
          hr()
        )
      }
    }
  })
  
  # assign start type when ui input updated
  observeEvent(input$start_type_i, {
    if(!is.null(input$start_type_i)){
      if(input$start_type_i == "Meeting Time 0"){
        updateSliderInput(inputId = "arb_start", value = c(meeting_start, input$arb_start[[2]]))
      }else if(input$start_type_i == "Organiser Join"){
        updateSliderInput(inputId = "arb_start", value = c(organiser_join(), input$arb_start[[2]]))
      }
    }
  })
  
  # assign end type when ui input updated
  observeEvent(input$end_type_i, {
    if(!is.null(input$end_type_i)){
      if(input$end_type_i == "Meeting Time Final"){
        updateSliderInput(inputId = "arb_start", value = c(input$arb_start[[1]], meeting_end()))
      }else if(input$end_type_i == "Organiser Leave"){
        updateSliderInput(inputId = "arb_start", value = c(input$arb_start[[1]], organiser_leave()))
      }
    }
  })
  
  # assign start and end times when ui updated
  observeEvent(input$arb_start,{
    if (!is.null(input$arb_start)){
      if(input$arb_start[[1]] >= input$arb_start[[2]]-10){
        updateSliderInput(inputId = "arb_start", 
                          value = c(input$arb_start[[2]]-10, 
                                    input$arb_start[[2]])) 
      }
      start_type(input$arb_start[[1]])
      end_type(input$arb_start[[2]])
      
      # adjust start selector
      if(input$arb_start[[1]] == meeting_start){
        updateSelectInput(inputId = "start_type_i", selected = "Meeting Time 0")
      }else if(input$arb_start[[1]] == round(organiser_join()) ||
               input$arb_start[[1]] == organiser_join()){
        updateSelectInput(inputId = "start_type_i", selected = "Organiser Join")
      }else{
        updateSelectInput(inputId = "start_type_i", selected = "Arbitrary")
      }

      # adjust end selector
      if(input$arb_start[[2]] == round(meeting_end()) || 
         input$arb_start[[2]] == meeting_end()){
        updateSelectInput(inputId = "end_type_i", selected = "Meeting Time Final")
      }else if(input$arb_start[[2]] == round(organiser_leave()) ||
               input$arb_start[[2]] == organiser_leave()){
        updateSelectInput(inputId = "end_type_i", selected = "Organiser Leave")
      }else{
        updateSelectInput(inputId = "end_type_i", selected = "Arbitrary")
      }
    }
  })
  
  # assign minimum duration when ui input updated
  observeEvent(input$min_dur, {
    if(!is.null(input$min_dur)){minimum_duration(input$min_dur)}
  })
  
  
  # the plot of the selected record
  observeEvent(list(selected_record_name(), 
                    start_type(), end_type(), minimum_duration()),
               {
                 output$single_record_plot <- renderPlot({
                   if(is.null((input$file)) || any(checkOrg())){return()}
                   if(is.null(input$tabsets) ||
                      is.null(selected_record_name())){return()}
                   if(input$tabsets != "Individual Record Plot"){return()}
                   if(is.null(start_type()) || is.null(end_type())){return()}
                   plot_attendees_by_time(srt(), srf(),
                                          start_type(), end_type(), 
                                          minimum_duration())
                 })
                 if(!is.null(input$tabsets) && 
                    input$tabsets == "Individual Record Plot"){
                   name_email <- srt()[,c("Name", "Email")]
                   att <- as.logical((srt()$`duration(min)` > minimum_duration()) *
                                       (srt()$`join(min after start)` < start_type()) *
                                       (srt()$`leave(min after start)` > end_type()))
                   current_nea(data.frame(cbind(name_email, data.frame(att)))) 
                   
                   output$single_record_info <- renderUI({
                     if(is.null((input$file)) || any(checkOrg())){return()}
                     if(is.null(input$tabsets) ||
                        is.null(selected_record_name())){return()}
                     if(input$tabsets != "Individual Record Plot"){return()}
                     if(is.null(start_type()) || is.null(end_type())){return()}
                     total <- nrow(srt())
                     attendee <- sum(att)
                     left_out <- total - attendee
                     late_comers <- sum(srt()$`join(min after start)` > start_type())
                     early_goers <- sum(srt()$`leave(min after start)` < end_type())
                     short_styrs <- sum(srt()$`duration(min)` > minimum_duration())
                     
                     return(
                       HTML(paste(
                         paste("<b>Total number:</b>", total),
                         paste("<b>Present:</b>", attendee),
                         paste("<b>Absent:</b>", left_out),
                         paste(HTML('&emsp;'), "Joined late:", late_comers),
                         paste(HTML('&emsp;'), "Left early:", early_goers),
                         paste(HTML('&emsp;'), "Stayed too short:", short_styrs),
                         sep = "<br>"
                       ))
                     )
                     
                   })
                 }
                 })
  
  
  
  # --------* everything above was for adjusting the attendance parameters *----
  # ----* everything below is for rendering the tabset panel under navbar 1*----
  # rtender the tabset panel
  output$tabs <- renderUI({
    if(is.null(input$file)) {return()}
    if(any(checkOrg())){
      tabsetPanel(id="tabsets",
                  tabPanel("Uploaded Records", tableOutput("tb")),
                  tabPanel("Individual Record Data", 
                           dataTableOutput("single_record_table")),
                  selected = selected_tabset()
      )
    }else{
      tabsetPanel(id="tabsets",
                  tabPanel("Uploaded Records", tableOutput("tb")),
                  tabPanel("Individual Record Data", 
                           dataTableOutput("single_record_table")),
                  tabPanel("Individual Record Plot", 
                           plotOutput("single_record_plot"),
                           uiOutput("single_record_info")),
                  tabPanel("More", textOutput("txt")),
                  selected = selected_tabset()
      )
    }
  })
  # ----* everything above was for rendering the tabset panel under navbar 1*---
  
  # NAVBAR 2
  # -------* everything below is for saving the current attendance column *-----
  
  # functionality for DONE! button
  observeEvent(input$apply_att,{
    new_nea <- expand_attendance(all_nea(), 
                                 current_nea(), 
                                 selected_record_name())
    rec_editto <- rec_edit()
    rec_editto[selected_record_index()] <- rec_editto[selected_record_index()] + 1
    rec_edit(
      rec_editto
    ) # this vector holds how many times each record was edited in plot tab
    all_nea(new_nea)
  })
  

  # place holder for navbar 2 sidebar panel
  output$ph_nb2 <- renderText({"More Controls Here!"})
  
  # add save button here
  output$final_table_ui <- renderUI({
    if(input$ah == "ft"){
      downloadButton("save_table", "Save")
    }else{
      return()  
    }
    
  })
  
  # functionality for save button
  output$save_table <- downloadHandler(
    filename = function(){"attendance_table.csv"},
    content = function(fname){
      write.csv(all_nea(), fnames,)}
  )
  
  observeEvent(all_nea(), {
    if(!is.null(all_nea)){
      output$att_table <- renderTable({all_nea()})
    }
    
  })
  
 
  
  
  
  # to test and print stuff
  # observeEvent(input$test_button, {
  #   to_print <- input$ah
  #   print(to_print)
  #   output$test_text <- renderText({
  #     to_print
  #   })
  # })
  
}

shinyApp(ui, server)


