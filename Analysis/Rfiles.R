# batch file to concatenate project files
REM ###########################################################
REM #
REM # concat.bat
REM #
REM # concatenate redacted files for submissions
REM #
REM ###########################################################

copy concat.bat + redact.txt + 00_help_funcs.R + 00_eot_build.R + 00_clean_funcs.R + 
  01_emp_churn.R + 02_utilization.R + 03_improvement.R + 04_crisis_int.R + 
  07_partitions.R + 08_model_phq9.R + 09_model_ansa.R + 11_model_emp.R + 
  12_model_crisis.R Rfiles.txt


# R
##########################################
#
# 00_load.R
#
# Dan Diamond
# 9/25/18
#
##########################################

library(DBI)

##########################################
# connectors / disconnect
##########################################

# warehouse
connect_to_wh <- function () {
  wh_connector <- dbConnect(RMySQL::MySQL(),
    dbname = "[REDACTED]",
    host = "[REDACTED]",
    port = [REDACTED],
    user = "[REDACTED]", 
    password = "[REDACTED]")
  
  return(wh_connector)
}
# dbListTables(wh_connect)


# ebs detail
connect_to_hr <- function () {
  hr_connector <- dbConnect(RMySQL::MySQL(),
    dbname = "[REDACTED]",
    host = "[REDACTED]",
    port = [REDACTED],
    user = "[REDACTED]", 
    password = "[REDACTED]")

    return(hr_connector)
}
#dbListTables(hr_connect)


# exit interview
connect_to_exit <- function () {
  exit_connector <- dbConnect(RMySQL::MySQL(),
    dbname = "[REDACTED]", 
    host = "[REDACTED]",
    port = [REDACTED],
    user = "[REDACTED]", 
    password = "[REDACTED]")
  
  return(exit_connector)
}
#dbListTables(exit_connect)


# other legacy data (if needed)
connect_to_legacy <- function () {
  legacy_connector <- dbConnect(RMySQL::MySQL(),
    dbname = "[REDACTED]",
    host = "[REDACTED]",
    port = [REDACTED],
    user = "[REDACTED]", 
    password = "[REDACTED]")
  
  return(legacy_connector)
}
#dbListTables(wva_connect)

##########################################
# get_Table functions
##########################################

getWHTable <- function(tbl_name, wh_connect) {
  q <- "SELECT * FROM "
  query <- paste0(q, tbl_name, ';')
  frame <- dbGetQuery(wh_connect, query)
  return(frame)
}

getHRTable <- function(tbl_name, hr_connect) {
  q <- "SELECT * FROM "
  query <- paste0(q, tbl_name, ';')
  frame <- dbGetQuery(hr_connect, query)
  return(frame)
}

getLocalTable <- function(tbl_name, exit_connect) {
  q <- "SELECT * FROM "
  query <- paste0(q, tbl_name, ';')
  frame <- dbGetQuery(exit_connect, query)
  return(frame)
}

getCSVTable <- function(path) {
  q <- "SELECT * FROM "
  query <- paste0(q, tbl_name, ';')
  frame <- dbGetQuery(exit_connect, query)
  return(frame)
}


# disconnect
dbDisconnectAll <- function () {
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}

##########################################
#
# 00_help_funcs.R
#
# Dan Diamond
# 10/04/18
#
##########################################

##########################################
#
# backUpFrame( dataframe, [name of file = name of frame], [# rows/file = 100000])
#
# backUpFrame takes 3 arguments:
#   - a data frame
#   - a filename (default to name of data frame)
#   - the maximum rows per file (default to 100000)
#
# and it breaks frames larger than the row limit
#   up into multiple pieces and saves as csv files.
#
##########################################

backUpFrame <- function (df, filename = deparse(substitute(df)), rows_per_file = 100000) {
#   writeLines(name(df))
    df_rows <- nrow(df)
    num_files <- ceiling(nrow(df) / rows_per_file)
    path <- "G:/888/FrameBackUps/"
    
    if(exists(deparse(substitute(df)))) {
      msg <- paste0("I'll create ", num_files, " file(s).")
      writeLines(msg)    
    
      if (df_rows <= rows_per_file) {
        filename <- paste0(path, filename, ".csv")
        writeLines(filename)
        write.csv(df, file = filename)
      } else {
        for(i in 0:(num_files-1)) {
          filenm <- paste0(path, filename, i, ".csv")
          first_row <- i * rows_per_file
          last_row <- min(c(df_rows, (i+1)*rows_per_file ))
          writeLines(paste0(filenm, "  last row is: ", last_row))
          write.csv(df[first_row:last_row, ], file = filenm)
        }  # end for
      } # end if else
    } # end if exists  
} # end function

improve_rate <- function (event, metric) {
  compare <- as.data.frame(cbind(event, metric))
  compare <- compare[event == TRUE, ]
  imp_rate <- round(100*sum(compare$metric)/nrow(compare), 2)
  
  return (imp_rate)
}

##########################################
#
# 00_eot_build.R
#
# Dan Diamond
# 9/21/18
#
# Provides structure for creation of 
#   'episode of treatment' table.  
#
##########################################

# rm(list=ls())
library(dplyr)

# ansa
build_eot_ansa <- function (ansa) {
  #ansa_client_count = 0
  ep_count = 0
  EOT_ansa <- data.frame(ansa_epid=character(), 
      research_num=integer(),
      start_date=as.Date(character()),
      end_date=as.Date(character()),
      ep_length=integer(),
      LON_start=integer(), LON_end=integer(),
      #ReasonCode=integer(), EpisodeStatus=integer() #, 
      Black=integer(), NativeAmerican=integer(), 
      Asian=integer(), Caucasian=integer(), 
      HawPacificIsle=integer(), OtherRace=integer(), 
      Ethnicity=integer(), Religion=integer(), 
      EduLevel=integer(),
	  #diag_cat=character(),
      start_hous_stab=integer(), 
      end_hous_stab=integer(),
      start_psych=integer(), end_psych=integer(),
      start_depr=integer(), end_depr=integer(), 
      start_anx=integer(), end_anx=integer(),
      start_interpers=integer(), end_interpers=integer(),
      diff_hous_stab=integer(),
      diff_LON=integer(), diff_psych=integer(),
      diff_depr=integer(), diff_anx=integer(), 
      diff_intepers=integer(), sum_diffs=integer()
      )

  # j = loop through clients
  # i = loop through assessments for each client
  for (j in unique(ansa$research_num)) {
    single <- ansa[ansa$research_num == j,]
    single_length <- nrow(single)

    # remove ansas closing episodes that would be too short
    if(single_length > 1) {
      i = 1
      while(!is.na(single$InterviewDate[i+1])) {
        period <- as.numeric(single$InterviewDate[i+1]-single$InterviewDate[i])
        if (period < 25) {
            single <- single[-(i+1),]
          } else {
            i <- i + 1
          } # end if
      } # end while
    } # end if
        
    single_length <- nrow(single)
    
    if(single_length > 1) {
      for (i in 1:(single_length-1)) {
        ep_count <- ep_count + 1
        temp_ep <- data.frame('ansa_epid'= paste(single$research_num[i], i, sep="_"), 
            'research_num' = single$research_num[i], 
            "start_date"=single$InterviewDate[i],
            "end_date"=single$InterviewDate[i+1], 
            'ep_length'= as.numeric(single$InterviewDate[i+1]-single$InterviewDate[i]),
            'LON_start'=single$OptionLevel[i], 
            'LON_end'=single$OptionLevel[i+1],
            #'ReasonCode'=single$Reasoncode[i], 'EpisodeStatus'=single$EpisodeStatus[i] #, 
            'Black'=single$Black[i], 'NativeAmerican'=single$Black[i], 
            'Asian'=single$Asian[i], 'Caucasian'=single$Caucasian[i], 
            'HawPacificIsle'=single$HawPacificIsle[i], 'OtherRace'=single$OtherRace[i], 
            'Ethnicity'=single$Ethnicity[i], 'Religion'=single$Religion[i], 
            'EduLevel'=single$EduLevel[i],
            'diag_cat'=add_diag_cat(single$research_num[i]),
            'start_hous_stab'=single$HousingStability[i], 
            'end_hous_stab'=single$HousingStability[i+1], 
            'start_psych'=single$BHPsychosis[i], 'end_psych'=single$BHPsychosis[i+1],
            'start_depr'=single$BHDepression[i], 'end_depr'=single$BHDepression[i+1],
            'start_anx'=single$BHAnxiety[i], 'end_anx'=single$BHAnxiety[i+1],
            'start_interpers'=single$BHInterpersonal[i], 
            'end_interpers'=single$BHInterpersonal[i+1],
            'diff_hous_stab'=(single$HousingStability[i+1] - single$HousingStability[i]),
            'diff_LON'=(single$OptionLevel[i+1] - single$OptionLevel[i]),
            'diff_psych'=(single$BHPsychosis[i+1] - single$BHPsychosis[i]),
            'diff_depr'=(single$BHDepression[i+1] - single$BHDepression[i]),
            'diff_anx'=(single$BHAnxiety[i+1] - single$BHAnxiety[i]),
            'diff_interpers'=(single$BHInterpersonal[i+1] - single$BHInterpersonal[i])
        )
        cat(sprintf("%-10s%15s%15s%15f%5d%5d%15s\n", # %9d
             temp_ep$ansa_epid, as.character(temp_ep$start_date), 
             as.character(temp_ep$end_date), as.integer(temp_ep$ep_length), temp_ep$LON_start, temp_ep$LON_end,  
             temp_ep$diag_cat 
             #temp_ep$anger_start#, 
             #temp_ep$anger_end #, 
             #ep_count
             )
             )
        
        EOT_ansa <- rbind(EOT_ansa, temp_ep)
      } # end for i
    } # end if > 1
  } # end for j
  
  EOT_ansa$sum_diffs <- (EOT_ansa$diff_hous_stab + EOT_ansa$diff_psych + EOT_ansa$diff_depr + 
    EOT_ansa$diff_anx + EOT_ansa$diff_interpers)
  
  return (EOT_ansa)
}


# ansa non-discharge ONLY!
build_eot_ansa_cm_prog <- function (ansa) {
  #ansa_client_count = 0
  ep_count = 0
  EOT_ansa <- data.frame(ansa_epid=character(), 
                         research_num=integer(),
                         start_date=as.Date(character()),
                         end_date=as.Date(character()),
                         ep_length=integer(),
                         LON_start=integer(), LON_end=integer(),
                         #ReasonCode=integer(), EpisodeStatus=integer() #, 
                         Black=integer(), NativeAmerican=integer(), 
                         Asian=integer(), Caucasian=integer(), 
                         HawPacificIsle=integer(), OtherRace=integer(), 
                         Ethnicity=integer(), Religion=integer(), 
                         EduLevel=integer(),
                         #      diag_cat=character(),
                         start_hous_stab=integer(), 
                         end_hous_stab=integer(),
                         start_psych=integer(), end_psych=integer(),
                         start_depr=integer(), end_depr=integer(), 
                         start_anx=integer(), end_anx=integer(),
                         start_interpers=integer(), end_interpers=integer(),
                         diff_hous_stab=integer(),
                         diff_LON=integer(), diff_psych=integer(),
                         diff_depr=integer(), diff_anx=integer(), 
                         diff_intepers=integer(), sum_diffs=integer(),
                         first_reason=integer()
  )
  
  # j = loop through clients
  # i = loop through assessments for each client
  for (j in unique(ansa$research_num)) {
    single <- ansa[ansa$research_num == j,]
    single_length <- nrow(single)
    
    # remove ansas closing episodes that would be too short
    if(single_length > 1) {
      i = 1
      while(!is.na(single$InterviewDate[i+1])) {
        period <- as.numeric(single$InterviewDate[i+1]-single$InterviewDate[i])
        if (period < 25) {
          single <- single[-(i+1),]
        } else {
          i <- i + 1
        } # end if
      } # end while
    } # end if
    
    single_length <- nrow(single)
    
    if(single_length > 1) {
      for (i in 1:(single_length-1)) {
        ep_count <- ep_count + 1
        temp_ep <- data.frame('ansa_epid'= paste(single$research_num[i], i, sep="_"), 
                              'research_num' = single$research_num[i], 
                              "start_date"=single$InterviewDate[i],
                              "end_date"=single$InterviewDate[i+1], 
                              'ep_length'= as.numeric(single$InterviewDate[i+1]-single$InterviewDate[i]),
                              'LON_start'=single$OptionLevel[i], 
                              'LON_end'=single$OptionLevel[i+1],
                              # 'ReasonCode'=single$Reasoncode[i], 'EpisodeStatus'=single$EpisodeStatus[i] #, 
                              'Black'=single$Black[i], 'NativeAmerican'=single$Black[i], 
                              'Asian'=single$Asian[i], 'Caucasian'=single$Caucasian[i], 
                              'HawPacificIsle'=single$HawPacificIsle[i], 'OtherRace'=single$OtherRace[i], 
                              'Ethnicity'=single$Ethnicity[i], 'Religion'=single$Religion[i], 
                              'EduLevel'=single$EduLevel[i],
                              'diag_cat'=add_diag_cat(single$research_num[i]),
                              'start_hous_stab'=single$HousingStability[i], 
                              'end_hous_stab'=single$HousingStability[i+1], 
                              'start_psych'=single$BHPsychosis[i], 'end_psych'=single$BHPsychosis[i+1],
                              'start_depr'=single$BHDepression[i], 'end_depr'=single$BHDepression[i+1],
                              'start_anx'=single$BHAnxiety[i], 'end_anx'=single$BHAnxiety[i+1],
                              'start_interpers'=single$BHInterpersonal[i], 
                              'end_interpers'=single$BHInterpersonal[i+1],
                              'diff_hous_stab'=(single$HousingStability[i+1] - single$HousingStability[i]),
                              'diff_LON'=(single$OptionLevel[i+1] - single$OptionLevel[i]),
                              'diff_psych'=(single$BHPsychosis[i+1] - single$BHPsychosis[i]),
                              'diff_depr'=(single$BHDepression[i+1] - single$BHDepression[i]),
                              'diff_anx'=(single$BHAnxiety[i+1] - single$BHAnxiety[i]),
                              'diff_interpers'=(single$BHInterpersonal[i+1] - single$BHInterpersonal[i]),
                              'first_reason'=(single$ReasonCode[i])
        )
        cat(sprintf("%-10s%15s%15s%15f%5d%5d%15s\n", # %9d
                    temp_ep$ansa_epid, as.character(temp_ep$start_date), 
                    as.character(temp_ep$end_date), as.integer(temp_ep$ep_length), temp_ep$LON_start, temp_ep$LON_end,  
                    temp_ep$diag_cat 
                    #temp_ep$anger_start#, 
                    #  temp_ep$anger_end #, 
                    #ep_count
        )
        )
        #        temp_ep$diag_cat <- add_diag_cat(temp_ep$research_num)
        #          mapply(add_diag_cat, re_num = temp_ep$research_num)
        if(temp_ep$first_reason != 3) {
          EOT_ansa <- rbind(EOT_ansa, temp_ep)
        }
      } # end for i
    } # end if > 1
  } # end for j
  
  EOT_ansa$sum_diffs <- (EOT_ansa$diff_hous_stab + EOT_ansa$diff_psych + EOT_ansa$diff_depr + 
                           EOT_ansa$diff_anx + EOT_ansa$diff_interpers)
  
  return (EOT_ansa)
}



# phq9
build_eot_phq9 <- function (p9) {
  #phq9_client_count = 0
  ep_count = 0
  EOT_phq9 <- data.frame(p9_epid=character(),
    research_num=integer(),
#     start_date=as.Date(character()),
#     end_date=as.Date(character()),
    start_date=as.POSIXct(character()),
    end_date=as.POSIXct(character()),

    tot_score_start=integer(),
    tot_score_end=integer(),
    diff_total=integer()
  )
  
  # j = loop through clients
  # i = loop through assessments for each client
  for (j in unique(p9$research_num)) {
    single <- phq9[p9$research_num == j,]
    single_length <- nrow(single)
    
    if(single_length > 1) {
      i = 1
      while(!is.na(single$serv_date[i+1])) {
        period <- as.numeric(single$serv_date[i+1]-single$serv_date[i])
        if (period < 3) {
          single <- single[-(i+1),]
        } else {
          i <- i + 1
        } # end if
      } # end while
    } # end if
    
    single_length <- nrow(single)
    
    if(single_length > 1) {
      for (i in 1:(single_length-1)) {
        ep_count <- ep_count + 1
        temp_ep <- data.frame('phq9_epid'= paste(single$research_num[i], 
                                                 i, sep="_"), 
            'research_num' = single$research_num[i], 
            "start_date"=single$serv_date[i], # !!!!!!! substitute visit date
            "end_date"=single$serv_date[i+1], #   here, too.  
            # 'ReasonCode'=single$ReasonCode[i],
            'tot_score_start'=single$TotalScore[i], 
            'tot_score_end'=single$TotalScore[i+1], 
            'diff_total'=(single$TotalScore[i+1]-single$TotalScore[i])
        )
        #cat(sprintf("%-10s%10d%22s%22s%5d%5d%9d\n", temp_ep$phq9_epid, 
        #   temp_ep$research_num, as.character(temp_ep$start_date), 
        #            as.character(temp_ep$end_date), temp_ep$tot_score_start, 
        #            temp_ep$tot_score_end, ep_count))
        EOT_phq9 <- rbind(EOT_phq9, temp_ep)
      } # end for i
    } # end if > 1
  } # end for j
  
  return (EOT_phq9)
}

# was the client prescribed Risperdal during this period?
took_risp <- function (start, end, res_num) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  mh_client <- medhist[which(medhist$research_num == res_num), ]
  mh_cli_period <- mh_client[which(mh_client$date_created > start &
                mh_client$date_created < (end - as.difftime(14, units="days"))), ]
  risp_rx <- str_detect(mh_cli_period$medication, regex("^risp", ignore_case = TRUE))
  any_risp <- any(risp_rx, na.rm = TRUE)
  return(any_risp)
}

# was the client prescribed Clozaril during this period?
took_cloz <- function (start, end, res_num) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  mh_client <- medhist[which(medhist$research_num == res_num), ]
  mh_cli_period <- mh_client[which(mh_client$date_created > start &
      mh_client$date_created < (end - as.difftime(14, units="days"))), ]
  cloz_rx <- str_detect(mh_cli_period$medication, regex("cloz", ignore_case = TRUE))
  any_cloz <- any(cloz_rx, na.rm = TRUE)
  return(any_cloz)
}

# was the client prescribed Prozac during this period?
took_proz <- function (start, end, res_num) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  mh_client <- medhist[which(medhist$research_num == res_num), ]
  mh_cli_period <- mh_client[which(mh_client$date_created > start &
      mh_client$date_created < (end - as.difftime(35, units="days"))), ]
  proz_rx <- str_detect(mh_cli_period$medication, regex("proza", ignore_case = TRUE))
  fluox_rx <- str_detect(mh_cli_period$medication, regex("fluox", ignore_case = TRUE))

  any_proz <- any(proz_rx, na.rm = TRUE) | any(fluox_rx, na.rm = TRUE)
  return(any_proz)
}

# was the client prescribed Wellbutrin during this period?
took_well <- function (start, end, res_num) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  mh_client <- medhist[which(medhist$research_num == res_num), ]
  mh_cli_period <- mh_client[which(mh_client$date_created > start &
      mh_client$date_created < (end - as.difftime(35, units="days"))), ]
  well_rx <- str_detect(mh_cli_period$medication, regex("well", ignore_case = TRUE))
  buprop_rx <- str_detect(mh_cli_period$medication, regex("buprop", ignore_case = TRUE))
  
  any_well <- any(well_rx, na.rm = TRUE) | any(buprop_rx, na.rm = TRUE)
  return(any_well)
}

# was the client prescribed Lexapro during this period
took_lexa <- function (start, end, res_num) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  mh_client <- medhist[which(medhist$research_num == res_num), ]
  mh_cli_period <- mh_client[which(mh_client$date_created > start &
      mh_client$date_created < (end - as.difftime(35, units="days"))), ]
  lexa_rx <- str_detect(mh_cli_period$medication, regex("lexap", ignore_case = TRUE))
  escit_rx <- str_detect(mh_cli_period$medication, regex("escita", ignore_case = TRUE))
  
  any_lexa <- any(lexa_rx, na.rm = TRUE) | any(escit_rx, na.rm = TRUE)
  return(any_lexa)
}

add_meds_eot_ansa <- function (eotans) {
  eotans$start_date <- as.POSIXct(eotans$start_date)
  eotans$end_date <- as.POSIXct(eotans$end_date)

  eotans$took_risp <- mapply(took_risp, start = eotans$start_date, 
    end = eotans$end_date, res_num = eotans$research_num)

  eotans$took_cloz <- mapply(took_cloz, start = eotans$start_date, 
    end = eotans$end_date, res_num = eotans$research_num)

  eotans$took_proz <- mapply(took_proz, start = eotans$start_date, 
    end = eotans$end_date, res_num = eotans$research_num)

  eotans$took_well <- mapply(took_well, start = eotans$start_date, 
    end = eotans$end_date, res_num = eotans$research_num)
    
  eotans$took_lexa <- mapply(took_lexa, start = eotans$start_date, 
    end = eotans$end_date, res_num = eotans$research_num)

  return(eotans)
}


add_meds_eot_phq9 <- function (eotp9) {
  eotp9$took_proz <- mapply(took_proz, start = eotp9$start_date, 
    end = eotp9$end_date, res_num = eotp9$research_num)
  
  eotp9$took_well <- mapply(took_well, start = eotp9$start_date, 
    end = eotp9$end_date, res_num = eotp9$research_num)

  eotp9$took_lexa <- mapply(took_lexa, start = eotp9$start_date, 
    end = eotp9$end_date, res_num = eotp9$research_num)
  
  return(eotp9)
}

med_added <- function (took_med, last_med) {
  if (took_med == TRUE && last_med == FALSE) {
    added <- TRUE 
  } else {
    added <- FALSE
  }
  
  return (added)
}


add_lasts_eot_ansa <- function (eotansa) {
  eotansa$last_risp <- NA
  eotansa$last_cloz <- NA
  eotansa$last_proz <- NA
  eotansa$last_well <- NA
  eotansa$last_lexa <- NA
  eotansa$last_std_wk   <- NA
  eotansa$last_cm_wk    <- NA
  eotansa$last_therp_wk <- NA
  
  for (j in unique(eotansa$research_num)) {
    single <- eotansa[which(eotansa$research_num == j), ] 
    
    single$last_risp <- lag(single$took_risp, default = FALSE)
    single$last_cloz <- lag(single$took_cloz, default = FALSE)
    single$last_proz <- lag(single$took_proz, default = FALSE)
    single$last_well <- lag(single$took_well, default = FALSE)
    single$last_lexa <- lag(single$took_lexa, default = FALSE)
    single$last_std_wk   <- lag(single$std_per_wk, default = 0)
    single$last_cm_wk    <- lag(single$cm_per_wk, default = 0)
    single$last_therp_wk <- lag(single$therp_per_wk, default = 0)
    
    eotansa[which(eotansa$research_num == j), 'last_risp'] <- single$last_risp
    eotansa[which(eotansa$research_num == j), 'last_cloz'] <- single$last_cloz
    eotansa[which(eotansa$research_num == j), 'last_proz'] <- single$last_proz
    eotansa[which(eotansa$research_num == j), 'last_well'] <- single$last_well
    eotansa[which(eotansa$research_num == j), 'last_lexa'] <- single$last_lexa
    eotansa[which(eotansa$research_num == j), 'last_std_wk']   <- single$last_std_wk
    eotansa[which(eotansa$research_num == j), 'last_cm_wk']    <- single$last_cm_wk
    eotansa[which(eotansa$research_num == j), 'last_therp_wk'] <- single$last_therp_wk
  }
  return (eotansa)
}

add_lasts_eot_phq9 <- function (eotphq9) {
  eotphq9$last_proz <- NA
  eotphq9$last_well <- NA
  eotphq9$last_lexa <- NA
  eotphq9$last_std_wk   <- NA
  eotphq9$last_cm_wk    <- NA
  eotphq9$last_therp_wk <- NA
  
  for (j in unique(eotphq9$research_num)) {
    single <- eotphq9[which(eotphq9$research_num == j), ] 
    
    single$last_proz <- lag(single$took_proz, default = FALSE)
    single$last_well <- lag(single$took_well, default = FALSE)
    single$last_lexa <- lag(single$took_lexa, default = FALSE)
    single$last_std_wk   <- lag(single$std_per_wk, default = 0)
    single$last_cm_wk    <- lag(single$cm_per_wk, default = 0)
    single$last_therp_wk <- lag(single$therp_per_wk, default = 0)
    
    eotphq9[which(eotphq9$research_num == j), 'last_proz'] <- single$last_proz
    eotphq9[which(eotphq9$research_num == j), 'last_well'] <- single$last_well
    eotphq9[which(eotphq9$research_num == j), 'last_lexa'] <- single$last_lexa
    eotphq9[which(eotphq9$research_num == j), 'last_std_wk']   <- single$last_std_wk
    eotphq9[which(eotphq9$research_num == j), 'last_cm_wk']    <- single$last_cm_wk
    eotphq9[which(eotphq9$research_num == j), 'last_therp_wk'] <- single$last_therp_wk
  }
  return (eotphq9)
}


add_firsts_eot_ansa <- function(eotansa) {
  eotansa$first_risp <- 
    mapply(med_added, took_med = eotansa$took_risp, last_med = eotansa$last_risp)
  eotansa$first_cloz <- 
    mapply(med_added, took_med = eotansa$took_cloz, last_med = eotansa$last_cloz)
  eotansa$first_proz <- 
    mapply(med_added, took_med = eotansa$took_proz, last_med = eotansa$last_proz)
  eotansa$first_well <- 
    mapply(med_added, took_med = eotansa$took_well, last_med = eotansa$last_well)
  eotansa$first_lexa <- 
    mapply(med_added, took_med = eotansa$took_lexa, last_med = eotansa$last_lexa)
  
  return(eotansa)  
}

add_firsts_eot_phq9 <- function(eotphq9) {
  eotphq9$first_proz <- 
    mapply(med_added, took_med = eotphq9$took_proz, last_med = eotphq9$last_proz)
  eotphq9$first_well <- 
    mapply(med_added, took_med = eotphq9$took_well, last_med = eotphq9$last_well)
  eotphq9$first_lexa <- 
    mapply(med_added, took_med = eotphq9$took_lexa, last_med = eotphq9$last_lexa)
  eotphq9$first_any_antidep <- 
    mapply(first_any_antidep, f_proz = eotphq9$first_proz, f_well = eotphq9$first_well, 
          f_lexa = eotphq9$first_lexa)
  return(eotphq9)  
}

first_any_antidep <- function(f_proz, f_well, f_lexa) {
  first_meds <- c(f_proz, f_well, f_lexa)
  f_any <- any(first_meds)
  
  return(f_any)
}


any_ansa_imps <- function(diff_psych, diff_depr, diff_anx, 
                          diff_interpers, diff_LON, diff_hous_stab) {
  if(diff_psych < 0 | diff_depr < 0 | diff_anx < 0 |
     diff_interpers < 0 | diff_LON < 0 | diff_hous_stab < 0) {
    any_imps = 1
  } else {
    any_imps = 0
  }
  
  return(any_imps)
}


serv_rate_wk <- function (r_num, vistype, start, end) {
  start <- as.POSIXct(start)
  end   <- as.POSIXct(end)
  services <- EOT_vis[(which(EOT_vis$research_num == r_num)),]
  services <- services[(which(services$rev_timein >= start)),]
  services <- services[(which(services$rev_timein < end)),]
  services <- services[(which(services$visittype == as.character(vistype))),]
  
  sum_units <- sum(services$units_of_svc)
  period <- as.numeric(end - start)
  
  wk_rate <- (sum_units / period) * 7
  return (wk_rate)
}
head(EOT_vis_cm)
head(clientvisit)

serv_rate_wk_cm_prog <- function (r_num, start, end) {
  start <- as.POSIXct(start)
  end   <- as.POSIXct(end)
  services <- EOT_vis_cm[(which(EOT_vis_cm$research_num == r_num)),]
  services <- services[(which(services$rev_timein >= start)),]
  services <- services[(which(services$rev_timein < end)),]

  sum_units <- sum(services$units_of_svc)
  period <- as.numeric(end - start)
  
  wk_rate <- (sum_units / period) * 7
  return (wk_rate)
}

add_services_eot <- function (eot) {
  eot$std_per_wk <- mapply(serv_rate_wk, r_num = eot$research_num, 
    vistype = 'SKILLS TRAINING/INDV', start = eot$start_date, 
    end = eot$end_date)
  eot$cm_per_wk <- mapply(serv_rate_wk, r_num = eot$research_num, 
    vistype = 'CASE MANAGEMENT', start = eot$start_date, 
    end = eot$end_date)
  eot$therp_per_wk <- mapply(serv_rate_wk, r_num = eot$research_num, 
    vistype = 'INDV PSYCHOTHERAPY', start = eot$start_date, 
    end = eot$end_date)
  
  return(eot)
}

add_cm_prog_services_eot <- function (eot) {
  eot$cm_prog_wk <- mapply(serv_rate_wk_cm_prog, r_num = eot$research_num, 
                           start = eot$start_date, 
                           end = eot$end_date)
  eot$std_per_wk <- mapply(serv_rate_wk, r_num = eot$research_num, 
                           vistype = 'SKILLS TRAINING/INDV', start = eot$start_date, 
                           end = eot$end_date)
  eot$cm_per_wk <- mapply(serv_rate_wk, r_num = eot$research_num, 
                          vistype = 'CASE MANAGEMENT', start = eot$start_date, 
                          end = eot$end_date)
  return(eot)
}


add_serv_diff_eot <- function (eot) {
  eot$diff_std <- 
    mapply(function(this, last){this - last}, this=eot$std_per_wk, 
        last=eot$last_std_wk)
  eot$diff_cm <- 
    mapply(function(this, last){this - last}, this=eot$cm_per_wk, 
        last=eot$last_cm_wk)
  eot$diff_therp <- 
    mapply(function(this, last){this - last}, this=eot$therp_per_wk, 
        last=eot$last_therp_wk)

  return(eot)
}

build_eot_crisis <- function(c_list, count = 1) {

  EOT_crisis <- data.frame(     #crisis_epid=character(), 
     crisis_start=as.Date(character()), crisis_end=as.Date(character()),
     research_num=integer(), no_shows=integer(), 
     any_crisis=integer() )

  crisis_start <- seq(as.Date(as.Date('2012-12-01')), Sys.Date(), by = "month")
  crisis_end <- lead(crisis_start)
  
  #c_list <- c_list[1:5] # for testing
   
  #count = 1
  for (r_num in unique(c_list)) {
    
    EOT_crisis_cli <- cbind.data.frame(crisis_start, crisis_end)
    EOT_crisis_cli <- EOT_crisis_cli[-nrow(EOT_crisis_cli), ]
    EOT_crisis_cli$research_num <- sapply(r_num, function (x) {x})

    EOT_crisis_cli$num_services <-
      mapply(getNumServices, re_num = EOT_crisis_cli$research_num, 
             start=EOT_crisis_cli$crisis_start, end=EOT_crisis_cli$crisis_end)
    
    # EOT_crisis_cli <- EOT_crisis_cli[EOT_crisis_cli$num_services != 0, ]
    
    EOT_crisis_cli$no_shows <- 
      mapply(num_noshows, res_num = EOT_crisis_cli$research_num, 
        start=EOT_crisis_cli$crisis_start, end=EOT_crisis_cli$crisis_end)
        
    EOT_crisis_cli$no_shows_last <- 
      mapply(num_noshows, res_num = EOT_crisis_cli$research_num, 
        start=lag(EOT_crisis_cli$crisis_start), end=lag(EOT_crisis_cli$crisis_end))
    
    EOT_crisis_cli$any_crisis <-
      mapply(had_crisis, res_num = EOT_crisis_cli$research_num, 
        start=EOT_crisis_cli$crisis_start, end=EOT_crisis_cli$crisis_end)

    EOT_crisis_cli <- EOT_crisis_cli[which(EOT_crisis_cli$num_services != 0 |
       EOT_crisis_cli$no_shows != 0 |
       EOT_crisis_cli$no_shows_last != 0 |
       EOT_crisis_cli$any_crisis != 0), ]
    
    EOT_crisis_cli$BirthGender <- 
      mapply(getBirthGender, re_num = EOT_crisis_cli$research_num)
    
    EOT_crisis_cli$Age <- 
      mapply(getAge, re_num = EOT_crisis_cli$research_num, 
        start = EOT_crisis_cli$crisis_start)
    
    EOT_crisis_cli$cum_crisis <- cumsum(EOT_crisis_cli$any_crisis)
    
    EOT_crisis_cli$diag_cat <- 
      mapply(add_diag_cat, re_num = EOT_crisis_cli$research_num)
    
    EOT_crisis <- rbind(EOT_crisis, EOT_crisis_cli)
    
    cat(sprintf("%10d%10d\n", count, r_num)) 
    count = count + 1
  } # end for

  return(EOT_crisis)
} # end build

add_diag_cat <- function (re_num) {

  if (re_num %in% cli_rec_diag_lookup$research_num) {
    diag <- cli_rec_diag_lookup$diag_cat[cli_rec_diag_lookup$research_num == re_num]
  } else {
    diag <- 'Unknown'
  }

  return(diag)
}

num_noshows <- function (res_num, start, end) {
  cli_vis <- crisisvisit[which(crisisvisit$research_num == res_num &
    crisisvisit$rev_timein > as.Date(start, origin='1970-01-01') &
    crisisvisit$rev_timein < as.Date(end, origin='1970-01-01')), ]
  noshows <- sum(cli_vis$NoShow)
  return(noshows)
}

had_faster_crisis <- function(res_num, start, end) {
  cli_vis <- crisisvisit[which(crisisvisit$research_num == res_num &
             crisisvisit$rev_timein > as.Date(start, origin='1970-01-01') &
             crisisvisit$rev_timein < as.Date(end, origin='1970-01-01')), ]
  
  crisis <-length(cli_vis$visittype[
      cli_vis$visittype == 'Emergency Call Log' |
      cli_vis$visittype == 'CM if Client is IP'| 
      cli_vis$visittype == 'CRISIS - PHONE ONLY'| 
      cli_vis$visittype == 'Crisis Add On'| 
      cli_vis$visittype == 'CRISIS INTERVENTION'| 
      cli_vis$visittype == 'CRISIS INTRVNTN F2F'| 
      cli_vis$visittype == 'Emergency Call Log'|
      cli_vis$visittype == 'Hospital Admission'| 
      cli_vis$visittype == 'EMERGENCY CONTACT'])
  
  return(crisis)
}

had_crisis <- function(res_num, start, end) {
  crisis = 0
  cli_vis <- crisisvisit[which(crisisvisit$research_num == res_num &
      crisisvisit$rev_timein >= as.Date(start, origin='1970-01-01') &
      crisisvisit$rev_timein < as.Date(end, origin='1970-01-01')), ]

  crisis_types  <- c('CM if Client is IP', 'CRISIS - PHONE ONLY', 'Crisis Add On', 
                     'CRISIS INTERVENTION', 'CRISIS INTRVNTN F2F', 'Emergency Call Log',
                     'Hospital Admission', 'EMERGENCY CONTACT')
    
  for (vtype in cli_vis$visittype) {
    if(vtype %in% crisis_types) {
      crisis = 1
      return(crisis)
    } else {
      next
    }
  }  
  return(crisis)
}

getNumServices <- function(re_num, start, end) {
  num = 0
  cli_vis <- crisisvisit[which(crisisvisit$research_num == re_num &
                                 crisisvisit$rev_timein >= as.Date(start, origin='1970-01-01') &
                                 crisisvisit$rev_timein < as.Date(end, origin='1970-01-01')), ]
  num <- nrow(cli_vis)
  
  return(num)
}

getBirthGender <- function (re_num) {
  b_gen <- client$BirthGender[client$research_num == re_num]
  return(b_gen)
}

getAge <- function(re_num, start) {
  dob <- as.Date(client$dob[client$research_num == re_num])
  age <- round(as.double(start - dob)/365, digits = 0)
  return(age)
}

##########################################
#
# 00_clean_funcs.R
#
# Dan Diamond
# 9/25/18
#
# Utility functions for data cleaning
# and de-identification.
#
##########################################

library('stringr')

##########################################
# client
##########################################
# create lookup table to de-identify client id's
c_lookup_df <- function (client) {
  c_id <- unique(client$client_id)
  set.seed(42)
  research_num <- sample.int(1e6, length(c_id)) # generate random client ID #'s
  lookup <- as.data.frame(cbind(c_id, research_num))
  return(lookup)
}

prepClient <- function (c) {
  keep <- c('research_num', 'admission_date', 'client_status', 'dob', 'BirthGender', 
               'city', 'preferred_language', 'english_fluency', 'employment_status', 
               'commitment_type', 'white', 'family_size', 'black', 'family_income', 
               'asian', 'amer_indian', 'marital_status', 'polynesian', 'ethnicity', 
               'other_race', 'veteran_status', 'religion', 'smoking_status', 
               'education_level', 'housing_category', 'housing_stability', 
               'resid_support', 'community_integration', 'homeless_serv', 
               'assigned_benefits', 'case_manager_emp_id', 
               'date_diagnosis_last_updated', 'date_medication_last_updated',
               'high_no_show')
  client <- subset(c, select = keep)
  
  # set data types
  client$admission_date               <- as.Date(client$admission_date)
  client$client_status                <- as.factor(client$client_status)
  client$dob                          <- as.Date(client$dob)
  client$white                        <- as.factor(client$white)
  client$black                        <- as.factor(client$black)
  client$asian                        <- as.factor(client$asian)
  client$amer_indian                  <- as.factor(client$amer_indian)
  client$marital_status               <- as.factor(client$marital_status)
  client$polynesian                   <- as.factor(client$polynesian)
  client$ethnicity                    <- as.factor(client$ethnicity)
  client$other_race                   <- as.factor(client$other_race)
  client$veteran_status               <- as.factor(client$veteran_status)
  client$religion                     <- as.factor(client$religion)
  client$smoking_status               <- as.factor(client$smoking_status)
  client$education_level              <- as.factor(client$education_level)
  client$housing_category             <- as.factor(client$housing_category)
  client$housing_stability            <- as.factor(client$housing_stability)
  client$resid_support                <- as.factor(client$resid_support)
  client$community_integration        <- as.factor(client$community_integration)
  client$homeless_serv                <- as.factor(client$homeless_serv)
  client$assigned_benefits            <- as.factor(client$assigned_benefits)
  client$date_diagnosis_last_updated  <- as.Date(client$date_diagnosis_last_updated)
  client$date_medication_last_updated <- as.Date(client$date_medication_last_updated)

  return(client)
}


#rattle()

head(clientvisit[(clientvisit$program_id == 9), c(3, 5, 11:13, 17)], 20)

unique(clientvisit$visittype[clientvisit$program_id == 9]

##########################################
# clientvisit - must create client lookup first!
##########################################
prepClientvisit <- function (cv) {
  # de-identify
  index <- client_lookup[match(cv$client_id, client_lookup$c_id), 
                         'research_num'] 
  cv$research_num <- index
  
  keep <- c('research_num',
     # 1-20
     'clientvisit_id', 'program_id', 'visittype_id', 'visittype', 
     'cptcode', 'location_id', 'status', 'rev_timein', 'rev_timeout', 
     'duration', 'non_billable', 
     # 21-40               
     'units_of_svc', 'team_id', 'timein', 'timeout', 
     # 41-60
     'credentials', 'signature_datetime', 'billing_group_id', # only 2800 trs 
     'is_emergency', 'cotherapist', 'unscheduled',
     #61-80
     #81-100 - remove axis codes because mostly null?  
     'MROValidDiag', 'assessment_type', 'axis_code', 'axis_code2', 
     'axis_code3', 'axis_code4', 'axis_code5',
     #101-120
     'location', # add summary_notes for text
     #121-140
     #141-160  - remove icd codes because mostly null?  
     'icd10_code', 'icd10_code2', 'icd10_code3', 'icd10_code4', 
     'icd10_code5'
     #161-171
     )
  clientvisit <- subset(cv, select = keep)
  
  # set data types
  clientvisit$rev_timein <- 
    as.POSIXct(clientvisit$rev_timein, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  clientvisit$rev_timeout <- 
    as.POSIXct(clientvisit$rev_timeout, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  clientvisit$timein <- 
    as.POSIXct(clientvisit$timein, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  clientvisit$timeout <- 
    as.POSIXct(clientvisit$timeout, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  clientvisit$signature_datetime <- 
    as.POSIXct(clientvisit$signature_datetime, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())

  # correct one anomalous revised time in:
  clientvisit$rev_timein[clientvisit$clientvisit_id == 2536013] <- as.POSIXct('2017-03-12 02:19:00')
  
  return(clientvisit)
}

##########################################
# crisisvisit
##########################################
prepCrisisVisit <- function (clientvisit) {
  service_types <- c('CASE MANAGEMENT', 'INDV PSYCHOTHERAPY', 'INDV THERAPY HOME', 
                     'No Show/Cancellation', 'SKILLS TRAINING/INDV', 
                     'SKILLS TRAINING/GRP')
  other_types   <- c('Income Verification', 'Attestation')
  crisis_types  <- c('CM if Client is IP', 'CRISIS - PHONE ONLY', 'Crisis Add On', 
                     'CRISIS INTERVENTION', 'CRISIS INTRVNTN F2F', 'Emergency Call Log',
                     'Hospital Admission', 'EMERGENCY CONTACT')

  keep_rows <- c(service_types, crisis_types)#, other_types)
  keep_cols <- c('clientvisit_id', 'research_num', 'visittype', 'rev_timein', 'units_of_svc')
  
  crisisvisit <- clientvisit[which(clientvisit$visittype %in% keep_rows), keep_cols]
  crisisvisit <- merge(crisisvisit, no_show[,c('clientvisit_id', 'NoShow', 'ClientCancResched', 
       'ClientCancNoResched', 'ClinicianCanc')], 
       by.x = 'clientvisit_id', by.y = 'clientvisit_id', all = TRUE) 

  # remove about 20 anomolous services in warehouse showing no research num.    
  crisisvisit <- crisisvisit[!is.na(crisisvisit$research_num), ]
  
  nosho_types <- c('NoShow', 'ClientCancResched', 'ClientCancNoResched', 'ClinicianCanc')
  crisisvisit[ , nosho_types] <-
    apply(crisisvisit[ , nosho_types], 2, function(x){replace(x, is.na(x), 0)})
  
  crisisvisit$rev_timein <- as.Date(crisisvisit$rev_timein)
  
  return(crisisvisit)                     
}

#no_show[!complete.cases(no_show),]
#crisisvisit[!complete.cases(crisisvisit),]

##########################################
# EOTvisit
##########################################
prepEOTVisit <- function (clientvisit) {
  keep_rows <- c('CASE MANAGEMENT', 'INDV PSYCHOTHERAPY', 'SKILLS TRAINING/INDV')
  keep_cols <- c('research_num', 'visittype', 'program_id', 'rev_timein', 'duration', 
                 'units_of_svc')
  
  EOTvisit <- clientvisit[which(clientvisit$visittype %in% keep_rows), keep_cols]
  
  return(EOTvisit)                     
}

prepEOTVisit_cm <- function (clientvisit) {
  #keep_rows <- c('CASE MANAGEMENT', 'INDV PSYCHOTHERAPY', 'SKILLS TRAINING/INDV')
  keep_cols <- c('research_num', 'visittype', 'program_id', 'rev_timein', 'duration', 
                 'units_of_svc')
  
  EOTvisit <- clientvisit[clientvisit$program_id == 9, keep_cols]
  
  return(EOTvisit)                     
}

##########################################
# ansa
##########################################
prepAnsa <- function (ansa, addLON = TRUE, subsetNulls = FALSE) {
  # de-identify
  index <- client_lookup[match(ansa$client_id, client_lookup$c_id), 
                         'research_num'] 
  ansa$research_num <- index

  keep <- c('research_num',
     #1-20
     'clientvisit_id', 'ReasonCode', 'EpisodeStatus', 
     #'PrimaryLanguage', 'KnowEnglish', 
     'Black', 'NativeAmerican', 'Asian', 'Caucasian', 
     'HawPacificIsle', 'OtherRace', 'Ethnicity', 
     'Religion',
     #21-40
     #'LegalBasisRefer', 'DepChildren', 'Disability', 'Pregnant',
     #'SocialSupport', 'EmployStatus', 
     'EduLevel', #'LivingArrange',
     #41-60
     #'HousingCategory', #'LevelResSupport', 'CommIntegration', 
     #'HomelessServices', 'SuppEmp', 'SuppEmpYesNo', 
     'HousingStability', 
     #'SuppHousing', 'PrimSubst', 'PrimRoute', 'PrimFreq',
     #'PrimAge', 'SecSubst', 
     #61-80
     #'SecRoute', 'SecFreq', 'SecAge', 'TertSubst', 'TertRoute', 'TertFreq', 
     #'TertAge', 'ACTTeam', 
     #81-100
     #'IMR', 'IDDT', 'MotivationalInterview', 'CogBehaveTherp', 'MatrixModel', 
     #'DBT', 'Clubhouse', 'Diabetes', 'CardioDisease', 'Hypertension', 
     #'Hyperlipidemia', 'Cancer', 'Smoking',
     #101-120
     #'Obesity', 'Asthma', 'COPD', 
     'InterviewDate', 
     #'LDPhysMed', 'LDFamily', 'LDEmployment',
     #121-140
     #'LDSocial', 'LDRecreational', 'LDIntellectual', 'LDSexuality', 'LDLifeSkills', 
     #'LDResidential', 'LDLegal', 'LDSleep', 'LDSelfCare', 'LDJudgement',
     #'LDRecovInvolve', 'LDTransport', 'LDMedInvolve', 
     #141-160
     #'STFamily', 'STConnected', 'STOptimism', 'STTalents', 'STEducation', 
     #'STVolunteering', 'STJobHist', 'STSpiritual', 'STConsSpirit', 
     #'STSupports', 'STResiliency', 'STCommLife', 'STResourceful', 
     #'ACLanguage', 'ACIdentity',
     #161-180
     #'ACIdentStressors', 'ACRitual', 'ACCulturalStress', 'JOBAspirations', 'JOBTime',
     #'JOBAttendance', 'JOBPerformance', 'JOBRelations', 'JOBSkills', 'DEVCognitive',
     #'DEVCommunication', 'DEVDevelopmental',
     #181-200
     #'CRISerious', 'CRIHistory', 'CRIArrests', 'CRIPlanning', 'CRICommSafety', 
     #'CRILegalComply', 'CRIPeerInfluence', 'CRIEnvInfluence',
     #201-220
     #'BHTraumaAdj', 'BHAnger', 'BHSubstanceAbuse', 'BHEatingDisturbance',
     'BHPsychosis', 'BHDepression', 'BHAnxiety', 'BHInterpersonal' 
     #'TRASexualAbuse', 'TRAPhysicalAbuse', 'TRAEmotionalAbuse', 'TRAMedical', 
     #'TRANatural', 'TRAWitnessFamily', 'TRAWitnessCommunity', 'TRAWarTerror', 
     #'TRANeglect', 'TRACareAttachLoss', 'TRAAffectReg',
     #221-240
     #'TRAIntrusions', 'TRAAttachment', 'TRADissociation', 'TRAAvoidance', 
     #'TRAGrief', 'SUBSeverity', 'SUBDuration', 'SUBStageOfRecovery', 
     #'SUBPeerInfluence', 'SUBEnvInfluence', 'SUBRecovSupp', 'SUBAcuteIntox', 
     #'SUBWithdrawHx', 'SUBWithdrawRisk', 'SUBAwareness', 'EATUncomfortFull',
     #241-260
     #261-280
     #281-200
     #301-320
     #321-340
     #341-360
     #361-380
     #381-400
     #401-420
     #'EATLostControl', 'EATLost15In3', 'EATBelieveFat',
     #'EATFoodDom', 'RBSuicide', 'RBOthers', 'RBSelfInjury', 'RBOtherSelfHarm',
     #'RBExploitation', 'RBGambling', 'RBSexualAggression', 'RBDangerSelfOther', 
     #'RBCriminal', 'CGPhysBehave', 'CGInvolvement',
     #421-440
     #'CGKnowledge', 'CGSocialResources', 'CGFamilyStress', 'CGSafety', 
     #'SuicideIdeation', 'SuicideIntent', 'SuicidePlanning', 'SuicideHist',
     #'DANIntent', 'DANPlanning', 'DANHist', 'DANFrustration', 'DANHostility',
     #'DANParanoid', 'DANSecondaryGains', 'DANThinking', 'DANAwareness', 
     #'DANResponseCons', 'DANSelfControl', 
     #441-454
     #'DANIdeation', 'DANTreatInvolve', 'SABRelationship', 'SABForceThreat', 
     #'SABPlanning', 'SABAgeDiff', 'SABTypeOfSex', 'SABResponseAcc'
     )
  
  ansa <- subset(ansa, select = keep)
  
  # set data types
  ansa$InterviewDate <- as.Date(ansa$InterviewDate)
  ansa <- ansa[which(ansa$InterviewDate > "2012-01-01"), ]

  # fix anomalous missing values:
  cols_toZero_na <- c('Black', 'NativeAmerican', 'Asian', 'HawPacificIsle', 
                     'OtherRace')
  
  ansa[,cols_toZero_na] <-
    apply(ansa[,cols_toZero_na], 2, function(x){replace(x, is.na(x), 0)})
  
  # manually correct anomalous values in two services; base values off of other services
  # for same clients
  ansa$Ethnicity[ansa$clientvisit_id == 1473390] <- 5
  ansa$Religion[ansa$clientvisit_id == 1473390] <- 1
  ansa$HousingStability[ansa$clientvisit_id == 1473390] <- 4
  ansa$Religion[ansa$clientvisit_id == 1480864] <- 5
  
  # add official LON or 'OptionLevel' to ansa table
  if (addLON == TRUE) {
    assess <- getWHTable('darmha_assess_results', wh_connect)
    assess <- prepAssess(assess)
    ansa <- merge(ansa, assess[,c('AssessID', 'OptionLevel')], 
                  by.x = 'clientvisit_id', by.y = 'AssessID', 
                  all.x = TRUE)
  }
  
  if (subsetNulls == TRUE) {
    ansa <- ansa[which(!is.na(ansa$OptionLevel)), ]
  }
  
  return(ansa)
}

##########################################
# employee
##########################################
prepEmployee <- function (emp) {
  keep    <- c('emp_id', 'emp_status', 'username', 'profile_code', 'credentials', 
               'is_supervisor_flag', # 'license_number', 'npi', 
               'gender', 'hire_date', 'term_date', 'is_prescriber', 'is_nurse', 
               'level_of_clinician',
               'is_intern', 'transfer_date', 'productivity_expectations', 
               'expected_dollars_per_day_fy16')
  employee <- subset(emp, select = keep)
  
  # set data types
  employee$emp_status           <- as.factor(employee$emp_status)
  employee$profile_code         <- as.factor(employee$profile_code)
  employee$gender               <- as.factor(employee$gender)
  employee$hire_date            <- as.Date(employee$hire_date)
  employee$term_date            <- as.Date(employee$term_date)
  employee$transfer_date        <- as.Date(employee$transfer_date)
  
  return(employee)
}


##########################################
# ebs_emp
##########################################
prepEBSemp <- function (ebs_emp) {
  keep    <- c('EmployeeID', 'EmployeeNumber', 'Badge', 'TimeGroupID', 'BirthDate',
               'LastHireDate', 'OriginalHireDate', 'Gender', 'Status', 
               'PositionFlag', 'PositionID', 'TermDate'
               )
  
  ebs_emp <- subset(ebs_emp, select = keep)
  
  # set data types
  ebs_emp$BirthDate <- 
    as.POSIXct(ebs_emp$BirthDate, format="%m/%d/%Y %H:%M:%S %p", tz=Sys.timezone())
  ebs_emp$LastHireDate <- 
    as.POSIXct(ebs_emp$LastHireDate, format="%m/%d/%Y %H:%M:%S %p", tz=Sys.timezone())
  ebs_emp$OriginalHireDate <- 
    as.POSIXct(ebs_emp$OriginalHireDate, format="%m/%d/%Y %H:%M:%S %p", tz=Sys.timezone())
  ebs_emp$TermDate <- 
    as.POSIXct(ebs_emp$TermDate, format="%m/%d/%Y %H:%M:%S %p", tz=Sys.timezone())
  
  return(ebs_emp)
}


##########################################
# loginlog
##########################################
prepLoginlog <- function (loginlog) {
  keep    <- c('loginlog_id', 'username', 'logindatetime', 'in_out', 'ip_address', 
               'ser_ver', 'user_agent', 'user_id')
  loginlog <- loginlog[which(loginlog$is_client == 0),]
  loginlog <- subset(loginlog, select = keep)
  
  # set data types
  loginlog$logindatetime <- 
    as.POSIXct(loginlog$logindatetime, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  
  return(loginlog)
}

##########################################
# medhist
##########################################
prepMedHist <- function (medhist) {
  # de-identify
  index <- client_lookup[match(medhist$client_id, client_lookup$c_id), 
                         'research_num'] 
  medhist$research_num <- index

  keep    <- c('research_num', 
               # 1-20
               'med_id', #'provider_id_int', 'fdb_medid', 
               'medication', 'dosage',
               #'frequency', 'start_date', 'chg_date', 'disc_date', 'rationale', 
               #'is_current', 'quantity', 'num_refill', 'instructions',
               # 21-40
               'date_created', 
               #'date_updated', 'is_rx', 'is_prescription', 'rx_sig', 
               'rx_status' #, 'rx_status_text', 'dosage_action_string', 
               #'dosage_quantity', 'route_string', 'route_per', 'route_time', 
               #'quantity_unit_string', 'dosage_string',
               # 41-60
               #'change_action', 'daw', 'addtl_comments', 'refillnotes',
               #'sig_comments', 'dayssupply', 'drug_classification', 'notes', 
               #'drug_schedule',
               # 61-70
               #'rx_norm_name', 'rx_norm_class_code', 'tm_medication'
               )  

  medhist <- subset(medhist, select = keep)

  # set data types
  #medhist$start_date <- 
  #  as.POSIXct(medhist$start_date, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  #medhist$chg_date <- 
  #  as.POSIXct(medhist$chg_date, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  #medhist$disc_date <- 
  #  as.POSIXct(medhist$disc_date, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  medhist$date_created <- 
    as.POSIXct(medhist$date_created, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  #medhist$date_updated <- 
  #  as.POSIXct(medhist$date_updated, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())

  good_rx = c('A', 'C', 'CC', 'EC', 'A', 'PC', 'FC')
  medhist <- medhist[which(medhist$rx_status %in% good_rx) ,]
    
  return(medhist)
}

##########################################
# empexit
##########################################
prepEmpExit <- function (empx) {
  keep    <- c('RespID', 'EndSurv', 'EmpID',
               'PosCat', 'Location', 'HireDate', 
               'TermDate', 'HighEdu', 
               # reasons for leaving
               'Adv', 'DiffWork', 'Pay', 'ConfEmp',
               'School', 'FamCirc', 'Stress', 'Vehicle', 'SelfEmploy', 'Illness', 
               'Commute', 'Supervision', 'Relocate', 'ConfSup', 'Stats', 'Doc', 
               'Retiring', 
               # supervisor ratings
               'SupPol', 'SupInf', 'SupFair', 'SupRec', 'SupCoop', 'SupResolve', 
               'SupTrn', 
               # department ratings
               'DeptCom', 'DeptCond', 'DeptCoop', 'DeptAdv',
               # expectations ratings
               'Expect', 'LikeJob', 'AmtWork', 
               # benefits ratings
               'RatePay', 'AnnLeave', 'PdHoliday', 'DevEdu', 'MDVIns', 
               'SLLifeIns', 'Retire', 
               # open-ended responses (for text analysis only):
               #'RsnOther', 'RsnCmt', 'SupCmt', 'LikeJobCmt', 'BetterAreas', 
               #'DoDifferent', 'AdComment', 'RecExplain',
               
               # response variable
               'Recommend'
               )
  emp_exit <- subset(empx, select = keep)
  
  # set data types
  emp_exit$EndSurv  <- 
    as.POSIXct(emp_exit$EndSurv, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())

  # convert reasons for leaving into bools:  
  reas_for_leave <- c('Adv', 'DiffWork', 'Pay', 'ConfEmp', 'School', 'FamCirc',
     'Stress', 'Vehicle', 'SelfEmploy', 'Illness', 'Commute', 'Supervision',
     'Relocate', 'ConfSup', 'Stats', 'Doc', 'Retiring')
  
  emp_exit[,reas_for_leave] <-
    apply(emp_exit[,reas_for_leave], 2, function(x){replace(x, is.na(x), 0)})

  emp_exit[,reas_for_leave] <-
    apply(emp_exit[,reas_for_leave], 2, function(x){replace(x, x > 0.5, 1)})

  # treat missing values for ratings  
  sup_means <- 
    mapply(sup_mean, SupPol = emp_exit$SupPol, SupInf = emp_exit$SupInf, 
           SupFair = emp_exit$SupFair, SupRec = emp_exit$SupRec, 
           SupCoop = emp_exit$SupCoop, SupResolve = emp_exit$SupResolve, 
           SupTrn = emp_exit$SupTrn)
  
  emp_exit$SupPol[is.na(emp_exit$SupPol)] <- sup_means[is.na(emp_exit$SupPol)]
  emp_exit$SupInf[is.na(emp_exit$SupInf)] <- sup_means[is.na(emp_exit$SupInf)]
  emp_exit$SupFair[is.na(emp_exit$SupFair)] <- sup_means[is.na(emp_exit$SupFair)]
  emp_exit$SupRec[is.na(emp_exit$SupRec)] <- sup_means[is.na(emp_exit$SupRec)]
  emp_exit$SupCoop[is.na(emp_exit$SupCoop)] <- sup_means[is.na(emp_exit$SupCoop)]
  emp_exit$SupResolve[is.na(emp_exit$SupResolve)] <- sup_means[is.na(emp_exit$SupResolve)]
  emp_exit$SupTrn[is.na(emp_exit$SupTrn)] <- sup_means[is.na(emp_exit$SupTrn)]

  dept_means <- 
    mapply(dept_mean, DeptCom = emp_exit$DeptCom, DeptCond = emp_exit$DeptCond, 
           DeptCoop = emp_exit$DeptCoop, DeptAdv = emp_exit$DeptAdv)
  
  emp_exit$DeptCom[is.na(emp_exit$DeptCom)] <- dept_means[is.na(emp_exit$DeptCom)]
  emp_exit$DeptCond[is.na(emp_exit$DeptCond)] <- dept_means[is.na(emp_exit$DeptCond)]
  emp_exit$DeptCoop[is.na(emp_exit$DeptCoop)] <- dept_means[is.na(emp_exit$DeptCoop)]
  emp_exit$DeptAdv[is.na(emp_exit$DeptAdv)] <- dept_means[is.na(emp_exit$DeptAdv)]
  
  exp_means <- 
    mapply(exp_mean, Expect = emp_exit$Expect, LikeJob = emp_exit$LikeJob, 
           AmtWork = emp_exit$AmtWork)
  
  emp_exit$Expect[is.na(emp_exit$Expect)] <- exp_means[is.na(emp_exit$Expect)]
  emp_exit$LikeJob[is.na(emp_exit$LikeJob)] <- exp_means[is.na(emp_exit$LikeJob)]
  emp_exit$AmtWork[is.na(emp_exit$AmtWork)] <- exp_means[is.na(emp_exit$AmtWork)]
  
  ben_means <- 
    mapply(ben_mean, RatePay = emp_exit$RatePay, AnnLeave = emp_exit$AnnLeave, 
           PdHoliday = emp_exit$PdHoliday, DevEdu = emp_exit$DevEdu, 
           MDVIns = emp_exit$MDVIns, SLLifeIns = emp_exit$SLLifeIns, 
           Retire = emp_exit$Retire)
  
  emp_exit$RatePay[is.na(emp_exit$RatePay)] <- ben_means[is.na(emp_exit$RatePay)]
  emp_exit$AnnLeave[is.na(emp_exit$AnnLeave)] <- ben_means[is.na(emp_exit$AnnLeave)]
  emp_exit$PdHoliday[is.na(emp_exit$PdHoliday)] <- ben_means[is.na(emp_exit$PdHoliday)]
  emp_exit$DevEdu[is.na(emp_exit$DevEdu)] <- ben_means[is.na(emp_exit$DevEdu)]
  emp_exit$MDVIns[is.na(emp_exit$MDVIns)] <- ben_means[is.na(emp_exit$MDVIns)]
  emp_exit$SLLifeIns[is.na(emp_exit$SLLifeIns)] <- ben_means[is.na(emp_exit$SLLifeIns)]
  emp_exit$Retire[is.na(emp_exit$Retire)] <- ben_means[is.na(emp_exit$Retire)]
  
  # remove observations for which response is null
  emp_exit <- emp_exit[!is.na(emp_exit$Recommend), ]
  
  return(emp_exit)
}

sup_mean <- function (SupPol, SupInf, SupFair, SupRec, SupCoop, SupResolve, SupTrn) {
  sups <- c(SupPol, SupInf, SupFair, SupRec, SupCoop, SupResolve, SupTrn)
  sup_mean <- round(mean(sups, na.rm=TRUE))
  return(sup_mean)
}

dept_mean <- function (DeptCom, DeptCond, DeptCoop, DeptAdv) {
  depts <- c(DeptCom, DeptCond, DeptCoop, DeptAdv)
  dept_mean <- round(mean(depts, na.rm=TRUE))
  return(dept_mean)
}

exp_mean <- function (Expect, LikeJob, AmtWork) {
  exp <- c(Expect, LikeJob, AmtWork)
  exp_mean <- round(mean(exp, na.rm=TRUE))
  return(exp_mean)
}

ben_mean <- function (RatePay, AnnLeave, PdHoliday, DevEdu, MDVIns, SLLifeIns, Retire) {
  bens <- c(RatePay, AnnLeave, PdHoliday, DevEdu, MDVIns, SLLifeIns, Retire)
  ben_mean <- round(mean(bens, na.rm=TRUE))
  return(ben_mean)
}


##########################################
# supexit
##########################################
prepSupExit <- function (supx) {
  keep    <- c('RespID', 'EndSurv', 'SupEmpID', 'EmpID', 'Location', 'LocOther', 

               # open-ended responses:
               #'OtherCmt',
               
               'FolThru', 'TmWrk', 'Growth', 'Resil', 'BPMindset', 'Attitude', 
               'Continuity', 'Resources', 'Engaging', 'StrBased', 'Productivity', 
               'Timeliness'
  )
  sup_exit <- subset(supx, select = keep)
  
  # set data types
  sup_exit$EndSurv  <- 
    as.POSIXct(sup_exit$EndSurv, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  
  return(sup_exit)
}

##########################################
# diagnosis
##########################################
prepDiagnosis <- function (diagnosis) {
  # de-identify
  index <- client_lookup[match(diagnosis$client_id, client_lookup$c_id), 
                         'research_num'] 
  diagnosis$research_num <- index
  
  keep    <- c('research_num',   
               'effdate', 'gaf', 'Prob1')
  
  diagnosis <- subset(diagnosis, subset = (!is.na(diagnosis$Prob1)), select = keep)
  diagnosis$effdate <- as.Date(diagnosis$effdate)
  
  return(diagnosis)
}

##########################################
# no_show
##########################################
prepNoShow <- function (nosho) {
  
  keep <- c('clientvisit_id', 'NoShow', 'ClientCancResched', 'ClientCancNoResched',
    'ClinicianCanc', 'date_updated')
  nosho <- subset(nosho, select = keep)
  
  # set data types
  nosho$EndSurv  <- 
    as.POSIXct(nosho$date_updated, format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  return(nosho)
}


##########################################
# payroll
##########################################
prepPayroll <- function (payroll) {

  # set data types
  payroll$AllocDate <- as.Date(payroll$AllocDate)

  return(payroll)
}

##########################################
# phq9
##########################################
prepPHQ9 <- function (phq9) {
  # de-identify
  index <- client_lookup[match(phq9$client_id, client_lookup$c_id), 
                         'research_num'] 
  phq9$research_num <- index
  
  keep    <- c('research_num', 'clientvisit_id', 'LowInterest', 'Depressed', 
               'SleepDisturbance', 'FeelTired', 'EatDisturbance', 'LowSelfEsteem', 
               'PoorConcentration', 'PsychoMotRetardation', 'SelfHarmIdeation', 
               'HowDifficult', 'IsInitial', #'StressorsHealth', 
               'TotalScore' 
               )
  phq9 <- subset(phq9, select = keep)
  
  phq9$serv_date <- clientvisit[match(phq9$clientvisit_id, 
        clientvisit$clientvisit_id), 'rev_timein']
  
  return(phq9)
}


##########################################
# therapy note
##########################################
prepTherpNote <- function (therp) {
  # de-identify
  index <- client_lookup[match(therp$client_id, client_lookup$c_id), 
                         'research_num'] 
  therp$research_num <- index
  
  keep    <- c('research_num', 
               # 1-20
               'clientvisit_id', 'Suicide', 'Homicidal', 'AcuteMStatus', 
               'GraveDisab', 'Risk_text', 'Risk_plan', 'Present_Client', 
               # 21-39
               'Present_text', 'Symptoms', 'Intervention', 'Response', 'Plan'
               )
  therp <- subset(therp, select = keep)

  return(therp)
}

##########################################
# progress note
##########################################
prepProgNote <- function (prog) {
  # de-identify
  index <- client_lookup[match(prog$client_id, client_lookup$c_id), 
                         'research_num'] 
  prog$research_num <- index

  keep    <- c('research_num',
               'clientvisit_id', 'Suicidal', 'Suicidal_text', 'Present_Client',
               'Present_text', 'Symptoms', 'Intervention', 'Response', 'Plan'
               )
  prog <- subset(prog, select = keep)

  return(prog)
}

##########################################
# assessment
##########################################
prepAssess <- function (assess) {
  assess <- assess[which(assess$Alg == 11), ]
  assess$VisitDate <- as.Date(assess$VisitDate)
  assess <- assess[which(assess$VisitDate > "2012-12-01"), ]
  assess$AssessID <- as.numeric(assess$AssessID)
  assess <- assess[which(assess$AssessID < 100000000000), ]
                                head(as.numeric(assess$AssessID))
  keep    <- c('VisitDate', 'AssessID', 'ClinicianID', 
               # 'ToolDesc', 'ToolID', 
               # 'AlgorithmDesc', 'Alg', 'ACT', 'IMR', 'IDDT', 'MotInt', 'CBT', 
               #'Matrix', 'DBT', 'CH', 'PeerSupport', 'Location',
               # response
               'OptionLevel'
               #, 'OptionDesc'
  )
  assess <- subset(assess, select = keep)
  
  # located rows which had duplicate visit IDs and one that was a complete duplicate.
  #duplicates <- (c(67773, 130395, 135009, 143620, 150418, 218833, 260185, 
  #                 1529590, 1650822, 1651934, 1668413, 2062418))
  #ansa_lon[which(ansa_lon$clientvisit_id %in% duplicates), ]
  #dups <- assess[which(assess$AgencyAssessID %in% duplicates), ]
  #dups <- dups[order(dups$AgencyAssessID),]
  #dim(dups)
  
  # correct errors from state database
  assess <- assess[-which((assess$VisitDate == '2013-01-21' & assess$AssessID == 67773) |
                  (assess$VisitDate == '2013-02-28' & assess$AssessID == 130395) | 
                  (assess$VisitDate == '2013-04-02' & assess$AssessID == 130395) | 
                  (assess$VisitDate == '2013-03-04' & assess$AssessID == 135009) | 
                  (assess$VisitDate == '2013-03-08' & assess$AssessID == 143620) | 
                  (assess$VisitDate == '2013-03-10' & assess$AssessID == 150418) |
                  (assess$VisitDate == '2013-04-22' & assess$AssessID == 218833) |
                  (assess$VisitDate == '2013-05-20' & assess$AssessID == 260185) | 
                  (assess$ClinicianID == 3775       & assess$AssessID == 1650822) |
                  (assess$VisitDate == '2015-07-01' & assess$AssessID == 1651934) | 
                  (assess$VisitDate == '2015-10-14' & assess$AssessID == 1668413) | 
                  (assess$VisitDate == '2016-05-26' & assess$AssessID == 2062418)
                  ), ]
  
  assess <- assess[-duplicated(assess), ]
  
  return(assess)
}

##########################################
#
# 01_emp_churn.R
#
# Dan Diamond
# 9/26/18
#
# Analyze employee churn.  
#
##########################################

#rm(list=ls())

#install.packages('caret')
library('caret')

source("00_load.R")
source("00_clean_funcs.R")
source("00_help_funcs.R")
source("00_eot_build.R")

##########################################
# get, clean, and subset tables
##########################################
wh_connect <- connect_to_wh()
hr_connect <- connect_to_hr()
exit_connect <- connect_to_exit()

# employee
employee <- getWHTable('employees', wh_connect)
employee <- prepEmployee(employee)

# exit interview
emp_exit <- getLocalTable('empexit', exit_connect)
emp_exit <- prepEmpExit(emp_exit)
emp_exit[!complete.cases(emp_exit),]

#write.csv(emp_exit, file = "G:/888/final_frames/emp_exit.csv")

# data exploration 
emp_exit <- read.csv("G:/888/final_frames/emp_exit.csv")

str(emp_exit)

par(mfrow=c(3,2), oma=c(0, 0, 2, 0))
par(las=1)
par(mar=c(5, 12, 4, 2))
barplot(rev(rsnPrc), 
        xlim=c(0,100), horiz = TRUE, col = "red",
        names.arg=rev(c("Advancement", "Different Work", "Pay", "Employee Conflict",
                        "School", "Family Circumstances", "Stress", "Vehicle Wear", 
                        "Self-Employment", "Illness", "Commute", "Supervision", "Relocating", 
                        "Supervisor Conflict", "Stats/Exp", "Documentation", "Retiring", "Other")), 
        main="Reasons Cited For Leaving",
        sub = "(% of Employees who Cited)")

supRatings <- c('SupPol', 'SupInf', 'SupFair', 'SupRec', 'SupCoop', 'SupResolve', 'SupTrn')
supRate <- emp_exit[ , supRatings]
supAvg  <- apply(supRate, 2, function(x) mean(x))

par(mar=c(5, 8, 4, 1)+.1)
barplot(rev(supAvg), xlim=c(1,5), horiz = TRUE, col = "green", xpd = FALSE, las = 1,
        names.arg=rev(c("Policies", "Informed", "Fairness", "Recognition", "Cooperation", "Resolve Probs", "Training")), 
        main="Average Supervisor Ratings")


depRatings <- c('DeptCom', 'DeptCond', 'DeptCoop', 'DeptAdv')
depRate <- emp_exit[ , depRatings]
depAvg  <- apply(depRate, 2, function(x) mean(x))
par(mar=c(5, 8, 4, 1)+.1)
barplot(rev(depAvg), xlim=c(1,5), horiz = TRUE, col = "blue", xpd = FALSE, las = 1,
        names.arg=rev(c("Communication", "Conditions", "Cooperation", "Advancement")), 
        main="Average Department Ratings")

benRatings <- c('RatePay', 'AnnLeave', 'PdHoliday', 'DevEdu', 'MDVIns', 'SLLifeIns', 'Retire')
benRate <- emp_exit[ , benRatings]
benAvg  <- apply(benRate, 2, function(x) mean(x))
par(mar=c(5, 8, 4, 1)+.1)
barplot(rev(benAvg), xlim=c(1,5), horiz = TRUE, col = "dark cyan", xpd = FALSE, las = 1,
        names.arg=rev(c("Pay", "Annual Leave", "Holidays", "Dev/Edu", "Med/Vis/Dent Ins", "Life Ins", "Retirement")), 
        main="Average Benefits Ratings")

#barplot(rev(RecPrc), xlim=c(0,100), horiz = TRUE, col = "light blue",
#        names.arg=rev(c("Recommend?")), 
#        main="Recommend WVA to Friend?",
#        sub = "(% of Employees Answering 'Yes')")
#barplot(rev(expPrc), xlim=c(0,100), horiz = TRUE, col = "dark green",
#        names.arg=rev(c("What expected?", "Liked Job?", "Amount Work?")), 
#        main="Expectations",
#        sub = "(% of Employees Answering 'Yes')")


# feature selection - empexit
head(emp_exit)

set.seed(42)
emp_exit_predictors <- c(5, 6, 9:47)
emp_exit_resp   <- c(48)

emp_exit_final <- emp_exit[ , c(emp_exit_predictors, emp_exit_resp)]
head(emp_exit_final)

bor_emp_exit   <- Boruta(Recommend~., data=emp_exit_final, doTrace = 2, maxRun = 1000)
print(bor_emp_exit)

par(mar = c(7, 4, 4, 2) + 0.1)
plot(bor_emp_exit, xlab = "", las = 2, main="Important Features - Employee Exit")

bor_emp_exit_tfix   <- TentativeRoughFix(bor_emp_exit)
print(bor_emp_exit_tfix)
attStats(bor_emp_exit_tfix)
getNonRejectedFormula(bor_emp_exit)
getConfirmedFormula(bor_emp_exit)


# ebs employee
ebs_emp  <- read.csv("G:/888/data/hEmployee.csv")
ebs_emp  <- prepEBSemp(ebs_emp)

# budget_detail  
payroll <- getHRTable('biactual', hr_connect)
payroll <- prepPayroll(payroll)
#head(payroll)
#str(payroll)

dbDisconnectAll()

##########################################
#
# 02_utilization.R
#
# Dan Diamond
# 9/26/18
#
# Analyze consumer utilization of services.  
#
##########################################

rm(list=ls())

source("00_load.R")
source("00_clean_funcs.R")

##########################################
# get, clean, and subset tables
##########################################

wh_connect <- connect_to_wh()

# client
client              <- getWHTable('clients', wh_connect)
client_lookup       <- c_lookup_df(client)
client$research_num <- client_lookup$research_num
client              <- prepClient(client)

# clientvisit
clientvisit <- getWHTable('clientvisit', wh_connect)
clientvisit <- prepClientvisit(clientvisit)

head(clientvisit[which(clientvisit$visittype == 'DV - COURT APPEAR'), ])

# ansa
ansa <- getWHTable('ansa_form', wh_connect)
ansa <- prepAnsa(ansa)

# diagnosis
diagnosis <- getWHTable('diagnosis_form', wh_connect)
diagnosis <- prepDiagnosis(diagnosis)



ansa_cm_wk <- EOT_ansa
ansa_cm_wk <- ansa_cm_wk[ansa_cm_wk$cm_per_wk < 1.75, ]
ansa_cm_wk <- ansa_cm_wk[ansa_cm_wk$cm_per_wk > 0.1, ]
EOT_ansa_plot <- ansa_cm_wk

ansa_thrp_wk <- EOT_ansa
ansa_thrp_wk <- ansa_thrp_wk[ansa_thrp_wk$therp_per_wk < 0.6, ]
ansa_thrp_wk <- ansa_thrp_wk[ansa_thrp_wk$therp_per_wk > 0.1, ]
EOT_ansa_plot <- ansa_thrp_wk



ansa_std_wk <- EOT_ansa
ansa_std_wk <- EOT_ansa[ansa_std_wk$std_per_wk < 10, ]
ansa_std_wk <- ansa_std_wk[ansa_std_wk$std_per_wk > 0.01, ]
EOT_ansa_plot <- ansa_std_wk
p02 <- EOT_ansa_plot %>%
  dplyr::mutate(diag_cat=as.factor(diag_cat)) %>%
  dplyr::select(std_per_wk, diag_cat) %>%
  ggplot2::ggplot(ggplot2::aes(x=std_per_wk)) +
  
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(colour=diag_cat), alpha=0.55, lwd = 7) +
  ggplot2::xlab("Average Units per Week") +
  ggplot2::ggtitle("Use of Skills Training Services\nby Diagnosis Category") +
  #ggplot2::labs( y="Density")
  ggplot2::labs(fill=diag_cat, y="Density")
gridExtra::grid.arrange(p02)

std_lon3_wk <- 600/(180/7)
std_lon4_wk <- 750/(180/7)
std_lon5_wk <- 900/(180/7)

head(EOT_ansa_plot)

ansa_std_wk <- EOT_ansa
ansa_std_wk <- EOT_ansa[ansa_std_wk$std_per_wk < 40, ]
ansa_std_wk <- ansa_std_wk[ansa_std_wk$std_per_wk > 0.6, ]
EOT_ansa_plot_LON <- ansa_std_wk[ansa_std_wk$LON_start > 2,]
pLON <- EOT_ansa_plot_LON %>%
  dplyr::mutate(LON_start=as.factor(LON_start)) %>%
  dplyr::select(std_per_wk, LON_start) %>%
  ggplot2::ggplot(ggplot2::aes(x=std_per_wk)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(colour=LON_start), alpha=0.55, lwd = 1) +
  #ggplot2::geom_density(ggplot2::aes(fill=LON_start, colour=LON_start), alpha=0.55) +
  ggplot2::xlab("Average Units per Week") +
  ggplot2::ggtitle("Use of Skills Training Services\nby Level Of Need") +
  ggplot2::labs(fill="LON_start", y="Density") +
  ggplot2::geom_vline(xintercept=std_lon3_wk, color="coral3", size = 1, linetype="dashed") +
  ggplot2::geom_vline(xintercept=std_lon4_wk, color="green3", size = 1, linetype="dashed") +
  ggplot2::geom_vline(xintercept=std_lon5_wk, color="sky blue3", size = 1, linetype="dashed")

gridExtra::grid.arrange(pLON)


# reference
pLON <- EOT_ansa_plot %>%
  dplyr::mutate(LON_start=as.factor(LON_start)) %>%
  #dplyr::select(cm_per_wk, LON_start) %>%
  #ggplot2::ggplot(ggplot2::aes(x=cm_per_wk)) + 
  
  #dplyr::select(therp_per_wk, LON_start) %>%
  #ggplot2::ggplot(ggplot2::aes(x=therp_per_wk)) +
  
  dplyr::select(std_per_wk, LON_start) %>%
  ggplot2::ggplot(ggplot2::aes(x=std_per_wk, group=1)) +
  
  ggplot2::geom_boxplot(lty=3, group=1) +
  ggplot2::geom_boxplot(ggplot2::aes(fill=LON_start, colour=LON_start, group=1), alpha=0.55) +
  ggplot2::xlab("cm_per_wk\n\nRattle 2018-Oct-23 17:49:34 op97dan") +
  ggplot2::ggtitle("Distribution of cm_per_wk (sample)\nby diag_cat") +
  ggplot2::labs(fill="LON_start", y="Density")
gridExtra::grid.arrange(pLON)

ansa_std_wk <- EOT_ansa
ansa_std_wk <- EOT_ansa[ansa_std_wk$std_per_wk < 25, ]
ansa_std_wk <- ansa_std_wk[ansa_std_wk$std_per_wk > 0.1, ]
EOT_ansa_plot_LON <- ansa_std_wk[ansa_std_wk$LON_start > 2,]
boxplot(std_per_wk~as.factor(LON_start), data=EOT_ansa_plot_LON, xlab="LON",
        ylab="15-min Units", main="Use of Skills Training Services per Week",
        col=(c('coral3', 'green3', 'sky blue3')))


# not used
boxplot(std_per_wk~as.factor(LON_start), data=EOT_ansa_plot, xlab="LON",
        ylab="STD/wk", main="Units of skills training per week",
        col=(c('gray', 'red', 'blue', 'green', 'yellow', 'orange')))

# not used
boxplot(cm_per_wk~as.factor(LON_start), data=EOT_ansa_plot, xlab="LON",
        ylab="CM/wk", main="Units of case management per week",
        col=(c('gray', 'red', 'blue', 'green', 'yellow', 'orange')))

# not used
boxplot(therp_per_wk~as.factor(LON_start), data=EOT_ansa_plot, xlab="LON",
        ylab="Therp/wk", main="Units of therapy per week",
        col=(c('gray', 'red', 'blue', 'green', 'yellow', 'orange')))



dbDisconnectAll()

##########################################
#
# 03_improvement.R
#
# Dan Diamond
# 9/25/18
#
# Perform analysis on client improvement.
#
##########################################

# rm(list=ls())

source("00_load.R")
source("00_clean_funcs.R")
source("00_help_funcs.R")
source("00_eot_build.R")

#install.packages("RColorBrewer")
library("RColorBrewer")
#install.packages("Boruta")
library(Boruta)

##########################################
# get, clean, and subset tables
##########################################

wh_connect <- connect_to_wh()

# client
client              <- getWHTable('clients', wh_connect)
client_lookup       <- c_lookup_df(client)
client$research_num <- client_lookup$research_num
client              <- prepClient(client)

# clientvisit
clientvisit <- getWHTable('clientvisit', wh_connect)
clientvisit <- prepClientvisit(clientvisit)

# medhist 
medhist <- getWHTable('medhist', wh_connect)
medhist <- prepMedHist(medhist)

# ansa
ansa <- getWHTable('ansa_form', wh_connect)
# ansa <- ansa[1:6000,] # subset for testing
ansa <- prepAnsa(ansa, addLON = TRUE, subsetNulls = TRUE)
EOT_ansa <- build_eot_ansa(ansa)
EOT_ansa <- add_meds_eot_ansa(EOT_ansa)
EOT_vis <- prepEOTVisit(clientvisit)
EOT_ansa <- add_services_eot(EOT_ansa)
EOT_ansa <- add_lasts_eot_ansa(EOT_ansa)
EOT_ansa <- add_firsts_eot_ansa(EOT_ansa)
EOT_ansa <- add_serv_diff_eot(EOT_ansa)


EOT_ansa$improved[EOT_ansa$diff_LON < 0] <- 1
EOT_ansa$improved[EOT_ansa$diff_LON >= 0] <- 0

EOT_ansa$imp_psych[EOT_ansa$diff_psych < 0] <- 1
EOT_ansa$imp_psych[EOT_ansa$diff_psych >= 0] <- 0

EOT_ansa$LON_dir[EOT_ansa$diff_LON < 0] <- 1
EOT_ansa$LON_dir[EOT_ansa$diff_LON == 0] <- 0
EOT_ansa$LON_dir[EOT_ansa$diff_LON > 0] <- -1

EOT_ansa$psych_dir[EOT_ansa$diff_psych < 0] <- 1
EOT_ansa$psych_dir[EOT_ansa$diff_psych == 0] <- 0
EOT_ansa$psych_dir[EOT_ansa$diff_psych > 0] <- -1

EOT_ansa$any_dir[EOT_ansa$sum_diffs < 0] <- 1
EOT_ansa$any_dir[EOT_ansa$sum_diffs == 0] <- 0
EOT_ansa$any_dir[EOT_ansa$sum_diffs > 0] <- -1


EOT_ansa$any_imps <- mapply(any_ansa_imps, diff_psych = EOT_ansa$diff_psych, 
  diff_depr = EOT_ansa$diff_depr, diff_anx = EOT_ansa$diff_anx, 
  diff_interpers = EOT_ansa$diff_interpers, 
  diff_hous_stab = EOT_ansa$diff_hous_stab)

EOT_ansa$diag_cat[is.na(EOT_ansa$diag_cat)] <- 'Unknown'
EOT_ansa[!complete.cases(EOT_ansa),]
write.csv(EOT_ansa[complete.cases(EOT_ansa),], file = "G:/888/recover/EOT_ansa_imp.csv")

# data exploration
EOT_ansa <- read.csv("G:/888/final_frames/EOT_ansa.csv") # changed from EOT_ansa_imp.csv

head(EOT_ansa)
summary(as.factor(EOT_ansa$LON_dir))
summary(as.factor(EOT_ansa$psych_dir))
summary(as.factor(EOT_ansa$any_imps))
summary(as.factor(EOT_ansa$any_dir))

#hist(EOT_ansa$diff_LON, 
#     col='light blue', xlab="", 
#     main='Change in LON During Episode')

summary(as.factor(EOT_ansa$improved))
summary(as.factor(EOT_ansa$imp_psych))
summary(as.factor(EOT_ansa$any_imps))
summary(as.factor(EOT_ansa$diag_cat))

summary(as.factor(EOT_ansa$took_risp))
summary(as.factor(EOT_ansa$took_cloz))
summary(as.factor(EOT_ansa$took_proz))
summary(as.factor(EOT_ansa$took_well))
summary(as.factor(EOT_ansa$took_lexa))

summary(as.factor(EOT_ansa$first_risp))
summary(as.factor(EOT_ansa$first_cloz))
summary(as.factor(EOT_ansa$first_proz))
summary(as.factor(EOT_ansa$first_well))
summary(as.factor(EOT_ansa$first_lexa))

took_r <- sum(EOT_ansa$took_risp)
took_c <- sum(EOT_ansa$took_cloz)
took_p <- sum(EOT_ansa$took_proz)
took_w <- sum(EOT_ansa$took_well)
took_l <- sum(EOT_ansa$took_lexa)

first_r <- sum(EOT_ansa$first_risp)
first_c <- sum(EOT_ansa$first_cloz)
first_p <- sum(EOT_ansa$first_proz)
first_w <- sum(EOT_ansa$first_well)
first_l <- sum(EOT_ansa$first_lexa)

risp_lon_imp    <- improve_rate(EOT_ansa$first_risp, EOT_ansa$improved)
risp_psych_imp  <- improve_rate(EOT_ansa$first_risp, EOT_ansa$imp_psych)
risp_any_imp    <- improve_rate(EOT_ansa$first_risp, EOT_ansa$any_imps)

cloz_lon_imp    <- improve_rate(EOT_ansa$first_cloz, EOT_ansa$improved)
cloz_psych_imp  <- improve_rate(EOT_ansa$first_cloz, EOT_ansa$imp_psych)
cloz_any_imp    <- improve_rate(EOT_ansa$first_cloz, EOT_ansa$any_imps)

proz_lon_imp    <- improve_rate(EOT_ansa$first_proz, EOT_ansa$improved)
proz_psych_imp  <- improve_rate(EOT_ansa$first_proz, EOT_ansa$imp_psych)
proz_any_imp    <- improve_rate(EOT_ansa$first_proz, EOT_ansa$any_imps)

well_lon_imp    <- improve_rate(EOT_ansa$first_well, EOT_ansa$improved)
well_psych_imp  <- improve_rate(EOT_ansa$first_well, EOT_ansa$imp_psych)
well_any_imp    <- improve_rate(EOT_ansa$first_well, EOT_ansa$any_imps)

lexa_lon_imp    <- improve_rate(EOT_ansa$first_lexa, EOT_ansa$improved)
lexa_psych_imp  <- improve_rate(EOT_ansa$first_lexa, EOT_ansa$imp_psych)
lexa_any_imp    <- improve_rate(EOT_ansa$first_lexa, EOT_ansa$any_imps)

risp_row <- c(risp_lon_imp, risp_psych_imp, risp_any_imp)
cloz_row <- c(cloz_lon_imp, cloz_psych_imp, cloz_any_imp)
proz_row <- c(proz_lon_imp, proz_psych_imp, proz_any_imp)
well_row <- c(well_lon_imp, well_psych_imp, well_any_imp)
lexa_row <- c(lexa_lon_imp, lexa_psych_imp, lexa_any_imp)

med_improve <- matrix(c(risp_row, cloz_row, proz_row, well_row, lexa_row), 
                      nrow = 5, ncol = 3, byrow = TRUE)

rownames(med_improve) <- c("Risperdal", "Clozaril", "Prozac", "Wellbutrin", "Lexapro")
colnames(med_improve) <- c("LON", "Psychosis", "Any")

barplot(med_improve,  
        col = brewer.pal(5, name = 'Dark2'),
        border="black", font.axis=2, beside=T, legend=rownames(med_improve), 
        args.legend=list(x=6, y=55),
        main="% of Time Improvement Co-occurs When Adding a Medication", font.lab=2)

hist(EOT_ansa$std_per_wk)
hist(EOT_ansa$std_per_wk[EOT_ansa$std_per_wk < 10])

#summary(EOT_ansa$std_per_wk)
#nrow(EOT_ansa[EOT_ansa$std_per_wk > 70, ])
EOT_ansa$ep_length[EOT_ansa$std_per_wk > 70]
summary(EOT_ansa$ep_length[EOT_ansa$std_per_wk > 70])
hist(EOT_ansa$std_per_wk[EOT_ansa$std_per_wk > 0.6 & EOT_ansa$std_per_wk < 20], 
     col='green', xlab="15-minute Units/Week", 
     main='Client Use of Skills Training Services')


hist(EOT_ansa$std_per_wk[EOT_ansa$std_per_wk > 0.6 & EOT_ansa$std_per_wk < 70 & EOT_ansa$LON_start == 5], 
     col='blue', xlab="15-minute Units/Week", breaks = 30, 
     main='Client Use of Skills Training Services - LON 5')
abline(v = 35, col = 'red', lwd = 2, lty = 2)

wks_ep <- 180/7

hist(EOT_ansa$therp_per_wk)
hist(EOT_ansa$therp_per_wk[EOT_ansa$therp_per_wk < 1.3], 
     col='red', xlab="1-hour Units/Week", 
     main='Client Use of Therapy Services')

summary(EOT_ansa$std_per_wk[EOT_ansa$LON_start==5])
EOT_ansa_non_zero <- EOT_ansa[EOT_ansa$std_per_wk > 0.3,]
mean(EOT_ansa_non_zero$std_per_wk[EOT_ansa_non_zero$LON_start==3])
mean(EOT_ansa_non_zero$std_per_wk[EOT_ansa_non_zero$LON_start==4])
mean(EOT_ansa_non_zero$std_per_wk[EOT_ansa_non_zero$LON_start==5])

# phq9
phq9 <- getWHTable('phq9a_form', wh_connect)
phq9 <- prepPHQ9(phq9)
EOT_phq9 <- build_eot_phq9(phq9)
EOT_phq9 <- add_meds_eot_phq9(EOT_phq9)
EOT_vis <- prepEOTVisit(clientvisit)
EOT_phq9 <- add_services_eot(EOT_phq9)

EOT_phq9 <- add_lasts_eot_phq9(EOT_phq9)
EOT_phq9 <- add_firsts_eot_phq9(EOT_phq9)
EOT_phq9 <- add_serv_diff_eot(EOT_phq9)

EOT_phq9$improved[EOT_phq9$diff_total < 0] <- 1
EOT_phq9$improved[EOT_phq9$diff_total >= 0] <- 0

sum(EOT_phq9$diff_total <= -1)
sum(EOT_phq9$diff_total >= 1)


write.csv(EOT_phq9[complete.cases(EOT_phq9),], file = "G:/888/final_frames/EOT_phq9.csv")

# data exploration 
EOT_phq9 <- read.csv("G:/888/final_frames/EOT_phq9.csv")

head(EOT_phq9)

hist(EOT_phq9$tot_score_start,col='cyan', xlab="# points on 27 pt scale", 
     main='PHQ9 Depression Score')
summary(EOT_phq9$tot_score_start)

hist(EOT_phq9$diff_total,col='dark cyan', xlab="# points on 27 pt scale", 
     main='Change in PHQ9 Depression Score', breaks=16)
summary(EOT_phq9$diff_total)

took_p_e <- sum(EOT_phq9$took_proz)
took_w_e <- sum(EOT_phq9$took_well)
took_l_e <- sum(EOT_phq9$took_lexa)

first_p_e <- sum(EOT_phq9$first_proz)
first_w_e <- sum(EOT_phq9$first_well)
first_l_e <- sum(EOT_phq9$first_lexa)

proz_phq9_imp    <- improve_rate(EOT_phq9$first_proz, EOT_phq9$improved)
well_phq9_imp    <- improve_rate(EOT_phq9$first_well, EOT_phq9$improved)
lexa_phq9_imp    <- improve_rate(EOT_phq9$first_lexa, EOT_phq9$improved)

proz_row_p <- c(proz_phq9_imp)
well_row_p <- c(well_phq9_imp)
lexa_row_p <- c(lexa_phq9_imp)

med_improve_p <- matrix(c(proz_row_p, well_row_p, lexa_row_p), 
                      nrow = 3, ncol = 1, byrow = TRUE)

rownames(med_improve_p) <- c("Prozac", "Wellbutrin", "Lexapro")
colnames(med_improve_p) <- c("Medication")

barplot(med_improve_p,  
        col = brewer.pal(5, name = 'Dark2'),
        border="black", font.axis=2, beside=T, legend=rownames(med_improve_p), 
        args.legend=list(x=2, y=95),
        ylim=c(0,100),
        main="% of Time PHQ9 Improvement Co-occurs \nWhen Adding a Medication", font.lab=2)

any_first_phq9_imp  <- improve_rate(EOT_phq9$first_any_antidep, EOT_phq9$improved)
no_first_phq9_imp   <- improve_rate(!EOT_phq9$first_any_antidep, EOT_phq9$improved)

barplot(med_improve_p,  
        col = brewer.pal(5, name = 'Dark2'),
        border="black", font.axis=2, beside=T, legend=rownames(med_improve_p), 
        args.legend=list(x=2, y=95),
        ylim=c(0,100),
        main="% of Time PHQ9 Improvement Co-occurs \nWhen Adding a Medication", font.lab=2)



hist(EOT_phq9$std_per_wk)
hist(EOT_phq9$std_per_wk[EOT_phq9$std_per_wk < 10])
hist(EOT_phq9$std_per_wk[EOT_phq9$std_per_wk > 1 & EOT_phq9$std_per_wk < 25])
hist(EOT_phq9$therp_per_wk)
hist(EOT_phq9$therp_per_wk[EOT_phq9$therp_per_wk < 1.3])


# feature selection / Boruta

# feature selection - ansa - without start scores
EOT_ansa <- read.csv("G:/888/final_frames/EOT_ansa.csv")
head(EOT_ansa)

set.seed(42)
ansa_predictors <- c(9:18, 36:38, 52:59)
ansa_resp_lon   <- c(60)
ansa_resp_psych   <- c(61)
ansa_resp_any  <- c(62)

dim(EOT_ansa)

ansa_final_lon <- EOT_ansa[ , c(ansa_predictors, ansa_resp_lon)]
ansa_final_psych <- EOT_ansa[ , c(ansa_predictors, ansa_resp_psych)]
ansa_final_any <- EOT_ansa[ , c(ansa_predictors, ansa_resp_any)]

head(ansa_final_lon)
head(ansa_final_psych)
head(ansa_final_any)

bor_ansa_lon   <- Boruta(improved~., data=ansa_final_lon, doTrace = 2)
bor_ansa_psych <- Boruta(imp_psych~., data=ansa_final_psych, doTrace = 2)
bor_ansa_any   <- Boruta(any_imps~., data=ansa_final_any, doTrace = 2)

print(bor_ansa_lon)
print(bor_ansa_psych)
print(bor_ansa_any)

par(mar = c(7, 4, 4, 2) + 0.1)
plot(bor_ansa_lon, xlab = "", las = 2, main="Important Features - EOT_ansa - LON")
plot(bor_ansa_psych, xlab = "", las = 2, main="Important Features - EOT_ansa - Psych")
plot(bor_ansa_any, xlab = "", las = 2, main="Important Features - EOT_ansa - Any")


bor_ansa_lon_tfix   <- TentativeRoughFix(bor_ansa_lon)
bor_ansa_psych_tfix <- TentativeRoughFix(bor_ansa_psych)
bor_ansa_any_tfix   <- TentativeRoughFix(bor_ansa_any)

print(bor_ansa_lon_tfix)
print(bor_ansa_psych_tfix)
print(bor_ansa_any_tfix)

attStats(bor_ansa_lon_tfix)
attStats(bor_ansa_psych)
attStats(bor_ansa_any)



getNonRejectedFormula(bor_ansa_lon)
getNonRejectedFormula(bor_ansa_psych)
getNonRejectedFormula(bor_ansa_any)


# feature selection - ansa - WITH start scores

EOT_ansa <- read.csv("G:/888/final_frames/EOT_ansa.csv")
head(EOT_ansa)

set.seed(42)
ansa_predictors_st <- c(9:18, 19, 21, 23, 25, 27, 36:38, 52:59)
ansa_resp_lon_st   <- c(60)
ansa_resp_psych_st   <- c(61)
ansa_resp_any_st  <- c(62)

ansa_final_lon_st <- EOT_ansa[ , c(ansa_predictors_st, ansa_resp_lon_st)]
ansa_final_psych_st <- EOT_ansa[ , c(ansa_predictors_st, ansa_resp_psych_st)]
ansa_final_any_st <- EOT_ansa[ , c(ansa_predictors_st, ansa_resp_any_st)]

head(ansa_final_lon_st)
head(ansa_final_psych_st)
head(ansa_final_any_st)

bor_ansa_lon_st   <- Boruta(improved~., data=ansa_final_lon_st, doTrace = 2)
bor_ansa_psych_st <- Boruta(imp_psych~., data=ansa_final_psych_st, doTrace = 2)
bor_ansa_any_st   <- Boruta(any_imps~., data=ansa_final_any_st, doTrace = 2)

print(bor_ansa_lon_st)
print(bor_ansa_psych_st)
print(bor_ansa_any_st)

par(mar = c(7, 4, 4, 2) + 0.1)
plot(bor_ansa_lon_st, xlab = "", las = 2, main="Important Features - EOT_ansa - LON+start")
plot(bor_ansa_psych_st, xlab = "", las = 2, main="Important Features - EOT_ansa - Psych+start")
plot(bor_ansa_any_st, xlab = "", las = 2, main="Important Features - EOT_ansa - Any+start")

bor_ansa_lon_tfix_st   <- TentativeRoughFix(bor_ansa_lon_st)
bor_ansa_psych_tfix_st <- TentativeRoughFix(bor_ansa_psych_st)
bor_ansa_any_tfix_st   <- TentativeRoughFix(bor_ansa_any_st)

print(bor_ansa_lon_tfix_st)
print(bor_ansa_psych_tfix_st)
print(bor_ansa_any_tfix_st)

attStats(bor_ansa_lon_tfix_st)
attStats(bor_ansa_psych_st)
attStats(bor_ansa_any_st)

getNonRejectedFormula(bor_ansa_lon_st)
getNonRejectedFormula(bor_ansa_psych_st)
getNonRejectedFormula(bor_ansa_any_st)

getConfirmedFormula(bor_ansa_lon_st)
getConfirmedFormula(bor_ansa_psych_st)
getConfirmedFormula(bor_ansa_any_st)


# feature selection - phq9
EOT_phq9 <- read.csv("G:/888/final_frames/EOT_phq9.csv")
head(EOT_phq9)

set.seed(42)
EOT_phq9_predictors <- c(6, 9:27)
EOT_phq9_resp   <- c(28)

EOT_phq9_final <- EOT_phq9[ , c(EOT_phq9_predictors, EOT_phq9_resp)]
head(EOT_phq9_final)

bor_phq9   <- Boruta(improved~., data=EOT_phq9_final, doTrace = 2) #, maxRun = 1000)
print(bor_phq9)

par(mar = c(7, 4, 4, 2) + 0.1)
plot(bor_phq9, xlab = "", las = 2, main="Important Features - PHQ9")

bor_phq9_tfix   <- TentativeRoughFix(bor_phq9)
print(bor_phq9_tfix)
attStats(bor_phq9_tfix)
phq_nr_form <- getNonRejectedFormula(bor_phq9)
phq_cf_form <- getConfirmedFormula(bor_phq9)

dbDisconnectAll()

##########################################
#
# 04_crisis_int.R
#
# Dan Diamond
# 9/25/18
#
# Perform analysis on crisis intervention.  
#
##########################################

# rm(list=ls())

source("00_load.R")
source("00_clean_funcs.R")
source("00_eot_build.R")
# install.packages('sqldf')
require(sqldf)


##########################################
# get, clean, and subset tables
##########################################
wh_connect <- connect_to_wh()

# client
client              <- getWHTable('clients', wh_connect)
client_lookup       <- c_lookup_df(client)
client$research_num <- client_lookup$research_num
client              <- prepClient(client)

# clientvisit
clientvisit <- getWHTable('clientvisit', wh_connect)
clientvisit <- prepClientvisit(clientvisit)

# no show
no_show <- getWHTable('no_show_form', wh_connect)
no_show <- prepNoShow(no_show)

# diagnosis
diagnosis <- getWHTable('diagnosis_form', wh_connect)
diagnosis <- prepDiagnosis(diagnosis)
ebs_emp  <- read.csv("G:/888/data/hEmployee.csv")
rec_diag <- sqldf("select research_num, max(effdate), Prob1, gaf 
                  from diagnosis group by research_num", drv = 'SQLite')
rec_diag$`max(effdate)` <- as.Date(rec_diag$`max(effdate)`, origin='1970-01-01')
diag_lookup  <- read.csv("G:/888/data/diag_lookup.csv")

cli_rec_diag_lookup <- merge(rec_diag, diag_lookup, by.x = 'Prob1', 
                             by.y = 'diag_code', all.x = TRUE)
levels(cli_rec_diag_lookup$diag_cat) <- c(levels(cli_rec_diag_lookup$diag_cat), 'Unknown')
cli_rec_diag_lookup$diag_cat[is.na(cli_rec_diag_lookup$diag_cat)] <- 'Unknown'

# crisis 
crisisvisit <- prepCrisisVisit(clientvisit)
client_list <- unique(crisisvisit$research_num)

# break into chunks
#client_list_01 <- client_list[1:3000]
#client_list_02 <- client_list[3001:9000]
#client_list_03 <- client_list[9001:15000]
#client_list_04 <- client_list[15001:21000]
#client_list_05 <- client_list[21001:26761]
#timer_begin <- Sys.time()
#EOT_crisis <- build_eot_crisis(c_list = client_list_01, count = 1)
#timer_end <- Sys.time()
#time_took <- timer_end - timer_begin
#write.csv(EOT_crisis, file = "G:/888/final_frames/EOT_crisis_01.csv")


#EOT_crisis[EOT_crisis$num_services == 0,]
summary(as.factor(EOT_crisis$diag_cat))

head(EOT_crisis)

eot_crisis_temp_01  <- read.csv("G:/888/final_frames/EOT_crisis_01.csv")
eot_crisis_temp_02  <- read.csv("G:/888/final_frames/EOT_crisis_02.csv")
eot_crisis_temp_03  <- read.csv("G:/888/final_frames/EOT_crisis_03.csv")
eot_crisis_temp_04  <- read.csv("G:/888/final_frames/EOT_crisis_04.csv")
eot_crisis_temp_05  <- read.csv("G:/888/final_frames/EOT_crisis_05.csv")

eot_crisis_full <- rbind(eot_crisis_temp_01, eot_crisis_temp_02, eot_crisis_temp_03,
                         eot_crisis_temp_04, eot_crisis_temp_05)

eot_crisis_full <- eot_crisis_full[!is.na(eot_crisis_full$BirthGender),]
eot_crisis_full$diag_cat[is.na(eot_crisis_full$diag_cat)] <- 'Unknown'
eot_crisis_full <- eot_crisis_full[eot_crisis_full$num_services != 0, ]

write.csv(eot_crisis_full, file = "G:/888/final_frames/EOT_crisis_full.csv")


EOT_crisis <- nrow(EOT_crisis[EOT_crisis$num_services != 0, ])

EOT_crisis <- read.csv(file = "G:/888/final_frames/EOT_crisis_full.csv")
summary(EOT_crisis)
summary(as.factor(EOT_crisis$diag_cat))
head(EOT_crisis, 20)

EOT_crisis_only <- EOT_crisis[EOT_crisis$any_crisis > 0, ]
summary(as.factor(EOT_crisis_only$diag_cat))
str(EOT_crisis_only)
tot_cri_eps <- 304506
tot_cri_only <- 5146
tot_cri_not <- tot_cri_eps - tot_cri_only
percent_non_crisis <- tot_cri_not/tot_cri_eps


# feature selection - crisis
head(EOT_crisis)

set.seed(42)
EOT_crisis_predictors <- c(6:8, 10:13)
EOT_crisis_resp   <- c(9)

EOT_crisis_final <- EOT_crisis[ , c(EOT_crisis_predictors, EOT_crisis_resp)]
head(EOT_crisis_final)

bor_crisis   <- Boruta(any_crisis~., data=EOT_crisis_final, doTrace = 2) #, maxRun = 1000)
print(bor_crisis)

par(mar = c(7, 4, 4, 2) + 0.1)
plot(bor_crisis, xlab = "", las = 2, main="Important Features - Crisis")

bor_crisis_tfix   <- TentativeRoughFix(bor_crisis)
print(bor_crisis_tfix)
attStats(bor_crisis_tfix)
getNonRejectedFormula(bor_crisis)
getConfirmedFormula(bor_crisis)

plotZHistory(BOruta.bor_crisis_tfix)

#summary(as.factor(EOT_crisis$num_services))

# ansa
ansa <- getWHTable('ansa_form', wh_connect)
ansa <- prepAnsa(ansa)

# progress note
therp_note <- getWHTable('therapy_note_form', wh_connect)
therp_note <- prepTherpNote(therp_note)
prog_note  <- getWHTable('progress_note_form', wh_connect)
prog_note  <- prepProgNote(prog_note)


dbDisconnectAll()

##########################################
#
# 07_partitions.R
#
# Dan Diamond
# 10/31/18
#
# Create and validate models: EOT_ansa, 
#     EOT_phq9, EOT_crisis, emp_exit  
#
##########################################

library('caret')

##########################################
# create train/test partitions
##########################################

# ansa
an <- read.csv("G:/888/final_frames/EOT_ansa.csv")
set.seed(42)
part <- createDataPartition(
  y = an$improved, 
  p = 0.7, 
  list = FALSE
)
an_train <- an[part, ]
an_test  <- an[-part, ]



# phq9
p9 <- read.csv("G:/888/final_frames/EOT_phq9.csv")
set.seed(42)
p9$improved <- as.factor(p9$improved)
#str(p9)
part <- createDataPartition(
  y = p9$improved, 
  p = 0.7, 
  list = FALSE
  )
p9_train <- p9[part, ]
p9_test  <- p9[-part, ]



# crisis
crisis <- read.csv(file = "G:/888/final_frames/EOT_crisis_full.csv")
set.seed(42)
part <- createDataPartition(
  y = crisis$any_crisis, 
  p = 0.7, 
  list = FALSE
)
crisis_train <- crisis[part, ]
crisis_test  <- crisis[-part, ]



# emp_exit
emp_exit <- read.csv("G:/888/final_frames/emp_exit.csv")
set.seed(42)
part <- createDataPartition(
  y = emp_exit$Recommend, 
  p = 0.8, 
  list = FALSE
)
emp_exit_train <- emp_exit[part, ]
emp_exit_test  <- emp_exit[-part, ]

part <- createDataPartition(
  y = emp_exit_train$Recommend, 
  p = 0.75, 
  list = FALSE
)
emp_exit_boost <- emp_exit_train[-part, ]
emp_exit_train <- rbind(emp_exit_train, emp_exit_boost)

# cross validate with k=10, 1 for validation.
##########################################
#
# 08_model_phq9.R
#
# Dan Diamond
# 10/31/18
#
# Predictive modeling for improvement in 
#   the PHQ9 depression score.  
#
##########################################

library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(ggplot2)
#install.packages('DMwR')
library(DMwR)
library(doParallel)

cls = makeCluster(20)
registerDoParallel(cls)
closeAllConnections()

# extraction, preparation, and partitioning
p9 <- read.csv("G:/888/final_frames/EOT_phq9.csv")
set.seed(42)
p9$improved <- as.factor(p9$improved)
#str(p9)
part <- createDataPartition(
  y = p9$improved, 
  p = 0.7, 
  list = FALSE
)
p9_train <- p9[part, ]
p9_test  <- p9[-part, ]

nrow(p9_train)
nrow(p9_test)

summary(p9_train$improved)
summary(p9_test$improved)

##########################################
# Logistic Regression
##########################################
# lr model creation/tuning
phq9_lr <- train(improved ~ tot_score_start + std_per_wk + therp_per_wk + last_std_wk + 
                   last_therp_wk + diff_std + diff_therp, p9_train, method = "multinom", 
                 trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))

# lr model summary
summary(phq9_lr)

# lr model evaluation
plot(phq9_lr)

y_pred_train = predict(phq9_lr, newdata=p9_train, type = "raw")
confusionMatrix(y_pred_train, p9_train$improved)

y_pred_test = predict(phq9_lr, newdata=p9_test, type = "raw")
confusionMatrix(y_pred_test, p9_test$improved)

# AUC ROC
prob <- predict(phq9_lr, newdata=p9_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, p9_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "PHQ9 - Logistic Regression - ROC")
performance(pred, "auc")


##########################################
# Random Forest
##########################################
# parameters to tune: 
#     ntree       number of trees
#     mtry        # variables sampled

num_trees <- c(50, 60, 80, 100, 130, 160, 190, 220, 250, 300)
grid <- expand.grid(mtry = c(2:7))
num_trees <- c(50)
grid <- expand.grid(mtry = c(2))


train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, 
                              allowParallel = TRUE, savePredictions = TRUE)

timer_begin <- Sys.time()
sink("G:/888/model results/p9_rf_test.csv")
cat(paste('num_trees, mtry, train_acc, test_acc, test_AUC\n'))
for (tr in num_trees){
  p9_rf <- train(improved ~ tot_score_start + std_per_wk + therp_per_wk + last_std_wk + 
                 last_therp_wk + diff_std + diff_therp, p9_train, method = 'rf', 
                 ntree = tr, tuneGrid = grid, 
                 trControl = train_control)
  assign(paste("p9_rf_", tr, "_trees_", sep=""), 
         p9_rf)
  y_pred_train = predict(p9_rf, data=p9_train)
  train_conf_mat = table(p9_train$improved, y_pred_train)
  train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)
  y_pred_test = predict(p9_rf, newdata=p9_test)
  test_conf_mat = table(p9_test$improved, y_pred_test)
  test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)
  
  # AUC ROC
  prob <- predict(p9_rf, newdata=p9_test, type="raw")
  prob = as.vector(as.numeric(prob))
  pred <- prediction(prob, p9_test$improved)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, "auc")
  auc@y.values
  cat(paste(tr, p9_rf$finalModel$mtry, train_acc, test_acc, auc@y.values, sep=', '))
  cat('\n')
}
timer_end <- Sys.time()
time_took <- timer_end - timer_begin
cat(paste("Time: ", as.character(time_took), " minutes.", sep=" "))
cat('\n')
sink()


m <- c(2, 3, 4, 5, 6, 7)
p9_kaps <- cbind(m, p9_rf_50_trees_$results$Kappa, p9_rf_60_trees_$results$Kappa,
                  p9_rf_80_trees_$results$Kappa, p9_rf_100_trees_$results$Kappa,
                  p9_rf_130_trees_$results$Kappa, p9_rf_160_trees_$results$Kappa,
                  p9_rf_190_trees_$results$Kappa, p9_rf_220_trees_$results$Kappa,
                  p9_rf_250_trees_$results$Kappa, p9_rf_300_trees_$results$Kappa)
colnames(p9_kaps) <- c('m', '50', '60', '80', '100', '130', '160', '190', '220',
                        '250', '300')
write.csv(p9_kaps, file = "G:/888/model results/p9_rf_cv_kaps.csv")


m <- c(2, 3, 4, 5, 6, 7)
p9_accs <- cbind(m, p9_rf_50_trees_$results$Accuracy, p9_rf_60_trees_$results$Accuracy,
                 p9_rf_80_trees_$results$Accuracy, p9_rf_100_trees_$results$Accuracy,
                 p9_rf_130_trees_$results$Accuracy, p9_rf_160_trees_$results$Accuracy,
                 p9_rf_190_trees_$results$Accuracy, p9_rf_220_trees_$results$Accuracy,
                 p9_rf_250_trees_$results$Accuracy, p9_rf_300_trees_$results$Accuracy)
colnames(p9_accs) <- c('m', '50', '60', '80', '100', '130', '160', '190', '220',
                       '250', '300')
write.csv(p9_accs, file = "G:/888/model results/p9_rf_cv_accs.csv")

##########################################
# Adaptive Boost
##########################################
# parameters to tune: 
#   nIter
#   mtry
#   Data Driven Decision Making - trees = nIter, max depth = 30, complexity

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE, 
                              allowParallel = TRUE)
#grid <- expand.grid(mtry = c(2:3), nIter = c(10, 30), method = 'adaboost')#
grid <- expand.grid(nIter = c(10, 30, 40, 50, 70, 100, 120, 200), method = 'adaboost')
grid <- expand.grid(nIter = c(100), method = 'adaboost')

timer_begin <- Sys.time()
p9_ada <- train(improved ~ tot_score_start + std_per_wk + therp_per_wk + last_std_wk + 
                  last_therp_wk + diff_std + diff_therp, data = p9_train, 'adaboost', mtry = 2,  
                trControl = train_control, tuneGrid = grid, verbose=TRUE, metric = "Kappa")
timer_end <- Sys.time()
(time_took <- timer_end - timer_begin)

y_pred_train = predict(p9_ada, data=p9_train)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(p9_train$improved, y_pred_train)
(train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(p9_ada, newdata=p9_test)
test_conf_mat = table(p9_test$improved, y_pred_test)
(test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(an_ada, newdata=p9_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, p9_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Adaptive Boost - ROC")
performance(pred, "auc")

write_csv(an_ada$results, path = "G:/888/model results/p9_ada_results.csv", 
          append = TRUE, col_names = TRUE)


##########################################
# Support Vector Machine
##########################################
# parameters to tune: 
#   Cost C : 0.05, 0.1, 0.2, ... 0.9, 1.0, 2.0, 3.0

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE,
                              allowParallel = TRUE)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.5, 5, 7.5, 10))
grid <- expand.grid(C = c(0.5))

timer_begin <- Sys.time()
p9_svm <- train(improved ~ tot_score_start + std_per_wk + therp_per_wk + last_std_wk + 
                last_therp_wk + diff_std + diff_therp, 
                data = p9_train, 'svmLinear', tuneLength = 10, metric = "Kappa",
                trControl = train_control, tuneGrid = grid, verbose = TRUE)
timer_end <- Sys.time()
time_took <- timer_end - timer_begin

print(p9_svm)
p9_svm$results$Kappa

#### svm model evaluation
y_pred_train = predict(p9_svm, data=p9_train)
train_conf_mat = table(p9_train$improved, y_pred_train)
(train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(p9_svm, newdata=p9_test)
test_conf_mat = table(p9_test$improved, y_pred_test)
(test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(p9_svm, newdata=p9_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, p9_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Support Vector Machine - ROC")
performance(pred, "auc")

write_csv(p9_svm$results, path = "G:/888/model results/p9_svm_results.csv", 
          append = TRUE, col_names = TRUE)


##########################################
# Artificial Neural Network
##########################################
# nnet parameters to tune:
#	size = # of hidden layers, decay = 'weight decay'

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE, 
                              allowParallel = TRUE)

grid <- expand.grid(decay = c(0.1, 0.5), size = c(2, 3))
grid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), size = c(3, 5, 10, 15, 20, 25))
grid <- expand.grid(decay = c(1.5, 1.4, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6), size = c(15, 20, 25, 30, 35))

grid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), size = c(35))
grid <- expand.grid(decay = 0.5, size = 15)

warnings()
#grid <- expand.grid(method = 'neuralNet', layer1 = c(10, 14, 18, 22), layer2 = c(10, 14, 18, 22), layer3 = c(10, 14))

summary(p9_train$improved)
str(p9_train$improved)
p9_train_nn <- p9_train
levels(p9_train_nn$improved) <- c("zero", "one")
p9_test_nn <- p9_test
levels(p9_test_nn$improved) <- c("zero", "one")

timer_begin <- Sys.time()
p9_nn <- train(improved ~ tot_score_start + std_per_wk + therp_per_wk + last_std_wk + 
               last_therp_wk + diff_std + diff_therp, 
               data = p9_train, 'nnet', tuneLength = 10, metric = "Kappa", 
               trControl = train_control, tuneGrid = grid, verbose=TRUE)
timer_end <- Sys.time()
(time_took <- timer_end - timer_begin)

p9_nn$results
str(p9_nn$results)
write.csv(p9_nn$results, file = "G:/888/model results/p9_nn_02.csv")

p9_nn$bestTune
p9_nn$results$Kappa
y_pred_train = predict(p9_nn, data=p9_train)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(p9_train$improved, y_pred_train)
(train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(p9_nn, newdata=p9_test)
test_conf_mat = table(p9_test$improved, y_pred_test)
(test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(p9_nn, newdata=p9_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, p9_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "PHQ9 - Adaptive Boost - ROC")
performance(pred, "auc")


# visualize model comparison
y_pred_test = predict(phq9_lr, newdata=p9_test, type = "raw")
confusionMatrix(y_pred_test, p9_test$improved)

y_pred_test = predict(p9_rf, newdata=p9_test, type = "raw")
confusionMatrix(y_pred_test, p9_test$improved)

y_pred_test = predict(p9_ada, newdata=p9_test, type = "raw")
confusionMatrix(y_pred_test, p9_test$improved)

y_pred_test = predict(p9_svm, newdata=p9_test, type = "raw")
confusionMatrix(y_pred_test, p9_test$improved)

y_pred_test = predict(p9_nn, newdata=p9_test, type = "raw")
confusionMatrix(y_pred_test, p9_test$improved)


p9_compare <- data.frame(model=character(),
                         accuracy=numeric(), 
                         precision=numeric(), 
                         recall=numeric(),
                         AUC=numeric()
)

cols <- c('model', 'accuracy', 'precision', 'recall', 'AUC')

p9_lr_compare <- as.data.frame(list('LogRegression', 0.6470, 0.6578, 0.6224, 0.6578))
p9_rf_compare <- as.data.frame(list('RandForest', 0.6216, 0.6204, 0.6351, 0.6204))
p9_ada_compare <- as.data.frame(list('AdaBoost', 0.6053, 0.5746, 0.5656, 0.5746))
p9_svm_compare <- as.data.frame(list('SVM', 0.6470, 0.6593, 0.6145, 0.6593))
p9_nn_compare <- as.data.frame(list('ANN', 0.6348, 0.6359, 0.6319, 0.6359))

colnames(p9_lr_compare) <- cols
colnames(p9_rf_compare) <- cols
colnames(p9_ada_compare) <- cols
colnames(p9_svm_compare) <- cols
colnames(p9_nn_compare) <- cols

p9_compare <- rbind(p9_lr_compare, p9_rf_compare, p9_ada_compare, p9_svm_compare, p9_nn_compare)

# plot
col_acc     <- 'red'
col_prec    <- 'cyan'
col_recall  <- 'green'
col_auc     <- 'purple'

lty_acc     <- 1
lty_prec    <- 2
lty_recall  <- 3
lty_auc     <- 3

pch_acc     <- 15
pch_prec    <- 0
pch_recall  <- 17
pch_auc     <- 20

plot.default(p9_compare$model, p9_compare$accuracy, axes=FALSE, type="o", col=col_acc, lty=lty_acc, 
#             pch=pch_acc, xlab="model", ylab='value', ylim=c(0.5,0.7), main="PHQ9 Model Performance")
             pch=pch_acc, xlab="model", ylab='value', ylim=c(0,1), main="PHQ9 Model Performance (full scale)")

axis(side=1, at=p9_compare$model, labels=p9_compare$model)
axis(side=2)
lines(p9_compare$precision, type="o", col=col_prec, lty=lty_prec, pch=pch_prec, lwd=2)
lines(p9_compare$recall, type="o", col=col_recall, lty=lty_recall, pch=pch_recall)
lines(p9_compare$AUC, type="o", col=col_auc, lty=lty_auc, pch=pch_auc, lwd=2)
#legend(x=3.5, y=0.59, legend=c("Accuracy", "Precision", "Recall", "AUC"), box.lty=0,
legend(x=3.5, y=1.1, legend=c("Accuracy", "Precision", "Recall", "AUC"), box.lty=0,       
       lty=c(lty_acc, lty_prec, lty_recall, lty_auc), 
       pch=c(pch_acc, pch_prec, pch_recall, pch_auc), 
       col=c(col_acc, col_prec, col_recall, col_auc),
       lwd=c(1, 2, 1, 2))

# bar plot
p9_comp <- read.csv("G:/888/final_frames/metrics_phq9.csv")
p9_comp$model <- factor(p9_comp$model, levels = c("LogRegression", "RandForest", "AdaBoost", "SVM", "ANN"))
ggplot(data=p9_comp, aes(x=model, y=value, fill=metric)) +
  geom_bar(stat='identity', position=position_dodge(), color="black") +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("PHQ9 Model Peformance (Full Scale)") +  coord_cartesian(ylim=c(0, 1))

##########################################
#
# 09_model_ansa.R
#
# Dan Diamond
# 10/31/18
#
# Predictive modeling for improvement in 
#   the ansa score.  
#
##########################################
install.packages('caret')

library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(DMwR)
library(doParallel)

cls = makeCluster(20)
registerDoParallel(cls)


closeAllConnections()


# extraction, preparation, and partitioning
an <- read.csv("G:/888/final_frames/EOT_ansa.csv")
set.seed(42)
an$improved <- as.factor(an$improved)
#str(an)
part <- createDataPartition(
  y = an$improved, 
  p = 0.7, 
  list = FALSE
)

an_train <- an[part, ]
an_test  <- an[-part, ]

summary(an_train$improved)

# test SMOTE parameters
overs <- c(40, 60, 80, 100, 140, 180, 200, 250, 300)
unders <- c(20, 40, 60, 90, 100, 120, 150, 200, 225, 250)

timer_begin <- Sys.time()
sink("G:/888/SMOTE_test.txt")
for (i in overs){
  for (j in unders){ 
    an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = i, k = 5, perc.under = j)
    neg <- sum(an_train_bal$improved == 0)
    pos <- sum(an_train_bal$improved == 1)
    cat(paste('*****************************\n'))
    cat(paste('under\t', 'over\n'))
    cat(paste(j, '\t', i, '\n\n'))
    cat(paste('0\t', '1\n'))
    cat(paste(neg, '\t', pos, '\n'))
  }
}
timer_end <- Sys.time()
time_took <- timer_end - timer_begin
time_took
sink()

summary(time_took)
closeAllConnections()

an_train <- an[part, ]
summary(an_train$improved)
# 8K, 8K, 3
an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = 100, k = 3, perc.under = 200)
# 12K, 12K, 3
an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = 200, k = 3, perc.under = 150)
# 8K, 8K, 5
an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = 100, k = 5, perc.under = 200)
# 12K, 12K, 5
an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = 200, k = 5, perc.under = 150)
# 8K, 8K, 10
an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = 100, k = 10, perc.under = 200)
# 12K, 12K, 10
an_train_bal <- SMOTE(improved ~ ., an_train, perc.over = 200, k = 10, perc.under = 150)

summary(an_train_bal$improved)

an_train_bal$took_risp <- as.logical(an_train_bal$took_risp)
an_train_bal$took_cloz <- as.logical(an_train_bal$took_cloz)
an_train_bal$took_proz <- as.logical(an_train_bal$took_proz)
an_train_bal$took_well <- as.logical(an_train_bal$took_well)
an_train_bal$took_lexa <- as.logical(an_train_bal$took_lexa)
an_train_bal$first_risp <- as.logical(an_train_bal$first_risp)
an_train_bal$first_cloz <- as.logical(an_train_bal$first_cloz)
an_train_bal$first_proz <- as.logical(an_train_bal$first_proz)
an_train_bal$first_well <- as.logical(an_train_bal$first_well)
an_train_bal$first_lexa <- as.logical(an_train_bal$first_lexa)
an_train_bal$last_risp <- as.logical(an_train_bal$last_risp)
an_train_bal$last_cloz <- as.logical(an_train_bal$last_cloz)
an_train_bal$last_proz <- as.logical(an_train_bal$last_proz)
an_train_bal$last_well <- as.logical(an_train_bal$last_well)
an_train_bal$last_lexa <- as.logical(an_train_bal$last_lexa)
an_train <- an_train_bal

str(an_train)
#str(an_test)
#head(an_train)
#unique(an_train$diag_cat)

##########################################
# Logistic Regression
##########################################
# lr model creation/tuning
timer_begin <- Sys.time()
an_lr <- train(improved ~ Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
                 Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
                 start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
                 therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
                 diff_therp, an_train, method = "multinom", 
                 trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
timer_end <- Sys.time()
(time_took <- timer_end - timer_begin)


# lr model summary
#summary(an_lr)

# lr model evaluation
y_pred_train = predict(an_lr, newdata=an_train, type = "raw")
confusionMatrix(y_pred_train, an_train$improved)

y_pred_test = predict(an_lr, newdata=an_test, type = "raw")
confusionMatrix(y_pred_test, an_test$improved)

# AUC ROC
prob <- predict(an_lr, newdata=an_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, an_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Logistic Regression - ROC")
performance(pred, "auc")


##########################################
# Random Forest
##########################################
# parameters to tune: 
#     ntree       number of trees
#     mtry        # variables sampled


num_cols <- c('ep_length', 'LON_start', 'LON_end', 'Black', 'NativeAmerican', 
              'Asian', 'Caucasian', 'HawPacificIsle', 'OtherRace', 'Ethnicity', 
              'Religion', 'EduLevel', 'start_hous_stab', 'end_hous_stab', 
              'start_psych', 'end_psych', 'start_depr', 'end_depr', 
              'start_anx', 'end_anx', 'start_interpers', 'end_interpers', 'diff_hous_stab', 
              'diff_LON', 'diff_psych', 'diff_depr', 'diff_anx', 'diff_interpers', 'sum_diffs', 
              'imp_psych', 'any_imps')

sapply(an_test, class)
an_test[num_cols] <- sapply(an_test[num_cols], as.numeric)


num_trees <- c(80)
grid <- expand.grid(mtry = c(6))
#num_trees <- c(50, 60, 80, 100, 130, 160, 190, 220, 250, 300)
#grid <- expand.grid(mtry = c(2:7))

train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, 
                    savePredictions = TRUE, allowParallel = TRUE)

timer_begin <- Sys.time()
sink("G:/888/an_rf_test_ROC.csv")
cat(paste('num_trees, mtry, train_acc, test_acc, test_AUC\n'))
for (tr in num_trees){
  an_rf <- train(improved ~ Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
     Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
     start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
     therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
     diff_therp, data = an_train, method = 'rf', ntree = tr, tuneGrid = grid, 
     trControl = train_control)
  assign(paste("an_rf_", tr, "_trees_", sep=""), 
     an_rf)
  y_pred_train = predict(an_rf, data=an_train)
  train_conf_mat = table(an_train$improved, y_pred_train)
  train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)
  y_pred_test = predict(an_rf, newdata=an_test)
  test_conf_mat = table(an_test$improved, y_pred_test)
  test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

  # AUC ROC
  prob <- predict(an_rf, newdata=an_test, type="raw")
  prob = as.vector(as.numeric(prob))
  pred <- prediction(prob, an_test$improved)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, "auc")
  auc@y.values
  cat(paste(tr, an_rf$finalModel$mtry, train_acc, test_acc, auc@y.values, sep=', '))
  cat('\n')
}
timer_end <- Sys.time()
time_took <- timer_end - timer_begin
cat(paste("Time: ", as.character(time_took), " minutes.", sep=" "))
cat('\n')
sink()

plot(perf)
str(perf)
summary(perf)

str(an_test)
summary(an_test$improved)


# manually get kappa scores
an_rf_50_trees_$results$Kappa
an_rf_60_trees_$results$Kappa
an_rf_80_trees_$results$Kappa
an_rf_100_trees_$results$Kappa
an_rf_130_trees_$results$Kappa
an_rf_160_trees_$results$Kappa
an_rf_190_trees_$results$Kappa
an_rf_220_trees_$results$Kappa
an_rf_250_trees_$results$Kappa
an_rf_300_trees_$results$Kappa


##########################################
# Adaptive Boost
##########################################
# parameters to tune: 
#   nIter
#   mtry
#   Data Driven Decision Making - trees = nIter, max depth = 30, complexity

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE)
train_control <- trainControl(method="cv", number = 5, savePredictions = TRUE, verboseIter = TRUE, 
                              allowParallel = TRUE)

#grid <- expand.grid(mtry = c(2:3), nIter = c(10, 30), method = 'adaboost')#
grid <- expand.grid(nIter = c(10, 30, 40, 50, 70, 100, 120, 200), method = 'adaboost')#

grid <- expand.grid(nIter = c(100, 110, 120), method = 'adaboost')#

timer_begin <- Sys.time()
an_ada <- train(improved ~ Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
                  Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
                  start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
                  therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
                  diff_therp, data = an_train, 'adaboost', mtry = 5,  
                trControl = train_control, tuneGrid = grid, verbose=TRUE)
timer_end <- Sys.time()
(timer_end - timer_begin)


an_ada$bestTune
an_ada$results$Kappa

y_pred_train = predict(an_ada, data=an_train)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(an_train$improved, y_pred_train)
(train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(an_ada, newdata=an_test)
test_conf_mat = table(an_test$improved, y_pred_test)
(test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(an_ada, newdata=an_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, an_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Adaptive Boost - ROC")
performance(pred, "auc")

write_csv(an_ada$results, path = "G:/888/model results/an_ada_results.csv", 
          append = TRUE, col_names = TRUE)


##########################################
# Support Vector Machine             
##########################################
# parameters to tune: 
#   Cost C : 0.05, 0.1, 0.2, ... 0.9, 1.0, 2.0, 3.0

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE,
                              allowParallel = TRUE)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.5, 5, 7.5, 10))
grid <- expand.grid(C = c(1.25))


timer_begin <- Sys.time()
an_svm <- train(improved ~ Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
                  Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
                  start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
                  therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
                  diff_therp, data = an_train, 'svmLinear', tuneLength = 10, 
                  trControl = train_control, tuneGrid = grid, verbose = TRUE)
timer_end <- Sys.time()
time_took <- timer_end - timer_begin

print(an_svm)
an_svm$results$Kappa

#### svm model evaluation

y_pred_train = predict(an_svm, data=an_train)
train_conf_mat = table(an_train$improved, y_pred_train)
train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(an_svm, newdata=an_test)
test_conf_mat = table(an_test$improved, y_pred_test)
test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(an_svm, newdata=an_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, an_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Support Vector Machine - ROC")
performance(pred, "auc")

##########################################
# Artificial Neural Network
##########################################
# neuralNet parameters to tune: 
#	layer1, layer2, layer3

# nnet parameters to tune:
#	size = # of hidden layers, decay = 'weight decay'

train_control <- trainControl(method="cv", number = 5, savePredictions = TRUE, verboseIter = TRUE, 
                              allowParallel = TRUE)

grid <- expand.grid(decay = 0.1, size = 2)
grid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), size = c(3, 5, 10, 15, 20, 25))
grid <- expand.grid(decay = c(1.5, 1.4, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6), size = c(15, 20, 25, 30, 35))
grid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), size = c(35))
grid <- expand.grid(decay = 0.5, size = 25)

warnings()

#grid <- expand.grid(method = 'neuralNet', layer1 = c(10, 14, 18, 22), layer2 = c(10, 14, 18, 22), layer3 = c(10, 14))

summary(an_train$improved)
str(an_train$improved)
an_train_nn <- an_train
levels(an_train_nn$improved) <- c("zero", "one")
an_test_nn <- an_test
levels(an_test_nn$improved) <- c("zero", "one")

summary(an_train_nn$improved)

timer_begin <- Sys.time()
an_nn <- train(improved ~ Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
                 Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
                 start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
                 therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
                 diff_therp, data = an_train_nn, 'nnet',
               trControl = train_control, tuneGrid = grid, verbose=TRUE)
timer_end <- Sys.time()
time_took <- timer_end - timer_begin


cls = makeCluster(20)
registerDoParallel(cls)

an_nn$results
str(an_nn$results)
write.csv(an_nn$results, file = "G:/888/model results/an_nn_02.csv")

an_nn$bestTune
an_nn$results$Kappa
y_pred_train = predict(an_nn, data=an_train_nn)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(an_train_nn$improved, y_pred_train)
train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(an_nn, newdata=an_test_nn)
test_conf_mat = table(an_test_nn$improved, y_pred_test)
test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(an_nn, newdata=an_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, an_test$improved)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Adaptive Boost - ROC")
performance(pred, "auc")


# visualize model comparison
y_pred_test = predict(an_lr, newdata=an_test, type = "raw")
confusionMatrix(y_pred_test, an_test$improved)

y_pred_test = predict(an_rf, newdata=an_test, type = "raw")
confusionMatrix(y_pred_test, an_test$improved)

y_pred_test = predict(an_ada, newdata=an_test, type = "raw")
confusionMatrix(y_pred_test, an_test$improved)

y_pred_test = predict(an_svm, newdata=an_test, type = "raw")
confusionMatrix(y_pred_test, an_test$improved)

y_pred_test = predict(an_nn, newdata=an_test_nn, type = "raw")
confusionMatrix(y_pred_test, an_test_nn$improved)


an_compare <- data.frame(model=character(),
                         accuracy=numeric(), 
                         precision=numeric(), 
                         recall=numeric(),
                         AUC=numeric()
)

cols <- c('model', 'accuracy', 'precision', 'recall', 'AUC')

an_lr_compare <- as.data.frame(list('LogRegression', 0.5754, 0.2219, 0.4517, 0.5407))
an_rf_compare <- as.data.frame(list('RandForest', 0.7659, 0.2770, 0.2134, 0.5158))
an_ada_compare <- as.data.frame(list('AdaBoost', 0.6283, 0.2333, 0.3615, 0.5284))
an_svm_compare <- as.data.frame(list('SVM', 0.5478, 0.2298, 0.5205, 0.5238))
an_nn_compare <- as.data.frame(list('ANN', 0.6575, 0.2228, 0.2770, 0.5157))

colnames(an_lr_compare) <- cols
colnames(an_rf_compare) <- cols
colnames(an_ada_compare) <- cols
colnames(an_svm_compare) <- cols
colnames(an_nn_compare) <- cols

an_compare <- rbind(an_lr_compare, an_rf_compare, an_ada_compare, an_svm_compare, an_nn_compare)

# plot
col_acc     <- 'red'
col_prec    <- 'cyan'
col_recall  <- 'green'
col_auc     <- 'purple'

lty_acc     <- 1
lty_prec    <- 2
lty_recall  <- 3
lty_auc     <- 3

pch_acc     <- 15
pch_prec    <- 0
pch_recall  <- 17
pch_auc     <- 20

plot.default(an_compare$model, an_compare$accuracy, axes=FALSE, type="o", col=col_acc, lty=lty_acc, 
             pch=pch_acc, xlab="model", ylab='value', ylim=c(0,1), main="ANSA Model Performance")
axis(side=1, at=an_compare$model, labels=an_compare$model)
axis(side=2)
lines(an_compare$precision, type="o", col=col_prec, lty=lty_prec, pch=pch_prec, lwd=2)
lines(an_compare$recall, type="o", col=col_recall, lty=lty_recall, pch=pch_recall)
lines(an_compare$AUC, type="o", col=col_auc, lty=lty_auc, pch=pch_auc, lwd=2)
legend(x=3.5, y=1.05, legend=c("Accuracy", "Precision", "Recall", "AUC"), box.lty=0,
       lty=c(lty_acc, lty_prec, lty_recall, lty_auc), 
       pch=c(pch_acc, pch_prec, pch_recall, pch_auc), 
       col=c(col_acc, col_prec, col_recall, col_auc),
       lwd=c(1, 2, 1, 2))

str(an_compare)


auc <- function (t, f) {
  auc = t*f/2 + (1-t)*(1-f)/2 + t*(1-f)
  return(auc)
}


auc1 <- function (t, f) {
  auc1 = t*f/2
  return(auc1)
}

auc2 <- function (t, f) {
  auc2 = (1-t)*(1-f)/2
  return(auc2)
}

auc3 <- function (t, f) {
  auc3 = t*(1-f)
  return(auc3)
}

# bar plot
ansa_comp <- read.csv("G:/888/final_frames/metrics_ansa.csv")
ansa_comp$model <- factor(ansa_comp$model, levels = c("LogRegression", "RandForest", "AdaBoost", "SVM", "ANN"))
ggplot(data=ansa_comp, aes(x=model, y=value, fill=metric)) +
  geom_bar(stat='identity', position=position_dodge(), color="black") +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("ANSA Model Peformance")


# multilayer neural net?  
install.packages("neuralnet")
library(neuralnet)

an_m_nn <- read.csv("G:/888/final_frames/EOT_ansa.csv")
str(an_m_nn)
an_m_nn <- an_m_nn[,]


f <- as.formula(paste("improved ~", "Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
  Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
  start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
  therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
  diff_therp"))



an_train_nn_multi <- model.matrix(~ Black + NativeAmerican + Caucasian + OtherRace + Ethnicity + 
                                    Religion + EduLevel + diag_cat + start_hous_stab + start_psych + 
                                    start_depr + start_anx + start_interpers + std_per_wk + cm_per_wk + 
                                    therp_per_wk + first_cloz + first_lexa + diff_std + diff_cm + 
                                    diff_therp, data=an_train_nn)

an_nn_multi <- neuralnet(f, data=an_train_nn_multi, hidden=c(4,4), linear.output=FALSE)

str(an_train)
str(an_train_nn)
str(an_train_nn_multi)

##########################################
#
# 11_model_emp.R
#
# Dan Diamond
# 11/09/18
#
# Predictive modeling for improvement in 
#   the employee exit survey.  
#
##########################################

library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(DMwR)
install.packages('readr')
library(readr)

# extraction, preparation, and partitioning
emp_exit <- read.csv("G:/888/final_frames/emp_exit.csv")
set.seed(42)
part <- createDataPartition(
  y = emp_exit$Recommend, 
  p = 0.8, 
  list = FALSE
)
emp_train <- emp_exit[part, ]
emp_test  <- emp_exit[-part, ]

part <- createDataPartition(
  y = emp_train$Recommend, 
  p = 0.75, 
  list = FALSE
)
emp_boost <- emp_train[-part, ]
emp_train <- rbind(emp_train, emp_boost)

summary(emp_train)
str(emp_train)
dim(emp_train)
dim(emp_test)

emp_train$Recommend <- as.factor(emp_train$Recommend)
emp_test$Recommend <- as.factor(emp_test$Recommend)

summary(as.factor(emp_train$Recommend))

##########################################
# Logistic Regression
##########################################
# lr model creation/tuning
timer_begin <- Sys.time()
emp_lr <- train(Recommend ~ DeptCom + LikeJob + Expect + PosCat + DeptAdv + 
    AmtWork + SupRec + SupFair + AnnLeave + Stress, emp_train, 
    method = "multinom", 
    trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
timer_end <- Sys.time()
(time_took <- timer_end - timer_begin)

# lr model summary
summary(emp_lr)

# lr model evaluation
y_pred_train = predict(emp_lr, newdata=emp_train, type = "raw")
confusionMatrix(y_pred_train, emp_train$Recommend)

(train_conf_mat = table(emp_train$Recommend, y_pred_train))
(train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat))

y_pred_test = predict(emp_lr, newdata=emp_test, type = "raw")
confusionMatrix(y_pred_test, emp_test$Recommend)

# AUC ROC
prob <- predict(emp_lr, newdata=emp_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, emp_test$Recommend)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Logistic Regression - ROC")
performance(pred, "auc")


##########################################
# Random Forest
##########################################
# parameters to tune: 
#     ntree       number of trees
#     mtry        # variables sampled

num_trees <- c(60)
grid <- expand.grid(mtry = c(3))
#num_trees <- c(50, 60, 80, 100, 130, 160, 190, 220, 250, 300)
#grid <- expand.grid(mtry = c(2:7))

train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, 
                              savePredictions = TRUE, allowParallel = TRUE)

timer_begin <- Sys.time()
sink("G:/888/model results/emp_rf_test.csv")
cat(paste('num_trees, mtry, train_acc, test_acc, test_AUC\n'))
for (tr in num_trees){
  emp_rf <- train(Recommend ~ DeptCom + LikeJob + Expect + PosCat + DeptAdv + 
     AmtWork + SupRec + SupFair + AnnLeave + Stress, 
     data = emp_train, method = 'rf', ntree = tr, tuneGrid = grid, 
     trControl = train_control)
  assign(paste("emp_rf_", tr, "_trees_", sep=""), 
         emp_rf)
  y_pred_train = predict(emp_rf, data=emp_train)
  train_conf_mat = table(emp_train$Recommend, y_pred_train)
  train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)
  y_pred_test = predict(emp_rf, newdata=emp_test)
  test_conf_mat = table(emp_test$Recommend, y_pred_test)
  test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)
  
  # AUC ROC
  prob <- predict(emp_rf, newdata=emp_test, type="raw")
  prob = as.vector(as.numeric(prob))
  pred <- prediction(prob, emp_test$Recommend)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, "auc")
  auc@y.values
  cat(paste(tr, emp_rf$finalModel$mtry, train_acc, test_acc, auc@y.values, sep=', '))
  cat('\n')
}
timer_end <- Sys.time()
time_took <- timer_end - timer_begin
cat(paste("Time: ", as.character(time_took), " minutes.", sep=" "))
cat('\n')
sink()

summary(emp_rf_130_trees_$finalModel)
emp_rf_130_trees_$finalModel$importance


summary(emp_rf)
rattle()
# manually get kappa scores
#emp_rf_50_trees_$results$Kappa
#emp_rf_60_trees_$results$Kappa
#emp_rf_80_trees_$results$Kappa
#emp_rf_100_trees_$results$Kappa
#emp_rf_130_trees_$results$Kappa
#emp_rf_160_trees_$results$Kappa
#emp_rf_190_trees_$results$Kappa
#emp_rf_220_trees_$results$Kappa
#emp_rf_250_trees_$results$Kappa
#emp_rf_300_trees_$results$Accuracy

m <- c(2, 3, 4, 5, 6, 7)
emp_accs <- cbind(m, emp_rf_50_trees_$results$Accuracy, emp_rf_60_trees_$results$Accuracy,
                    emp_rf_80_trees_$results$Accuracy, emp_rf_100_trees_$results$Accuracy,
                    emp_rf_130_trees_$results$Accuracy, emp_rf_160_trees_$results$Accuracy,
                    emp_rf_190_trees_$results$Accuracy, emp_rf_220_trees_$results$Accuracy,
                    emp_rf_250_trees_$results$Accuracy, emp_rf_300_trees_$results$Accuracy)

colnames(emp_accs) <- c('m', '50', '60', '80', '100', '130', '160', '190', '220',
                       '250', '300')

write.csv(emp_accs, file = "G:/888/model results/emp_rf_cv_acc.csv")


##########################################
# Adaptive Boost
##########################################
# parameters to tune: 
#   nIter
#   mtry
#   Data Driven Decision Making - trees = nIter, max depth = 30, complexity

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE, 
                              allowParallel = TRUE)
grid <- expand.grid(nIter = c(100), method = 'adaboost')#
#grid <- expand.grid(nIter = c(10, 30, 40, 50, 70, 100, 120, 200), method = 'adaboost')#

# ADABOOST MODEL:  
timer_begin <- Sys.time()
emp_ada <- train(Recommend ~ DeptCom + LikeJob + Expect + PosCat + DeptAdv + 
                 AmtWork + SupRec + SupFair + AnnLeave + Stress, 
                 data = emp_train, 'adaboost', mtry = 2, 
                trControl = train_control, tuneGrid = grid, verbose=TRUE)
timer_end <- Sys.time()
time_took <- timer_end - timer_begin

#emp_ada$bestTune
#emp_ada$results$Kappa
y_pred_train = predict(emp_ada, data=emp_train)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(emp_train$Recommend, y_pred_train)
train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)

y_pred_test = predict(emp_ada, newdata=emp_test)
test_conf_mat = table(emp_test$Recommend, y_pred_test)
test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)

# AUC ROC
prob <- predict(emp_ada, newdata=emp_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, emp_test$Recommend)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Adaptive Boost - ROC")
performance(pred, "auc")

write_csv(emp_ada$results, path = "G:/888/model results/emp_ada_results.csv", 
          append = TRUE, col_names = TRUE)

##########################################
# Support Vector Machine  
##########################################
# parameters to tune: 
#   Cost C : 0.05, 0.1, 0.2, ... 0.9, 1.0, 2.0, 3.0

install.packages('doParallel')
library(doParallel)

cls = makeCluster(20)
registerDoParallel(cls)

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE,
                              allowParallel = TRUE)
#grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.5, 5, 7.5, 10))
grid <- expand.grid(C = c(0.1))

timer_begin <- Sys.time()
emp_svm <- train(Recommend ~ DeptCom + LikeJob + Expect + PosCat + DeptAdv + 
                AmtWork + SupRec + SupFair + AnnLeave + Stress, 
                data = emp_train, 'svmLinear', tuneLength = 10, 
                trControl = train_control, tuneGrid = grid, verbose = TRUE)
timer_end <- Sys.time()
time_took <- timer_end - timer_begin

write_csv(emp_svm$results, path = "G:/888/model results/emp_svm_results.csv", 
          append = TRUE, col_names = TRUE)

#### svm model evaluation
y_pred_train = predict(emp_svm, data=emp_train)
(train_conf_mat = table(emp_train$Recommend, y_pred_train))
(train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat))

y_pred_test = predict(emp_svm, newdata=emp_test)
(test_conf_mat = table(emp_test$Recommend, y_pred_test))
(test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat))

# AUC ROC
prob <- predict(emp_svm, newdata=emp_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, emp_test$Recommend)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Support Vector Machine - ROC")
performance(pred, "auc")

# 2 means the person would not recommend.  1 means they would.  
# visualize model comparison
y_pred_test = predict(emp_lr, newdata=emp_test, type = "raw")
confusionMatrix(y_pred_test, emp_test$Recommend)

y_pred_test = predict(emp_rf, newdata=emp_test, type = "raw")
confusionMatrix(y_pred_test, emp_test$Recommend)

y_pred_test = predict(emp_ada, newdata=emp_test, type = "raw")
confusionMatrix(y_pred_test, emp_test$Recommend)

y_pred_test = predict(emp_svm, newdata=emp_test, type = "raw")
confusionMatrix(y_pred_test, emp_test$Recommend)

emp_compare <- data.frame(model=character(),
                         accuracy=numeric(), 
                         precision=numeric(), 
                         recall=numeric(),
                         AUC=numeric()
)

cols <- c('model', 'accuracy', 'precision', 'recall', 'AUC')

emp_lr_compare <- as.data.frame(list('LogRegression', 0.7273, 0.7143, 0.8333, 0.7167))
emp_rf_compare <- as.data.frame(list('RandForest', 1.0, 1.0, 1.0, 1.0))
emp_ada_compare <- as.data.frame(list('AdaBoost', 0.9091, 0.8571, 1.0, 0.6))
emp_svm_compare <- as.data.frame(list('SVM', 0.6364, 0.6, 1.0, 0.6))

colnames(emp_lr_compare) <- cols
colnames(emp_rf_compare) <- cols
colnames(emp_ada_compare) <- cols
colnames(emp_svm_compare) <- cols

emp_compare <- rbind(emp_lr_compare, emp_rf_compare, emp_ada_compare, emp_svm_compare)


# plot
plot.default(emp_compare$model, emp_compare$accuracy, axes=FALSE, type="o", col=col_acc, lty=lty_acc, 
             pch=pch_acc, xlab="model", ylab='value', ylim=c(0,1), main="Employee Exit Model Performance")
axis(side=1, at=emp_compare$model, labels=emp_compare$model)
axis(side=2)
lines(emp_compare$precision, type="o", col=col_prec, lty=lty_prec, pch=pch_prec, lwd=2)
lines(emp_compare$recall, type="o", col=col_recall, lty=lty_recall, pch=pch_recall)
lines(emp_compare$AUC, type="o", col=col_auc, lty=lty_auc, pch=pch_auc, lwd=2)
legend(x=3.1, y=0.4, legend=c("Accuracy", "Precision", "Recall", "AUC"), box.lty=0,
       lty=c(lty_acc, lty_prec, lty_recall, lty_auc), 
       pch=c(pch_acc, pch_prec, pch_recall, pch_auc), 
       col=c(col_acc, col_prec, col_recall, col_auc),
       lwd=c(1, 2, 1, 2))

# bar plot
emp_comp <- read.csv("G:/888/final_frames/metrics_emp.csv")
emp_comp$model <- factor(emp_comp$model, levels = c("LogRegression", "RandForest", "AdaBoost", "SVM", "ANN"))
ggplot(data=emp_comp, aes(x=model, y=value, fill=metric)) +
  geom_bar(stat='identity', position=position_dodge(), color="black") +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Employee Exit Model Peformance")

##########################################
#
# 12_model_crisis.R
#
# Dan Diamond
# 10/31/18
#
# Predictive modeling for occurence of 
#   psychiatric crisis.  
#
##########################################

library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(DMwR)
#install.packages('doParallel')
library(doParallel)
library(Boruta)

cls = makeCluster(20)
registerDoParallel(cls)
closeAllConnections()

# extraction, preparation, and partitioning
crisis <- read.csv(file = "G:/888/final_frames/EOT_crisis_full.csv")

# correct dependency problem between cumulative crisis and current crisis
new_crisis <- crisis[0,]
cli_nums <- unique(crisis$research_num)
count = 1

timer_begin <- Sys.time()
for(cli_num in cli_nums) {
  temp <- crisis[crisis$research_num == cli_num, ]
  temp$cum_crisis[match(1, temp$any_crisis)] <- 0
  new_crisis <- rbind(new_crisis, temp)
  cat(sprintf("%10d\n", count))
  count = count + 1
}
timer_end <- Sys.time()
(timer_end - timer_begin)

summary(as.factor(crisis$any_crisis))
summary(as.factor(cr$any_crisis))
summary(as.factor(new_crisis$any_crisis))
cr <- new_crisis
cr$any_crisis <- as.factor(cr$any_crisis)
str(cr)
summary(cr$any_crisis)

# recheck important factors
dim(cr)
set.seed(42)
crisis_predictors2 <- c(6:8, 10:13)
crisis_resp2   <- c(9)

crisis_final2 <- cr[ , c(crisis_predictors2, crisis_resp2)]
head(crisis_final2, 20)

bor_crisis   <- Boruta(any_crisis~., data=crisis_final2, doTrace = 2) #, maxRun = 1000)
print(bor_crisis)

par(mar = c(7, 4, 4, 2) + 0.1)
plot(bor_crisis, xlab = "", las = 2, main="Important Features - Crisis")

bor_crisis_tfix   <- TentativeRoughFix(bor_crisis)
print(bor_crisis_tfix)
attStats(bor_crisis_tfix)
getNonRejectedFormula(bor_crisis)

set.seed(42)
part <- createDataPartition(
  y = cr$any_crisis, 
  p = 0.7, 
  list = FALSE
)
cr_train <- cr[part, ]
summary(cr_test$any_crisis)

cr_train <- cr[part, ]
summary(cr_train$any_crisis)
#cr_train_bal <- SMOTE(any_crisis ~ ., cr_train, perc.over = 100, k = 3, perc.under = 300)
#cr_train_bal <- SMOTE(any_crisis ~ ., cr_train, perc.over = 100, k = 3, perc.under = 200)
#cr_train_bal <- SMOTE(any_crisis ~ ., cr_train, perc.over = 100, k = 5, perc.under = 300)
cr_train_bal <- SMOTE(any_crisis ~ ., cr_train, perc.over = 100, k = 5, perc.under = 200)
#cr_train_bal <- SMOTE(any_crisis ~ ., cr_train, perc.over = 100, k = 10, perc.under = 300)
#cr_train_bal <- SMOTE(any_crisis ~ ., cr_train, perc.over = 100, k = 10, perc.under = 200)


##########################################
# Logistic Regression
##########################################

# lr model creation/tuning
summary(cr_train_bal$any_crisis)
cr_train <- cr_train_bal
summary(cr_train$any_crisis)
summary(cr_test$any_crisis)


timer_begin <- Sys.time()
cr_lr <- train(any_crisis ~ num_services + no_shows + no_shows_last + BirthGender +
                 Age + cum_crisis + diag_cat, cr_train, method = "multinom", 
               trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
timer_end <- Sys.time()
(time_took <- timer_end - timer_begin)


# lr model summary
summary(cr_lr)

# lr model evaluation
y_pred_train = predict(cr_lr, newdata=cr_train, type = "raw")
confusionMatrix(y_pred_train, cr_train$any_crisis)

y_pred_test = predict(cr_lr, newdata=cr_test, type = "raw")
confusionMatrix(y_pred_test, cr_test$any_crisis)

# AUC ROC
prob <- predict(cr_lr, newdata=cr_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, cr_test$any_crisis)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Logistic Regression - ROC")
performance(pred, "auc")


##########################################
# Random Forest
##########################################
# parameters to tune: 
#     ntree       number of trees
#     mtry        # variables sampled


num_trees <- c(100)
grid <- expand.grid(mtry = c(5))
num_trees <- c(50, 60, 80, 100, 130, 160, 190, 220, 250, 300)
grid <- expand.grid(mtry = c(2:7))

train_control <- trainControl(method = "cv", number = 10, verboseIter = FALSE, 
                              allowParallel = TRUE, savePredictions = TRUE)

timer_begin <- Sys.time()
sink("G:/888/model results/cr_rf_test.csv")
cat(paste('num_trees, mtry, train_acc, test_acc, test_sens, test_AUC\n'))
for (tr in num_trees){
  cr_rf <- train(any_crisis ~ num_services + no_shows + no_shows_last + BirthGender +
                 Age + cum_crisis + diag_cat,
                 data = cr_train, method = 'rf', ntree = tr, tuneGrid = grid, 
                 trControl = train_control)
  assign(paste("cr_rf_", tr, "_trees_", sep=""), 
         cr_rf)
  y_pred_train = predict(cr_rf, data=cr_train)
  train_conf_mat = table(cr_train$any_crisis, y_pred_train)
  train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat)
  y_pred_test = predict(cr_rf, newdata=cr_test)
  test_conf_mat = table(cr_test$any_crisis, y_pred_test)
  test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat)
  #summary(cr_test$any_crisis)
  # sensitivity of occurence is specificity of it not occurring.  
  test_sens = test_conf_mat[2,2] / (test_conf_mat[2,2] + test_conf_mat[2,1])
    
  # AUC ROC
  prob <- predict(cr_rf, newdata=cr_test, type="raw")
  prob = as.vector(as.numeric(prob))
  pred <- prediction(prob, cr_test$any_crisis)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, "auc")
  auc@y.values
  cat(paste(tr, cr_rf$finalModel$mtry, train_acc, test_acc, test_sens, auc@y.values, sep=', '))
  cat('\n')
}
timer_end <- Sys.time()
time_took <- timer_end - timer_begin
cat(paste("Time: ", as.character(time_took), " minutes.", sep=" "))
cat('\n')
sink()

m <- c(2, 3, 4, 5, 6, 7)
cr_kaps <- cbind(m, cr_rf_50_trees_$results$Kappa, cr_rf_60_trees_$results$Kappa,
                 cr_rf_80_trees_$results$Kappa, cr_rf_100_trees_$results$Kappa,
                 cr_rf_130_trees_$results$Kappa, cr_rf_160_trees_$results$Kappa,
                 cr_rf_190_trees_$results$Kappa, cr_rf_220_trees_$results$Kappa,
                 cr_rf_250_trees_$results$Kappa, cr_rf_300_trees_$results$Kappa)
colnames(cr_kaps) <- c('m', '50', '60', '80', '100', '130', '160', '190', '220',
                       '250', '300')
write.csv(cr_kaps, file = "G:/888/model results/cr_rf_cv_kaps.csv")


##########################################
# Adaptive Boost
##########################################
# parameters to tune: 
#   nIter
#   mtry
#   Data Driven Decision Making - trees = nIter, max depth = 30, complexity

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE,
                    allowParallel = TRUE)
grid <- expand.grid(nIter = c(40), method = 'adaboost')
#grid <- expand.grid(nIter = c(10, 30, 40, 50, 70, 100, 120, 200), method = 'adaboost')#

timer_begin <- Sys.time()
cr_ada <- train(any_crisis ~ num_services + no_shows + no_shows_last + BirthGender +
                Age + cum_crisis + diag_cat, metric = "Kappa",
                data = cr_train, 'adaboost', mtry = 4, 
                trControl = train_control, tuneGrid = grid, verbose=TRUE)
timer_end <- Sys.time()
(timer_end - timer_begin)

cr_ada$bestTune
cr_ada$results$Kappa

# adaboost model evaluation
y_pred_train = predict(cr_ada, data=cr_train)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(cr_train$any_crisis, y_pred_train)
(train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat))

y_pred_test = predict(cr_ada, newdata=cr_test)
test_conf_mat = table(cr_test$any_crisis, y_pred_test)
(test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat))
(test_sens = test_conf_mat[2,2] / (test_conf_mat[2,2] + test_conf_mat[2,1]))

# AUC ROC
prob <- predict(cr_ada, newdata=cr_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, cr_test$any_crisis)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "Crisis - Adaptive Boost - ROC")
performance(pred, "auc")

write_csv(cr_ada$results, path = "G:/888/model results/cr_ada_results.csv", 
          append = TRUE, col_names = TRUE)

##########################################
# Support Vector Machine             
##########################################
# parameters to tune: 
#   Cost C : 0.05, 0.1, 0.2, ... 0.9, 1.0, 2.0, 3.0

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE,
                              allowParallel = TRUE)
#grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.5, 5, 7.5, 10))
grid <- expand.grid(C = c(0.5))

timer_begin <- Sys.time()
cr_svm <- train(any_crisis ~ num_services + no_shows + no_shows_last + BirthGender +
                  Age + cum_crisis + diag_cat, 
                data = cr_train, 'svmLinear', tuneLength = 10, metric = "Kappa",
                trControl = train_control, tuneGrid = grid, verbose = TRUE)
timer_end <- Sys.time()
(timer_end - timer_begin)

print(cr_svm)
cr_svm$results$Kappa

# svm model evaluation
y_pred_train = predict(cr_svm, data=cr_train)
train_conf_mat = table(cr_train$any_crisis, y_pred_train)
(train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat))

y_pred_test = predict(cr_svm, newdata=cr_test)
test_conf_mat = table(cr_test$any_crisis, y_pred_test)
(test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat))
(test_sens = test_conf_mat[2,2] / (test_conf_mat[2,2] + test_conf_mat[2,1]))

# AUC ROC
prob <- predict(cr_svm, newdata=cr_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, cr_test$any_crisis)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf, main = "ANSA - Support Vector Machine - ROC")
performance(pred, "auc")

write_csv(cr_svm$results, path = "G:/888/model results/cr_svm_results.csv", 
          append = TRUE, col_names = TRUE)

##########################################
# Artificial Neural Network
##########################################
# neuralNet parameters to tune: 
#	layer1, layer2, layer3

# nnet parameters to tune:
#	size = # of hidden layers, decay = 'weight decay'

train_control <- trainControl(method="cv", number = 10, savePredictions = TRUE, verboseIter = TRUE, 
                              allowParallel = TRUE)

grid <- expand.grid(decay = 0.000001, size = 25)
#grid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), size = c(3, 5, 10, 15, 20, 25))
#grid <- expand.grid(decay = c(1.5, 1.4, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6), size = c(15, 20, 25, 30, 35))
#grid <- expand.grid(decay = 0.5, size = 25)

summary(cr_train$any_crisis)

timer_begin <- Sys.time()
cr_nn <- train(any_crisis ~ num_services + no_shows + no_shows_last + BirthGender +
                 Age + cum_crisis + diag_cat,
               data = cr_train, 'nnet', metric = "Kappa",
               trControl = train_control, tuneGrid = grid, verbose=TRUE)
timer_end <- Sys.time()
(timer_end - timer_begin)

cr_nn$results
str(cr_nn$results)
write.csv(cr_nn$results, file = "G:/888/model results/cr_nn_02.csv")

cr_nn$bestTune
cr_nn$results$Kappa

# neural net model evaluation
y_pred_train = predict(cr_nn, data=cr_train)
#y_pred_train = y_pred_train[,2]
train_conf_mat = table(cr_train$any_crisis, y_pred_train)
(train_acc = (train_conf_mat[1,1] + train_conf_mat[2,2])/sum(train_conf_mat))

y_pred_test = predict(cr_nn, newdata=cr_test)
test_conf_mat = table(cr_test$any_crisis, y_pred_test)
(test_acc = (test_conf_mat[1,1] + test_conf_mat[2,2])/sum(test_conf_mat))
(test_sens = test_conf_mat[2,2] / (test_conf_mat[2,2] + test_conf_mat[2,1]))

sum(test_conf_mat)
100*19516/(19516+1146)

# AUC ROC
prob <- predict(cr_nn, newdata=cr_test, type="raw")
prob = as.vector(as.numeric(prob))
pred <- prediction(prob, cr_test$any_crisis)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "Crisis - Neural Net - ROC")
performance(pred, "auc")



# visualize model comparison
y_pred_test = predict(cr_lr, newdata=cr_test, type = "raw")
confusionMatrix(y_pred_test, cr_test$any_crisis)

y_pred_test = predict(cr_rf, newdata=cr_test, type = "raw")
confusionMatrix(y_pred_test, cr_test$any_crisis)

y_pred_test = predict(cr_ada, newdata=cr_test, type = "raw")
confusionMatrix(y_pred_test, cr_test$any_crisis)

y_pred_test = predict(cr_svm, newdata=cr_test, type = "raw")
confusionMatrix(y_pred_test, cr_test$any_crisis)

y_pred_test = predict(cr_nn, newdata=cr_test, type = "raw")
confusionMatrix(y_pred_test, cr_test$any_crisis)

cr_compare <- data.frame(model=character(),
                         accuracy=numeric(), 
                         precision=numeric(), 
                         recall=numeric(),
                         AUC=numeric()
)

cols <- c('model', 'accuracy', 'precision', 'recall', 'AUC')

cr_lr_compare <- as.data.frame(list('LogRegression', 0.8426, 0.0618, 0.5865, 0.7168))
cr_rf_compare <- as.data.frame(list('RandForest', 0.8689, 0.0825, 0.6682, 0.7703))
cr_ada_compare <- as.data.frame(list('AdaBoost', 0.8064, 0.0580, 0.6870, 0.7477))
cr_svm_compare <- as.data.frame(list('SVM', 0.8947, 0.0882, 0.5606, 0.7305))
cr_nn_compare <- as.data.frame(list('ANN', 0.7553, 0.0520, 0.7809, 0.7679))

colnames(cr_lr_compare) <- cols
colnames(cr_rf_compare) <- cols
colnames(cr_ada_compare) <- cols
colnames(cr_svm_compare) <- cols
colnames(cr_nn_compare) <- cols

cr_compare <- rbind(cr_lr_compare, cr_rf_compare, cr_ada_compare, cr_svm_compare, cr_nn_compare)

# plot
plot.default(cr_compare$model, cr_compare$accuracy, axes=FALSE, type="o", col=col_acc, lty=lty_acc, 
             pch=pch_acc, xlab="model", ylab='value', ylim=c(0,1), main="Crisis Model Performance")
axis(side=1, at=cr_compare$model, labels=cr_compare$model)
axis(side=2)
lines(cr_compare$precision, type="o", col=col_prec, lty=lty_prec, pch=pch_prec, lwd=2)
lines(cr_compare$recall, type="o", col=col_recall, lty=lty_recall, pch=pch_recall)
lines(cr_compare$AUC, type="o", col=col_auc, lty=lty_auc, pch=pch_auc, lwd=2)
legend(x=4.1, y=0.55, legend=c("Accuracy", "Precision", "Recall", "AUC"), box.lty=0,
       lty=c(lty_acc, lty_prec, lty_recall, lty_auc), 
       pch=c(pch_acc, pch_prec, pch_recall, pch_auc), 
       col=c(col_acc, col_prec, col_recall, col_auc),
       lwd=c(1, 2, 1, 2))


# bar plot
crisis_comp <- read.csv("G:/888/final_frames/metrics_crisis.csv")
crisis_comp$model <- factor(crisis_comp$model, levels = c("LogRegression", "RandForest", "AdaBoost", "SVM", "ANN"))
ggplot(data=crisis_comp, aes(x=model, y=value, fill=metric)) +
  geom_bar(stat='identity', position=position_dodge(), color="black") +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Crisis Model Peformance")
