#!/usr/bin/env Rscript
# Author  : Fahad Alswaina
# Github  : github.com/alswaina
# Email   : alswaina.fahad@gmail.com

#TODO:
# check # of regions before calculating gini
# apply gini for Plutus outputs
# move gini.setup to config file

#DEFAULT VALUES ----
#<query_numner> = <col>
#<query_numner> = c(<col1>, <col2>, .., <coln>) means summation of table.cols values
gini.setup = list(Q11 = 'value',
                  Q1 = c('2015', '2020'))

#LIBRARIES ----
library(ggplot2, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(xlsx, warn.conflicts = FALSE)


#FUNCTIONS ----
get_query_number <- function(path){
  name <- basename(path)
  return(strsplit(name,"_")[[1]][1])
}

get_file_for_query <- function(query.number){
  file.pattern <- paste0('^', query.number, '_')
  files <- list.files(path = query.path, recursive = F, full.names = T, pattern = file.pattern)
  return(files)
}

get_query_data <- function(file.path, query.number, col){
  file.path = normalizePath(file.path)
  table <- NULL
  scenario <- NULL

  table.cols <- c("scenario",col)
  if(class(try({read_csv(file.path, col_types = cols())[table.cols]}, silent = TRUE))[1] != 'try-error'){
    table <- read_csv(file.path, col_types = cols())[table.cols]
    # rename the selected col to value
    temp_col <- 'temp_result9939'
    table[temp_col] <- with(table, rowSums(table[col]))
    table[, col] <- NULL
    names(table)[names(table)==temp_col] <- 'value'
  }else{
    stop('Could not read the query table!')
  }
  scenario <- unique(table[['scenario']])
  
  record <- list(
    scenario = scenario,
    path = file.path,
    query.number = query.number,
    table = table)
  return(record)
}
#build structure
#list(<scenario-1> = list(
#       Q# = list(path = <path>, 
#             cat = {'year', 'value'}, 
#             table = <table>)), 
#     <scenario-2> = list(..), ..)
build_file_structure <- function(setup) {
  a.query.number <- names(setup[1])
  b.query.number <- names(setup[2])
  a.query.col <- setup[[1]]
  b.query.col <- setup[[2]]
  
  #get the files associated with each query
  a.files <- get_file_for_query(a.query.number)
  b.files <- get_file_for_query(b.query.number)

  files <- c(a.files, b.files)
  queries.list <- list()

  for (f in files){
    if (f %in% a.files){
      meta <- get_query_data(f, a.query.number, a.query.col)
      }else{
        meta <- get_query_data(f, b.query.number, b.query.col)
      }
    query.record <- c()
    query.number <- meta[["query.number"]]
    query.record[[query.number]] <- list(path = meta[["path"]],
                                      table = meta[["table"]])
    scenario <- meta[["scenario"]]
    queries.list[[scenario]] = c(queries.list[[scenario]], query.record)
  }
  return(queries.list)
}

validate.setup <- function(setup){
  #get the query name and value col.
  a.query.number <- names(setup[1])
  b.query.number <- names(setup[2])
  a.query.col <- setup[[1]]
  b.query.col <- setup[[2]]
  
  queries.list <- build_file_structure(setup)
  #validate number of regions inside each file/query
  # validate # scenario for both queries
  for(scenario in names(queries.list)){
    if(length(names(queries.list[[scenario]])) != 2 | length(setdiff(names(queries.list[[scenario]]), c(a.query.number, b.query.number))) != 0){
      filter <- setdiff(c(a.query.number, b.query.number), names(queries.list[[scenario]]))
      msg <- paste('SCENARIO MISSING:', '[', scenario,']', 'QUERY:','[', filter,']', '- scenario will be skipped for both queries!')
      message(msg)
      # delete scenario
      queries.list[scenario] <- NULL 
    }
  }
  return(queries.list)
}

sorted_increasting <- function(query.table, byColumn){
  sorted <- query.table[order(query.table[[byColumn]], decreasing = FALSE),]
  return(sorted)
}

main <- function(gini.setup){
  queries.list <- validate.setup(gini.setup)
  gini.table <- NULL
  a.query.number <- names(gini.setup[1])
  b.query.number <- names(gini.setup[2])
  a.query.col <- gini.setup[[1]]
  b.query.col <- gini.setup[[2]]

  #timing
  Saudi.time <- as.POSIXlt(Sys.time(), tz = "Asia/Riyadh")
  time_formated <- format(Saudi.time, "%b_%d__%H_%M")
  output.folder.name <- paste0('gini_',basename(query.path))
  output.folder.path <- paste0('.', "/", output.folder.name)

  dir.create(path = output.folder.path, showWarnings = FALSE)

  output.file.name <- paste0("gini_", a.query.number, '_', b.query.number, '_', basename(query.path),"__", time_formated)
  output.xls <- paste0(output.folder.path, '/', output.file.name, '.xlsx')
  output.png <- paste0(output.folder.path, '/', output.file.name, '.png')

  if(file.exists(output.xls)){
    msg <- paste('File:', basename(output.xls), 'is removed!')
    message(msg)
    file.remove(output.xls)
  }
  if(file.exists(output.png)){
    msg <- paste('File:', basename(output.png), 'is removed!')
    message(msg)
    file.remove(output.png)
  }

  gini.table <- list()
  if(!length(queries.list)){
    stop('No shared scenarios found between the queries!')
  }

  for (scenario in names(queries.list)) {
    a.table <- queries.list[[scenario]][[a.query.number]][["table"]]
    b.table <- queries.list[[scenario]][[b.query.number]][["table"]]

    table <-
      data.frame(list(
        scenario = a.table$scenario,
        a.value = a.table$value,
        b.value = b.table$value
      ))

    table.sorted <- sorted_increasting(table, 'a.value')
    table.sorted['ab.mult'] <- with(table.sorted, a.value * b.value)
    table.sorted['b.cumsum'] <- with(table.sorted, cumsum(b.value))
    tail.b.cumsum <- tail(table.sorted$b.cumsum, n = 1)
    table.sorted['b.cumsumFrac'] <- with(table.sorted, b.cumsum / tail.b.cumsum)
    table.sorted['ab.mult.cumsum'] <- with(table.sorted, cumsum(ab.mult))
    tail.ab.mult.cumsum <- tail(table.sorted$ab.mult.cumsum, n = 1)
    table.sorted['ab.mult.cumsumFrac'] <- with(table.sorted, ab.mult.cumsum / tail.ab.mult.cumsum)
    table.sorted['frac.diff'] <- with(table.sorted, b.cumsumFrac - ab.mult.cumsumFrac)
    
    gini <- sum(table.sorted$frac.diff[-length(table.sorted$frac.diff)]) / sum(table.sorted$b.cumsumFrac[-length(table.sorted$b.cumsumFrac)])

    equity <- 1 - gini
    total.cost <- sum(table.sorted$a.value)
    gini.row <- data.frame(list(
      scenario = scenario,
      gini= gini,
      equity = equity,
      total.cost = total.cost,
      scenario_group = sub("[-|_].*", "", scenario)
    ))
    gini.table <-rbind(gini.table, gini.row)

    write.xlsx2(
      x = table.sorted,
      file = output.xls,
      row.names = FALSE,
      sheetName = scenario,
      append = TRUE
    )
  }
  write.xlsx2(
    x = gini.table,
    file = output.xls,
    row.names = FALSE,
    sheetName = paste0('gini_table'),
    append = TRUE
  )

  gini.plot <- ggplot(data=gini.table, aes(x=equity, y=total.cost, colour=scenario)) +
    geom_point(size=2) +
    geom_line(aes(group = scenario_group),color="black", size=0.3, linetype="dotted") +
    labs(x='Equity', y='Total Mitigation Cost (Trillion 1990$)')
  gini.plot + scale_fill_manual(values=c("#CC0000", "#006600", "#080cff", "#a405e3",
                                         "#660099", "#CC0066", "#FF9999", "#FF9900",
                                         "black", "black", "black", "black", "black"))

  ggsave(filename = output.png, gini.plot)
  #return(gini.table)
}

validation_args <- function(args){
  # test if there is at least one argument: if not, return an error
  if(!length(args) %in% c(2)){
    stop("Please provide the QUERY_PATH: Rscript <file_script.R> -f <QUERY_FOLDER>", call. = FALSE)
  } else if(length(args) == 2){
    if(args[1] == "-f"){
      # default output file
      query.path <- args[2]
    }else{
      stop("Expected -f <QUERY_FOLDER> as in the following: Rscript <Rscript.R> -f <DBs_FOLDER>", call. = FALSE)
    }
  }
  return(list("query.path" = query.path))
}

tryCatch(
  {
    validation_path <- function(val, path_type){
      tryCatch({
        val <- normalizePath(val)
        if(path_type == 'folder'){
          if(!file.exists(val) | !dir.exists(val)){
            stop(paste("Folder:", val, "- doesn't exsist or not a folder"), call. = FALSE)
          }
        }
        val
      },
      error = function(cond){
        stop(cond$message, call. = FALSE)
      },
      warning = function(cond){
        stop(cond$message, call. = FALSE)
      })
      return(val)
    }
    #terminal input
    input.full <- commandArgs(trailingOnly = FALSE)
    args <- commandArgs(trailingOnly = TRUE)
    
    #extract script path
    file.arg.name <- "--file="
    script.path <- sub(file.arg.name, "", input.full[grep(file.arg.name, input.full)])
    script.path <- normalizePath(script.path)
    script.name <- basename(script.path)
    script.dir <- dirname(script.path)
    
    #args validation for db
    input.query.folder <- validation_args(args)
    query.path <-  input.query.folder[["query.path"]]
    query.path <<- validation_path(query.path, path_type = 'folder')
    
    setwd(script.dir)
    main(gini.setup)
  },
  warning = function(cond){
    warning(cond)
  },
  error = function(cond){
    stop(cond)
  }
)


