
pkgs <- c("configr","tidyverse","lubridate","here","magrittr","kableExtra")
pkgs_to_install <- setdiff(pkgs,row.names(installed.packages()))
install.packages(pkgs_to_install)
#pkgs <- vapply(pkgs,library,logical(1),character.only=T)
pkgs_loaded <- lapply(pkgs,library,character.only=T)

read_TOML_header <- function(filename){
  raw_file <- readLines(filename)
  plus_rows <- which(raw_file == "+++")
  keep_rows <- (plus_rows[1]+1):(plus_rows[2]-1)
  file_header <- raw_file[keep_rows]
  gsub("#.*$","",file_header)
}

extract_config <- function(raw){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(raw,tmp)
  configr::read.config(tmp)
}

basic_flatten <- function(x){
  nm_list <- lapply(x,names)
  full_nms <- unique(unlist(nm_list))
  nms_to_be_added <- lapply(nm_list,setdiff,x=full_nms)
  
  if(any(length(nms_to_be_added)>0))
    for(i in 1:length(x))
      if(length(nms_to_be_added[[i]])>0)
        for(c_nm in nms_to_be_added[[i]])
          x[[i]][[c_nm]] <- NA
  
  Reduce(rbind,lapply(x,as.data.frame))
}

paste_authors <- function(x){
  for(i in 1:length(x)){
    aut <- x[[i]]$authors
    aut[aut == "michael-barrowman"] <- "**Michael A Barrowman**"
    n_aut <- length(aut)
    if(n_aut == 2){
      aut <- paste0(aut[1]," & ",aut[2])
    } else {
      aut <- paste0(paste0(aut[-n_aut],collapse=", ")," & ",aut[n_aut])
    }
    x[[i]]$authors <- aut
    x[[i]]$image <- NULL
  }
  x
}

get_experience <- function(){
  exp_file_path <- here::here("content","experience","experience.md")
  exp_TOML <- read_TOML_header(exp_file_path)
  exp_data <- extract_config(exp_TOML)
  weight <- exp_data$weight
  exp <- exp_data$experience
  exp_tbl <- basic_flatten(exp)
  exp_tbl$date_start <- lubridate::ymd(exp_tbl$date_start)
  exp_tbl$date_end <- lubridate::ymd(exp_tbl$date_end)
  exp_tbl$month_start <- format(exp_tbl$date_start,"%b %Y")
  exp_tbl$month_end <- format(exp_tbl$date_end,"%b %Y")
  exp_tbl$month_end[is.na(exp_tbl$month_end)] <- "Present"
  
  exp_tbl
}

get_publications <- function(){
  pub_dir <- here::here("content","publication")
  pubs_list <- list.dirs(pub_dir,recursive = F)
  pubs_index_files <- vapply(pubs_list,file.path,character(1),"index.md")
  
  pubs_YAML_list <- lapply(pubs_index_files,configr::read.config)
  pubs_YAML_authored <- paste_authors(pubs_YAML_list)
  
  pubs_tbl <- basic_flatten(pubs_YAML_authored)
  pubs_tbl$year <- lubridate::year(lubridate::ymd(pubs_tbl$date))
  
  pubs_tbl
}














