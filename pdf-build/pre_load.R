
pkgs <- c("configr","tidyverse","lubridate","here","magrittr","kableExtra")
pkgs_to_install <- setdiff(pkgs,row.names(installed.packages()))
install.packages(pkgs_to_install)
#pkgs <- vapply(pkgs,library,logical(1),character.only=T)
pkgs_loaded <- lapply(pkgs,library,character.only=T)


### General
{
  
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
  
  space_out <- function(x,inserter,surround=T){
    
    y <- c(matrix(c(rep(inserter,each=length(x)),
                    x),
                  byrow=T,
                  ncol=length(x)))
    if(surround){
      c(y,inserter)
    } else {
      y[-(1:length(inserter))]
    }
  }
}



### EXPERIENCE
{

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

  make_experience_indvidual <- function(.x){
    c("\\textbf{\\large <title>}",
      "\\textit{\\textcolor{lightgray}{<company>}}",
      "<month_start> - <month_end>",
      "<str_wrap(description,width=60)>") %>%
      paste0(collapse="\n") %>%
      str_glue_data(.x,.,.open="<",.close=">") %>%
      as.character %>%
      gsub("&","\\\\&",.) %>%
      kableExtra::linebreak()
  }
  
  draw_circle <- function(fill){
    if_else(fill,
            "A",
            "B")
  }
  
  make_experience <- function(n){
    X <- get_experience()[1:n,]
    
    cells <- X %>%
      split(1:nrow(.)) %>%
      map_chr(make_experience_indvidual) %>%
      paste0("\\fbox{\\placetextbox{0.6}{",seq(0.2,0.8,length.out=n),"}{",.,"}}")

    stem <- is.na(X$date_end) %>%
      draw_circle %>%
      space_out(" ")
    
    left_header <- "Experience"
    
    left_col <- cells[which((1:n) %% 2 == 0)] %>%
      space_out(c(" "," "," "),surround=F) %>%
      c(left_header," ",.," "," ")
    
    right_col <- cells[which((1:n) %%2 == 1)] %>%
      space_out(c(" "," "," "),surround=F) %>%
      c(" "," ")
    
    if(n %% 2 == 1){
      left_col <- c(left_col,c(" "," "))
    } else {
      right_col <- c(right_col,c(" "," "))
    }
    
    #paste0("\\begin{tabular}{lcl}\n",
    #       paste0(left_col," & ",stem," & ",right_col,collapse="\\\\\n"),
    #       "\n\\end{tabular}")
    paste0(cells,collapse="\n")
  }
}

### Publications
{
  
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

}
