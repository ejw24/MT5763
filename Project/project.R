ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

libs <- c("ggplot2", 
          "gridExtra",
          "tidyverse",
          "git2r",
          "car",
          "minpack.lm")

ipak(libs)
setwd("~/UNI/2019:2020/MT5763/MY5763/Project")

ascotdata <- read_csv("~/UNI/2019:2020/MT5763/MY5763/Project/ascot data.csv")
