## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----auth, eval=FALSE---------------------------------------------------------
#  library(rtoot)
#  auth_setup()

## ----name, eval=FALSE---------------------------------------------------------
#  auth_setup(name = "account1")

## ----token, eval=FALSE--------------------------------------------------------
#  token <- readRDS(file.path(tools::R_user_dir("rtoot", "config"), "account1.rds"))
#  get_status(id = "109297677620300632", instance = "mastodon.social", token = token)

## ----options, eval=FALSE------------------------------------------------------
#  options("rtoot_token" = file.path(tools::R_user_dir("rtoot", "config"), "account1.rds"))

## ----clipboard, eval = FALSE--------------------------------------------------
#  auth_setup(clipboard = TRUE)

## ----convert, eval = FALSE----------------------------------------------------
#  token <- readRDS(file.path(tools::R_user_dir("rtoot", "config"), "account1.rds"))
#  content <- convert_token_to_envvar(token)

