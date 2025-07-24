## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("rtoot")

## ----aut, eval=FALSE----------------------------------------------------------
# auth_setup()

## ----eval = FALSE-------------------------------------------------------------
# get_instance_general(instance = "mastodon.social")

## ----eval = FALSE-------------------------------------------------------------
# get_instance_activity(instance = "mastodon.social")
# get_instance_trends(instance = "mastodon.social")

## ----eval = FALSE-------------------------------------------------------------
# get_timeline_public(instance = "mastodon.social")

## ----eval = FALSE-------------------------------------------------------------
# get_timeline_hashtag(hashtag = "rstats", instance = "mastodon.social")

## ----eval = FALSE-------------------------------------------------------------
# get_timeline_home()

## ----eval=FALSE---------------------------------------------------------------
# search_accounts("schochastics")

## ----eval = FALSE-------------------------------------------------------------
# id <- "109302436954721982"
# get_account_followers(id)
# get_account_following(id)
# get_account_statuses(id)

## ----eval = FALSE-------------------------------------------------------------
# post_toot(status = "my first rtoot #rstats")

## ----eval = FALSE-------------------------------------------------------------
# post_toot(status = "my first rtoot #rstats", media="path/to/media",
#           alt_text = "description of media")

