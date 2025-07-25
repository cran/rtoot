## tests for all "get_account_*" functions that require a user token
## get_account_followers, get_account_following, get_account_featured_tags,
## get_account_lists, get_account_relationships,get_account_bookmarks,
## get_account_favourites, get_account_blocks,
## get_account_mutes

fake_token <- rtoot:::get_token_from_envvar(
  "RTOOT_DEFAULT_TOKEN",
  check_stop = FALSE
)
fake_token$type <- "user"
fake_token$instance <- "fosstodon.org"

id <- "109302436954721982"

## These have a meaningful parse parameter, so need to test `noparse`

test_that("get_account_followers", {
  vcr::use_cassette("get_account_followers_default", {
    x <- get_account_followers(id = id, limit = 3, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
  expect_true(nrow(x) != 0)
  vcr::use_cassette("get_account_followers_noparse", {
    x <- get_account_followers(
      id = id,
      limit = 3,
      parse = FALSE,
      token = fake_token
    )
  })
  expect_false("tbl_df" %in% class(x))
})

test_that("get_account_following", {
  vcr::use_cassette("get_account_following_default", {
    x <- get_account_following(id = id, limit = 3, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
  expect_true(nrow(x) != 0)
  vcr::use_cassette("get_account_following_noparse", {
    x <- get_account_following(
      id = id,
      limit = 3,
      parse = FALSE,
      token = fake_token
    )
  })
  expect_false("tbl_df" %in% class(x))
})

test_that("get_account_bookmarks", {
  vcr::use_cassette("get_account_bookmarks_default", {
    x <- get_account_bookmarks(limit = 3, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
  expect_true(nrow(x) != 0)
  vcr::use_cassette("get_account_bookmarks_noparse", {
    x <- get_account_bookmarks(limit = 3, parse = FALSE, token = fake_token)
  })
  expect_false("tbl_df" %in% class(x))
})

test_that("get_account_favourites", {
  vcr::use_cassette("get_account_favourites_default", {
    x <- get_account_favourites(limit = 3, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
  expect_true(nrow(x) != 0)
  vcr::use_cassette("get_account_favourites_noparse", {
    x <- get_account_favourites(limit = 3, parse = FALSE, token = fake_token)
  })
  expect_false("tbl_df" %in% class(x))
})

test_that("get_account_blocks", {
  vcr::use_cassette("get_account_blocks_default", {
    x <- get_account_blocks(limit = 3, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
  expect_true(nrow(x) != 0)
  vcr::use_cassette("get_account_blocks_noparse", {
    x <- get_account_blocks(limit = 3, parse = FALSE, token = fake_token)
  })
  expect_false("tbl_df" %in% class(x))
})

test_that("get_account_mutes", {
  ## sorry rfortunes, you get muted for science too
  vcr::use_cassette("get_account_mutes_default", {
    x <- get_account_mutes(limit = 3, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
  expect_true(nrow(x) != 0)
  vcr::use_cassette("get_account_mutes_noparse", {
    x <- get_account_mutes(limit = 3, parse = FALSE, token = fake_token)
  })
  expect_false("tbl_df" %in% class(x))
})

## no meaningful parse parameter; no need to test noparse
test_that("get_account_featured_tags", {
  vcr::use_cassette("get_account_featured_tags_default", {
    x <- get_account_featured_tags(id = id, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
})

test_that("get_account_lists", {
  vcr::use_cassette("get_account_lists_default", {
    x <- get_account_lists(id = id, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
})

test_that("get_account_relationships", {
  vcr::use_cassette("get_account_relationships_default", {
    x <- get_account_relationships(id = id, token = fake_token)
  })
  expect_true("tbl_df" %in% class(x))
})
