fake_token <- rtoot:::get_token_from_envvar(
  "RTOOT_DEFAULT_TOKEN",
  check_stop = FALSE
)
fake_token$type <- "user"
fake_token$instance <- "fosstodon.org"

test_that("get_timeline_hashtag", {
  vcr::use_cassette("get_timeline_hashtag_default", {
    x <- get_timeline_hashtag(
      hashtag = "#rstats",
      limit = 5,
      token = fake_token
    )
  })
  expect_true(nrow(x) == 5)
  expect_true("tbl_df" %in% class(x))
  vcr::use_cassette("get_timeline_hashtag_default_nohash", {
    x <- get_timeline_hashtag(
      hashtag = "rstats",
      limit = 5,
      token = fake_token
    )
  })
  expect_true(nrow(x) == 5)
  expect_true("tbl_df" %in% class(x))
  vcr::use_cassette("get_timeline_hashtag_default_noparse", {
    x <- get_timeline_hashtag(
      hashtag = "rstats",
      limit = 5,
      parse = FALSE,
      token = fake_token
    )
  })
  expect_false("tbl_df" %in% class(x))
  vcr::use_cassette("get_timeline_hashtag_instance", {
    x <- get_timeline_hashtag(
      hashtag = "rstats",
      limit = 5,
      instance = "mastodon.social",
      token = fake_token
    )
  })
  expect_true(nrow(x) == 5)
  expect_true("tbl_df" %in% class(x))
  vcr::use_cassette("get_timeline_hashtag_anonymous", {
    x <- get_timeline_hashtag(
      hashtag = "rstats",
      limit = 5,
      instance = "mastodon.social",
      anonymous = TRUE,
      token = fake_token
    )
  })
  expect_true(nrow(x) == 5)
  expect_true("tbl_df" %in% class(x))
})
