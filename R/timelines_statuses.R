#' View information about a specific status
#'
#' Query the instance for information about a specific status. [get_status] returns complete information of a status.
#' [get_reblogged_by] returns who boosted a given status. [get_favourited_by] returns who favourited a given status.
#'
#' @param id character, local ID of a status in the database
#' @param instance character, the server name of the instance where the status is located. If `NULL`, the same instance used to obtain the token is used.
#' @param anonymous some API calls do not need a token. Setting anonymous to TRUE allows to make an anonymous call if possible.
#' @param parse logical, if `TRUE`, the default, returns a tibble. Use `FALSE`  to return the "raw" list corresponding to the JSON returned from the Mastodon API.
#' @inheritParams post_toot
#' @return a status or a list of users
#' @examples
#' \dontrun{
#' get_status(id = "109326579599502650")
#' get_reblogged_by(id = "109326579599502650")
#' get_favourited_by(id = "109326579599502650")
#' }
#' @export
get_status <- function(
    id,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE
) {
    process_request(
        token = token,
        path = paste0("/api/v1/statuses/", id),
        instance = instance,
        anonymous = anonymous,
        parse = parse,
        FUN = parse_status
    )
}

#' @rdname get_status
#' @export
get_reblogged_by <- function(
    id,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE
) {
    process_request(
        token = token,
        path = paste0("/api/v1/statuses/", id, "/reblogged_by"),
        instance = instance,
        anonymous = anonymous,
        parse = parse,
        FUN = v(parse_account)
    )
}

#' @rdname get_status
#' @export
get_favourited_by <- function(
    id,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE
) {
    process_request(
        token = token,
        path = paste0("/api/v1/statuses/", id, "/favourited_by"),
        instance = instance,
        anonymous = anonymous,
        parse = parse,
        FUN = v(parse_account)
    )
}

#' View statuses above and below this status in the thread
#'
#' Query the instance for information about the context of a specific status. A context contains statuses above and below a status in a thread.
#' @inheritParams get_status
#' @param parse logical, if `TRUE`, the default, returns a named list of two tibbles, representing the ancestors (statuses above the status) and descendants (statuses below the status). Use `FALSE`  to return the "raw" list corresponding to the JSON returned from the Mastodon API.
#' @return context of a toot as tibble or list
#' @export
#' @examples
#' \dontrun{
#' get_context(id = "109294719267373593", instance = "mastodon.social")
#' }
get_context <- function(
    id,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE
) {
    process_request(
        token = token,
        path = paste0("/api/v1/statuses/", id, "/context"),
        instance = instance,
        anonymous = anonymous,
        parse = parse,
        FUN = parse_context
    )
}

#' View a poll
#'
#' View a polls attached to statuses. To discover poll ID, you will need to use [get_status()] and look into the `poll`.
#' @param id character, ID of the poll in the database
#' @inheritParams get_status
#' @return a poll
#' @export
#' @examples
#' \dontrun{
#' get_poll(id = "105976")
#' }
get_poll <- function(
    id,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE
) {
    process_request(
        token = token,
        path = paste0("/api/v1/polls/", id),
        instance = instance,
        anonymous = anonymous,
        parse = parse,
        FUN = parse_poll
    )
}

#' Get the public timeline
#'
#' Query the instance for the public timeline
#' @param local logical, Show only local statuses?
#' @param remote logical, Show only remote statuses?
#' @param only_media logical, Show only statuses with media attached?
#' @param max_id character or `POSIXct` (date time), Return results older than this id
#' @param since_id character or `POSIXct` (date time), Return results newer than this id
#' @param min_id character or `POSIXct` (date time), Return results immediately newer than this id
#' @param limit integer, Maximum number of results to return
#' @param retryonratelimit If TRUE, and a rate limit is exhausted, will wait until it refreshes. Most Mastodon rate limits refresh every 5 minutes. If FALSE, and the rate limit is exceeded, the function will terminate early with a warning; you'll still get back all results received up to that point.
#' @param verbose logical whether to display messages
#' @inheritParams post_toot
#' @inheritParams get_status
#' @details `max_id`, `since_id`, and `min_id` can either be character or `POSIXct` (date time). If it is `POSXIct`, it will be converted to the so-called snowflake ID.
#' @return statuses
#' @export
#' @examples
#' \dontrun{
#' ## as tibble
#' get_timeline_public()
#' ## as list
#' get_timeline_public(parse = FALSE)
#' }
#' @references
#' https://docs.joinmastodon.org/methods/timelines/
get_timeline_public <- function(
    local = FALSE,
    remote = FALSE,
    only_media = FALSE,
    max_id,
    since_id,
    min_id,
    limit = 20L,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE,
    retryonratelimit = TRUE,
    verbose = TRUE
) {
    params <- handle_params(
        list(
            local = local,
            remote = remote,
            only_media = only_media,
            limit = min(limit, 40)
        ),
        max_id,
        since_id,
        min_id
    )
    process_request(
        token = token,
        path = "/api/v1/timelines/public",
        instance = instance,
        params = params,
        anonymous = anonymous,
        parse = parse,
        FUN = v(parse_status),
        n = limit,
        retryonratelimit = retryonratelimit,
        verbose = verbose
    )
}

#' Get hashtag timeline
#'
#' Query the instance for the timeline of a specific hashtag
#' @param hashtag character, Content of a #hashtag. The hash is optional
#' @inherit get_timeline_public
#' @inherit get_timeline_public details
#' @export
#' @examples
#' \dontrun{
#' get_timeline_hashtag(hashtag = "#ichbinhanna")
#' ## anonymously
#' get_timeline_hashtag(hashtag = "ichbinhanna", instance = "mastodon.social", anonymous = TRUE)
#' ## Search for toots by date
#' get_timeline_hashtag(hashtag = "ichbinhanna", instance = "mastodon.social", anonymous = TRUE,
#' max_id = as.POSIXct("2024-03-01"))
#' }
get_timeline_hashtag <- function(
    hashtag = "rstats",
    local = FALSE,
    only_media = FALSE,
    max_id,
    since_id,
    min_id,
    limit = 20L,
    instance = NULL,
    token = NULL,
    anonymous = FALSE,
    parse = TRUE,
    retryonratelimit = TRUE,
    verbose = TRUE
) {
    params <- handle_params(
        list(local = local, only_media = only_media, limit = min(limit, 40L)),
        max_id,
        since_id,
        min_id
    )
    path <- paste0("/api/v1/timelines/tag/", gsub("^#+", "", hashtag))
    process_request(
        token = token,
        path = path,
        instance = instance,
        params = params,
        anonymous = anonymous,
        parse = parse,
        FUN = v(parse_status),
        n = limit,
        retryonratelimit = retryonratelimit,
        verbose = verbose
    )
}

#' Get home and list timelines
#'
#' Query the instance for the timeline from either followed users or a specific list. These functions can only be called with a user token from [create_token()].
#' @param list_id character, Local ID of the list in the database.
#' @inherit get_timeline_public
#' @inherit get_timeline_public details
#' @export
#' @examples
#' \dontrun{
#' get_timeline_home()
#' }
get_timeline_home <- function(
    local = FALSE,
    max_id,
    since_id,
    min_id,
    limit = 20L,
    token = NULL,
    parse = TRUE,
    retryonratelimit = TRUE,
    verbose = TRUE
) {
    params <- handle_params(
        list(local = local, limit = min(limit, 40L)),
        max_id,
        since_id,
        min_id
    )
    process_request(
        token = token,
        path = "/api/v1/timelines/home",
        params = params,
        parse = parse,
        FUN = v(parse_status),
        n = limit,
        retryonratelimit = retryonratelimit,
        verbose = verbose
    )
}

#' @rdname get_timeline_home
#' @export
#' @examples
#' \dontrun{
#' get_timeline_list("<listid>")
#' }
get_timeline_list <- function(
    list_id,
    max_id,
    since_id,
    min_id,
    limit = 20L,
    token = NULL,
    parse = TRUE,
    retryonratelimit = TRUE,
    verbose = TRUE
) {
    params <- handle_params(
        list(limit = min(limit, 40L)),
        max_id,
        since_id,
        min_id
    )
    process_request(
        token = token,
        path = paste0("/api/v1/timelines/list/", list_id),
        params = params,
        parse = parse,
        FUN = v(parse_status),
        n = limit,
        retryonratelimit = retryonratelimit,
        verbose = verbose
    )
}
