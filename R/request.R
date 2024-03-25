OBT_BASE_URL <- "http://webserver1.lsb.state.ok.us/"

obt_req <- function(path = "cf") {
  res <- httr2::request(OBT_BASE_URL) |>
    httr2::req_url_path(path) |>
    httr2::req_perform()

  res_status <- httr2::resp_status(res)

  if (!res_status == 200L) {
    rlang::abort("Unexpected response")
  }

  res <- httr2::resp_body_html(res)

  return(res)
}

obt_recurse_links <- function(path = "cf", levels = 2) {
  all_links <- c()

  for (i in seq_len(levels)) {
    new_links <- purrr::map(path, obt_get_links) |>
      purrr::list_c()
    path <- new_links
    all_links <- append(all_links, new_links)
  }

  # Return all collected links
  return(all_links)
}

obt_get_links <- function(path = "cf") {
  links <- obt_req(path) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  return(links[-1])
}
