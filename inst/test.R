res <- obt_req()

links <- res |>
  html_elements("a") |>
  html_attr("href")

