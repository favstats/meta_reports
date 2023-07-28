# Hier ist die eltzte Version: 
# library(reticulate)
# # library(tidyverse)
# library(glue)
# library(rvest)
options(python_init = TRUE)

walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}


# install.packages("pacman")
pacman::p_load(reticulate, dplyr, stringr, lubridate, purrr, glue, rvest, cli, digest, glue, rvest)

cntry <- "ES"

library(playwrightr)
library(dplyr)
pw_init(use_xvfb = T)

# Launch the browser

browser_df <- browser_launch(headless = F, browser = "firefox", 
                             user_agent = NULL,
                             user_data_dir = "out")

# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


pw_restart <- function(){
  reticulate::py_run_string("p.stop()")
  pw_init(use_xvfb = T)
  reticulate::py_run_string("p = sync_playwright().start()")
}



on <- function(page_df, event, lambda_string){
  playwrightr:::py_run(glue('{page_df$page_id}.on("{event}", {lambda_string})'))
  return(page_df)
}
off <- function(page_df, event, lambda_string){
  playwrightr:::py_run(glue('{page_df$page_id}.remove_listener("{event}", {lambda_string})'))
  return(page_df)
}

execute_script <- function (page_df, script) {
  playwrightr:::py_run(glue("d = {{page_df$page_id}}.evaluate('{{script}}')"))
}

page_df %>% 
  goto("https://www.facebook.com/ads/library/report") %>% 
  screenshot("/data/res/facebook_add_reports/test.png")

Sys.sleep(2)

# page_df %>% screenshot("/data/res/facebook_add_reports/test.png")

try({
  page_df %>% 
    get_by_test_id("cookie-policy-manage-dialog-accept-button") %>%
    slice(1) %>%
    click() %>% 
    screenshot("/data/res/facebook_add_reports/test.png")
})


# Write post-data string to disk into tmp
tmp_post_data_string <- paste0(digest::digest("YOOYOOo"), ".txt")#  tempfile(fileext = ".txt")
# page_df %>% on("request", glue::glue('lambda request: print(request.url)'))
# page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'))
# page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'))
page_df %>% on("request", glue::glue('lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'))
page_df %>% on("request", glue::glue('lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'))

# Print Console
# tmp_download_link <- tempfile()
tmp_download_link <- paste0(digest::digest("sdff"), ".txt")#  tempfile(fileext = ".txt")

page_df %>% on("console", "lambda msg: open('{tmp_download_link}', 'w').write(msg.text)")

# First click to obtain the request post-body-data
page_df %>% 
  get_by_text("Download report") %>%
  slice(2) %>%
  click()

# Print download path
tmp_download_path <-  paste0(digest::digest("sdsdfsdfdff"), ".txt")#  
page_df %>% on("download", glue::glue('lambda download: open("{tmp_download_path}", "w").write(download.path())'))

data_string <- readLines(tmp_post_data_string, warn = F) %>% 
  str_squish() %>%
  glimpse


# countries <- tibble::tibble(country = c("NL", "DE", "CA", "FR", "US"))
countries <- tibble::tibble(country = countrycode::codelist$iso2c) %>%
  filter(!is.na(country)) %>%
  glimpse
# countries <- fs::file_info(dir("/data", recursive = T, full.names = T)) %>%
#   filter(size > 1) %>%
#   pull(path) %>%
#   fs::path_dir() %>%
#   fs::path_file() %>%
#   unique
# readr::write_rds(countries, "data/countries.rds")
# 
# countries <- tibble::tibble(country = readr::read_rds("data/countries.rds")) %>%
#   filter(!is.na(country)) %>%
#   glimpse

# days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("01-07-2019"), lubridate::today(), by = 30))) %>%
days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("01-07-2023"), lubridate::today(), by = 1))) #%>%
# head(-7)

dt <- expand_grid(countries, days) %>%
  glimpse

dt %>%
  # arrange(day, country != "RU") %>%
  filter(country == cntry) %>% 
  arrange(country, day) %>% 
  split(1:nrow(.)) %>% #bashR::simule_map(1)
  walk_progress(~{
    
    file_name <- glue::glue("report/{.x$country}/{as.character(.x$day)}.zip")
    if(fs::file_exists(file_name)) return()
    
    c::cli_alert_info(glue::glue("{.x$country} - {.x$day}"))
    
    path_dir <- fs::path_dir(file_name)
    if(!fs::dir_exists(path_dir)) fs::dir_create(path_dir)
    
    # .x <- list(day = "2021-12-17", country = "NL")
    # .x <- list(day = "2021-12-16", country = "NL")
    # .x <- list(day = "2021-01-17", country = "NL")
    # time_preset <- "lifelong"
    time_preset <- "yesterday"
    # time_preset <- "last_90_days"
    # time_preset <- "last_365_days"
    
    js_code <- paste0('fetch("https://www.facebook.com/ads/library/report/v2/download/?report_ds=', as.character(.x$day), '&country=', .x$country, '&time_preset=', time_preset, '", {"headers": {"accept": "*/*", "content-type": "application/x-www-form-urlencoded"}, "body": "', data_string, '", "method": "POST", "mode": "cors", "credentials": "include" }).then(resp => resp.text()).then(data => console.log(data));')
    
    page_df %>% execute_script(js_code)
    Sys.sleep(.1)
    
    download_url <- readLines(tmp_download_link, warn = F) %>%  
      str_extract("\"https.*?\"") %>%
      str_remove_all("(^\")|(\"$)") %>%
      str_remove_all("\\\\") %>%
      glimpse
    
    if(is.na(download_url)){
      write(list(), file_name)
    } else if(str_detect(download_url, "facebook.com/help/contact/")) {
      # cli::cli_alert_danger("Blocked")
      # system("taskkill /IM Pulse.exe /F")
      Sys.sleep(10)
      # beepr::beep(10)
      # system('powershell "Disable-NetAdapter -Name \'WiFi\' -Confirm:$false"')
      # system('powershell "Enable-NetAdapter -Name \'WiFi\' -Confirm:$false"')
      # system('cmd /c "C:/Program Files (x86)/Common Files/Pulse Secure/JamUI/Pulse.exe"', wait = F)
      # Sys.sleep(60*5)
      # rstudioapi::jobRunScript("get_em.R")
      return("Blocked")
    } else {
      # req <- httr::GET(download_url)
      download.file(download_url, file_name, quiet = T, mode = "wb")
      # cli::cli_alert_success(object.size(req$content))
      # writeBin(req$content, file_name)
    }
    
    
    
    Sys.sleep(runif(1, 0, .3))
  })


# dir("report/ES")
# 
# the_dat <- dir("report/ES", full.names = T) %>% 
#   # keep(~str_detect(.x, "advert")) %>% 
#   # .[1:2] %>% 
#   walk_progress(unzip)
