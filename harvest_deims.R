library(purrr)

## takes 560 seconds for 1240 sites
get_all_sites_as_big_json <- function(){
  ids <- jsonlite::fromJSON('https://deims.org/api/sites/') %>% 
    ## head %>%
    .$id %>% .$suffix
  get_single_json <- function(id){
    jsonlite::read_json(paste0('https://deims.org/api/sites/',id),
                        simplifyVector = TRUE ## important!
    )
  }
  big_tree_list <- list()
  ids %>%
    purrr::walk(function(id){
      ## print(id);
      big_tree_list[[id]] <<- get_single_json(id)
    })
  big_tree_list
}


## convert big nested list of site details into tibble:
get_site_details_from_json <- function(jsons){
  attribute_strings <- get_attribute_strings(jsons)
  setNames(
    attribute_strings %>%
      map(., function(attribute_path){
        plucklist = strsplit(attribute_path,'\\.')  %>% unlist    
        jsons %>%
          map(~ pluck(.x, !!!plucklist),.default = NA) %>%
          map(~ paste(.x, collapse='|')) %>%
          unlist
      }),
    attribute_strings
  ) %>%
    as_tibble %>%
    rename('id' = 'id.suffix')
}



starttime = Sys.time()

while(TRUE){
  # download DEIMS data each Monday:
  if(weekdays(x = Sys.time()) == 'Monday'){
    big_tree_list <- get_all_sites_as_big_json()
    # site_details <- get_site_details_from_json(big_tree_list)
    save(big_tree_list, file = './www/data/site-details-as-json.RData')
  }
  Sys.sleep(3600 * 24) ## seconds
}

