pacman::p_load(tidyverse, purrr, tidyr, crayon, pokerena)
devtools::load_all()

### Start poker server
serv <- server$new()
# serv$forward(6)

#' @post /register
#' @serializer unboxedJSON
function(req){
        
   players <- jsonlite::fromJSON(req$postBody)
   
   glimpse(players)
   
   serv$register(players)        
   
   # return(tibble(status = 1, players = paste(players$name, collapse = ", ")))
}

#' @get /registered
#' @serializer unboxedJSON
function(req){
   return(serv$waitlist)
}


#' @get ping
#' @serializer unboxedJSON
function(){
   message("Somebody pinged me, so I pong back!")
   return(list(pong = T))
}


#' @post /reload
function(req){
 devtools::load_all()
 return()
}

#' @post /start
#' @serializer unboxedJSON
function(req){
  
    players <- serv$forward(10)%>% 
      dplyr::mutate(fun = purrr::map(fun, ~ eval(parse(text = .x)))) %>% 
      glimpse
    
    # players$fun[[1]](tibble(to_call = 10)) %>% glimpse
    
    config <- jsonlite::fromJSON(req$postBody) %>% dplyr::mutate_if(is.numeric, as.double) %>% glimpse

    tourn <- pokerena::tournament$new(players, config)
    tourn$run(T)
        
}
