server <- R6::R6Class("poker_server",
  public = list(
   
    waitlist = NULL, 
   
    initalize = function(){
     
    },
    
    register = function(player){
      if(all(player$name %in% self$waitlist$name)) return()
      
      self$waitlist <- rbind(self$waitlist, player)
    },
    
    forward = function(k){
      self$waitlist %>% dplyr::slice(1:k) %>% tidyr::drop_na(name)
    }
    # unque = function(){
    #  self$waitlist <- self$waitlist[self$waitlist$name %in% forward()]
    # }
  )
)
