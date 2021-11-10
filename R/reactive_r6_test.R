
# Taken from 
# https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890/8 

Person <- R6::R6Class(
  "Person",
  private = list(
    name = "",
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    count = 0
  ),
  public = list(
    initialize = function(name) {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$name <- name
    },
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    },
    print = function() {
      cat("Person:", private$name)
    },
    changeName = function(newName) {
      private$name <- newName
      private$invalidate()
    },
    getName = function() {
      private$name
    }
  )
)