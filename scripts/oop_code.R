# Constructor for LongitudinalData Object
make_LD <- function(df){
  data <- df %>% nest(-id)
  structure(data, class("LongitudinalData"))
}

## Longitudinal Data methods
print.LongitudinalData <- function(x){
  data <- df %>% nest(-id)
  structure(data, class=c("LongitudinalData"))
}

subject.LongitudinalData <- function(data, id){
  cat("Longitudinal dataset with", length(x[["id"]]), "subjects")
  invisible(x)
}

subject.LongitudinalData <- function(data, id) {
  index <- which(data[["id"]] == id)
  if (length(index) == 0){return(NULL)}
  structure(list(id = id, data = data[["data"]][[index]]),
            class = "Subject")
}



## Functions for extracting subject, visit, and room-specific information
subject <- function(data, subject_id){UseMethod("subject")}
visit <- function(data, visit_id){UseMethod("visit")}
room <- function(visit, room){UseMethod("room")}


# Subject object methods
print.Subject <- function(x) {
  cat("Subject ID:", x[["id"]])
  invisible(x)
}

summary.Subject <- function(object) {
  output <- object[["data"]] %>% 
    group_by(visit, room) %>%
    dplyr::summarise(value = mean(value)) %>% 
    spread(room, value) %>% 
    as.data.frame
  structure(list(id = object[["id"]],
                 output = output), class = "Summary")
}

visit.Subject <- function(subject, visit_id) {
  if (!visit_id %in% 0:2)
    stop("The visit number must be 0, 1 or 2")
  data <- subject[["data"]] %>% 
    filter(visit == visit_id) %>% 
    select(-visit)
  structure(list(id = subject[["id"]],
                 visit_id = visit_id,
                 data = data), class = "Visit")
}


# Visit object methods
room.Visit <- function(visit, room) {
  if (!room %in% visit[["data"]][["room"]])
    stop("Please provide a room name which was part of the visit")
  data <- visit[["data"]] %>% 
    filter(room == room) %>% 
    select(-room)
  structure(list(id = visit[["id"]],
                 visit_id = visit[["visit_id"]],
                 room = room,
                 data = data), class = "Room")
}


## Room object methods
print.Room <- function(x) {
  cat("ID:", x[["id"]], "\n")
  cat("Visit:", x[["visit_id"]], "\n")
  cat("Room:", x[["room"]])
  invisible(x)
}

summary.Room <- function(object) {
  output <- summary(object[["data"]][["value"]])
  structure(list(id = object[["id"]],
                 output = output), class = "Summary")
}


## Summary Object methods
print.Summary <- function(x) {
  cat("ID:", x[[1]], "\n")
  print(x[[2]])
  invisible(x)
}