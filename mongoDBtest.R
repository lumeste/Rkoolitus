library(mongolite)

m <- mongo(collection = "posts")

m$count()
out <- m$find('{"body" : "post"}')
out
m$import(file(post), bson = TRUE)

m$drop
