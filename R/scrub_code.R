# testplot <- function(meansdf, xvar = "condition", yvar = "means",
#                      fillvar = "condition") {
#   p <- ggplot(meansdf,
#               aes_string(x = xvar, y= yvar, fill = fillvar)) +
#     geom_bar(position="dodge", stat="identity")
# }
# 
# 
# 
# testplot <- function(meansdf)
# {
#   scale <- 0.5
#   p <- ggplot(meansdf, 
#               aes(fill = condition,
#                   y = means * scale,
#                   x = condition),
#               environment = environment())   # This is the KEY line 
#   p + geom_bar(position = "dodge", stat = "identity")
# }
# 
# ## Now, the following works
# testplot(means)