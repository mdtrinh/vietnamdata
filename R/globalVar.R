# add ..count.. to global variables to avoid R CMD check NOTE
if(getRversion() >= "2.15.1")  utils::globalVariables(c("..count.."))
