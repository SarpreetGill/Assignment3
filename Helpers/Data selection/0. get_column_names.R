
get_colnames <- function(data_source) {
  data_MS <- data_source %>% summarise_all(~(sum(is.na(.))/n()))
  data_MS <- gather(data_MS, key = "variables", value = "percent_missing")
  return data_MS[data_MS[2]<0.25,][[1]]
}