etime <- read_csv(file = '../inst/extdata/TimingEntanglementReformatDate.csv')
devtools::use_data(etime, overwrite = TRUE)

estStartDates <- read_csv(file = '../inst/extdata/EntglEstimatedStartDates.csv')
devtools::use_data(estStartDates, overwrite = TRUE)
