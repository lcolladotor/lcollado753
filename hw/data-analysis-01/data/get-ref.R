## Get reference tables
ref <- data.frame(name = c("Units", "StatesAndCounty", "Collection", "Duration"), url = c("https://aqs.epa.gov/aqsweb/codes/data/Units.csv", "https://aqs.epa.gov/aqsweb/codes/data/StateCountyCodes.csv", "https://aqs.epa.gov/aqsweb/codes/data/CollectionFrequencies.csv", "https://aqs.epa.gov/aqsweb/codes/data/SampleDurationCodes.csv"), file = c("Units.csv", "StateCountyCodes.csv", "CollectionFrequencies.csv", "SampleDurationCodes.csv"))

## Download reference files
apply(ref, 1, function(x) {
	download.file(x[2], destfile = x[3], method="curl")
})

## Save info
dateDownloaded <- date()
save.image("raw-ref.Rdata")