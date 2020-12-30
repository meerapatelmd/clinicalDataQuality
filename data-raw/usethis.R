library(readr)
CONDITION_OCCURRENCE <- readr::read_csv("/Users/meerapatel/GitHub/projects/clinical-data-quality/data-raw/CONDITION_OCCURRENCE.csv")
DRUG_EXPOSURE <- readr::read_csv("/Users/meerapatel/GitHub/projects/clinical-data-quality/data-raw/DRUG_EXPOSURE.csv")
MEASUREMENT <- readr::read_csv("/Users/meerapatel/GitHub/projects/clinical-data-quality/data-raw/MEASUREMENT.csv")
OBSERVATION <- readr::read_csv("/Users/meerapatel/GitHub/projects/clinical-data-quality/data-raw/OBSERVATION.csv")
PROCEDURE_OCCURRENCE <- readr::read_csv("/Users/meerapatel/GitHub/projects/clinical-data-quality/data-raw/PROCEDURE_OCCURRENCE.csv")
usethis::use_data(
	CONDITION_OCCURRENCE,
	DRUG_EXPOSURE,
	MEASUREMENT,
	OBSERVATION,
	PROCEDURE_OCCURRENCE,
overwrite = TRUE
)
