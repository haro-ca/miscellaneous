all: clean reports

DATA_RAW = ./data-raw
DATA = ./data
DOC = ./doc
VIGNETTES = ./vignettes
PANDOC =  /usr/lib/rstudio/bin/pandoc

## commands	: show all commands.
commands : 
	@ grep '^##' Makefile | sed -e 's/##/-/g'

## clean		: borrado de los datos
clean	: 
	-@ find data/ -type f -name '*.fst' | xargs rm 
	@ echo 'Se borraron los contenidos de ./data'

## tidy		: make tidy dataset for analysis
tidy: $(DATA)/flights.fst \
      $(DATA)/flights_clean.fst

$(DATA)/flights.fst: $(DATA_RAW)/01*.R
	Rscript $(DATA_RAW)/01*.R

$(DATA)/flights_clean.fst: $(DATA)/flights.fst \
			   			   $(DATA_RAW)/02*.R
	Rscript $(DATA_RAW)/02*.R

## reports	: create reports
.PHONY: reports
reports: $(DOC)/Graficas_de_flights.html

$(DOC)/Graficas_de_flights.html: $(DATA)/flights.fst \
								 $(DATA)/flights_clean.fst \
								 $(VIGNETTES)/Graficas_de_flights.Rmd
	Rscript -e "rmarkdown::render(input = here::here('vignettes', 'Graficas_de_flights.Rmd'), output_dir = here::here('doc'))"


## clean_and_tidy: runs clean and tidy commands
clean_and_tidy: clean tidy reports 


## test_reports	: prueba de rmarkdown en make
test_reports	: 
	export RSTUDIO_PANDOC=$(PANDOC)
	Rscript -e "rmarkdown::render(input = here::here('vignettes', 'Graficas_de_flights.Rmd'), output_dir = here::here('doc'))"





