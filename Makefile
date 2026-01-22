.PHONY: help all document install build test check clean run_shiny

RSCRIPT = Rscript

help: ## Show this help message
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

all: install ## Install the package (default)

document: ## Generate documentation with roxygen2
	$(RSCRIPT) -e 'devtools::document()'

install: document ## Install the package locally
	$(RSCRIPT) -e 'devtools::install()'

build: document ## Build the package tarball
	$(RSCRIPT) -e 'devtools::build()'

test: document ## Run tests
	$(RSCRIPT) -e 'devtools::test()'

check: document ## Run R CMD check
	$(RSCRIPT) -e 'devtools::check()'

app: ## Run the example Shiny app
	$(RSCRIPT) -e "shiny::runApp('inst/shiny/elevenlabs_app')"

clean: ## Clean up build artifacts
	rm -rf *.Rcheck
	rm -rf src/*.o src/*.so
	rm -f *.tar.gz
