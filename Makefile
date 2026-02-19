.PHONY: build test clean coverage coverage-html coverage-summary coverage-clean

build:
	dune build --root .

test:
	dune test --root .

clean:
	dune clean --root .
	$(MAKE) coverage-clean

# --- Coverage targets ---

COVERAGE_DIR := _coverage

coverage-clean:
	rm -rf $(COVERAGE_DIR)

coverage: coverage-clean
	mkdir -p $(COVERAGE_DIR)
	BISECT_FILE=$(CURDIR)/$(COVERAGE_DIR)/bisect dune test \
		--root . --instrument-with bisect_ppx --force
	bisect-ppx-report summary --coverage-path $(COVERAGE_DIR)

coverage-html: coverage-clean
	mkdir -p $(COVERAGE_DIR)
	BISECT_FILE=$(CURDIR)/$(COVERAGE_DIR)/bisect dune test \
		--root . --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(COVERAGE_DIR) -o $(COVERAGE_DIR)/html
	@echo "Coverage report: $(COVERAGE_DIR)/html/index.html"

coverage-summary:
	@bisect-ppx-report summary --coverage-path $(COVERAGE_DIR)
