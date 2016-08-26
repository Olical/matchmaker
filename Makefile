.PHONY: clean release dev

default: release

clean:
	lein clean

release: clean
	lein release
	cp -r public docs
	rm -rf docs/js/release
	@echo "Commit the changes to 'docs' directory to release on GitHub pages."

dev:
	lein figwheel
