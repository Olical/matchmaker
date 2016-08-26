.PHONY: release dev

release:
	lein clean
	-rm -r docs
	lein release
	cp -r public docs
	rm -r docs/js/release
	@echo "Done! Commit the changes to 'docs' directory to release on GitHub pages."

dev:
	lein figwheel
