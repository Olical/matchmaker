.PHONY: release dev

release:
	lein clean
	rm -rf docs
	lein release
	cp -r public docs
	rm -rf docs/js/release
	@echo "Done! Commit the changes to 'docs' directory to release on GitHub pages."

dev:
	lein figwheel
