.PHONY: clean release dev

default: release

clean:
	lein clean

release: clean
	lein release

dev:
	lein figwheel
