.PHONY: npm test-clj test-cljs test

version-number  = 0.1.1
group-id        = io.zalky
artifact-id     = cinch
description     = Utility functions that make things a cinch
license         = :apache
url             = https://github.com/zalky/cinch

include make-clj/Makefile

.makecache/npm-install: package.json
	npm install
	@mkdir -p .makecache
	@touch .makecache/npm-install

npm: .makecache/npm-install
	@:

test-clj:
	clojure -M:test/clj

test-cljs: npm
	clojure -M:test/cljs compile ci
	karma start --single-run

test: test-clj test-cljs
	@:

nuke:
	@make nuke-super
	@rm -rf .makecache
	@rm -rf node_modules
	@rm -rf .shadow-cljs
