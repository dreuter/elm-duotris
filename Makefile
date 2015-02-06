ELM-SRCS=$(shell ls *.elm)

deploy: out.html
	set -e
	git clone -b gh-pages https://${GH_TOKEN}@github.com/dreuter/elm-duotris gh-pages

	cp out.html gh-pages/try/index.html

	export COMMIT_HASH=$(git rev-parse HEAD)

	# commit and push generated content to `master' branch
	# since repository was cloned in write mode with token auth - we can push there
	cd gh-pages

	git config user.email "daniel.robin.reuter@googlemail.com"
	git config user.name "Daniel Reuter (autocommit via travis)"
	git add -A .
	git commit -a -m "Autogenerated try page from commit ${COMMIT_HASH}. Travis build number #$TRAVIS_BUILD_NUMBER"
	git push --quiet origin gh-pages > /dev/null 2>&1

out.html: installElm ${ELM-SRCS}
	./Elm-Platform/0.14/bin/elm-make duotris.elm --output out.html --yes

installElm:
	curl -O https://raw.githubusercontent.com/elm-lang/elm-platform/master/installers/BuildFromSource.hs
	runhaskell BuildFromSource.hs 0.14
	touch installElm
