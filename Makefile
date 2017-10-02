# Building the docs

LOCAL_GHC_DOCS=/usr/lib/ghc-doc/haddock/ghc/base-4.9.1.0/base.haddock

doc: Exercise_2/*.hs Exercise_3/*.hs
	haddock \
		-o doc \
		--html \
		--hyperlinked-source \
		--read-interface=http://www.haskell.org/ghc/docs/latest/html/libraries/base,$(LOCAL_GHC_DOCS) \
		$^
