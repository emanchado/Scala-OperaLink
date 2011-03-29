all:

org/demiurgo/operalink/LinkAPI.class: scalaj-link.scala
	scalac $<

LinkAPISpec.class: src/test/LinkAPISpec.scala
	scalac $<

test: LinkAPISpec.class org/demiurgo/operalink/LinkAPI.class
	scala org.scalatest.tools.Runner -p . -o -s LinkAPISpec

.PHONY: test
