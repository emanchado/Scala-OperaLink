DEPRECATED
==========

This library is deprecated. It probably doesn't work anymore on more recent versions of Scala anyway.












Opera Link client library for Scala
===================================

This is (a very early version of) a Scala client library for the
[Opera Link API](http://www.opera.com/docs/apis/linkrest/) (see also
"[Introducing the Opera Link API](http://dev.opera.com/articles/view/introducing-the-opera-link-api/)"
and
"[Building your first Link API application](http://dev.opera.com/articles/view/building-your-first-link-api-application/)").

I'm still learning Scala, so probably the APIs feel a bit weird, or I
use strange naming conventions or whatever. Bear with me, and let me
know about ways to make the code more idiomatic.

There's no API documentation yet, but the source is so tiny that I'm
sure you can figure it out just by reading it (you can start with the
silly example at `src/main/scala/org/demiurgo/operalink/Test.scala`).

I'm using [sbt](http://code.google.com/p/simple-build-tool/) to
compile it and run the tests, and you'll need these libraries to compile it:

* [Apache Commons Codec](http://commons.apache.org/codec/)
* [scalaj-http](https://github.com/scalaj/scalaj-http)
* [ScalaTest](http://www.scalatest.org/) (this is only for running the tests, obviously)

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=emanchado&url=https://github.com/emanchado/Scala-OperaLink&title=Scala Opera Link API client library&language=en_GB&tags=github&category=software)
