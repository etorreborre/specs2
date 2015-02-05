[![Build Status](https://travis-ci.org/etorreborre/specs2.png?branch=master)](https://travis-ci.org/etorreborre/specs2)

Installation instructions
=========================

[![Join the chat at https://gitter.im/etorreborre/specs2](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/etorreborre/specs2?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

You need to download and install sbt. Then execute the following command:

    sbt update publishLocal

Then you can generate the User Guide with:

    sbt testOnly org.specs2.guide.UserGuide -- html

This should create html files in the `target/specs2-reports` directory. 
