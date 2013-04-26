[![Build Status](https://travis-ci.org/etorreborre/specs2.png?branch=master)](https://travis-ci.org/etorreborre/specs2)

Installation instructions
=========================

You need to download and install sbt (simple build tool)
Then execute the following command:

        > sbt update publish-local

Then you can generate the User Guide with:

        sbt
        > test-only org.specs2.guide.UserGuide -- html

This should create html files in the target/specs2-reports directory. 
