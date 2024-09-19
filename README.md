# Sigrid

## About

A help queue web app for lab sessions and tutorials named after the doll [Sigrid](https://www.youtube.com/watch?v=cc-TAuKWdTI) in  "Beppes godnattstund".

## About the 2.x branch

This branch is a rewrite of Sigrid from scratch with minimal complexity lean Scala using 
* Scala >= 3.5.0
* OpenJDK >= 21
* [Scala CLI](https://scala-cli.virtuslab.org/) as build tool
* [cask](https://github.com/com-lihaoyi/cask) instead of akka
* [storky](https://github.com/bjornregnell/storky) as the in-memory, thread-safe key-value-store

# How to use a running Sigrid server

TODO

# How to run Sigrid as a server

* You need at least java 21 installed.

* Download the latest assembly jar from Releases and start with `java -jar nameofjar.jar` and the server is now running on localhost:????

# How to build and start Sigrid from code 

* Clone this repo

* Run with `scala run .`

* Your server is now running at `localhost:8091` open to the world if that port is forwarded in your router.

* Package with `scala --power package . -o sigrid-assembly-2.x.y.jar --assembly`


