# Sigrid

## About

A help queue web app for lab sessions and tutorials named after the doll [Sigrid](https://www.youtube.com/watch?v=cc-TAuKWdTI) in  "Beppes godnattstund".

### About the 2.x branch

This branch is a rewrite of Sigrid from scratch with minimal complexity lean Scala using
* Scala 3.3.6 (Current LTS)
* OpenJDK >= 21
* [sbt](https://www.scala-sbt.org/) as build tool (multi-project setup)
* [Scala.js](https://www.scala-js.org/) for client-side development
* [cask](https://github.com/com-lihaoyi/cask) web framework for the server
* [Laminar](https://laminar.dev/) for the Scala.js client

## Project Structure

The project uses an sbt multi-project setup with three core subdirectories:

* **`common/`** - Shared code that runs on both JVM (server) and JavaScript (client)
* **`server/`** - Scala/JVM backend using the cask web framework
* **`client/`** - Scala.js frontend using Laminar for reactive UI

## Development

To get the development environment running:

1. **Prerequisites**: You need at least Java 21 installed.

2. **Clone and setup**:
   ```bash
   git clone <repo-url>
   cd sigrid
   git checkout v2.x
   ```

3. **Start sbt**:
   ```bash
   sbt
   ```

4. **Start the development servers**: In the sbt shell, run these commands in sequence:

   First, start the server:
   ```
   server/run
   ```
   This starts the server at `localhost:8080`. Keep this running.

   Then, in the same sbt shell, compile the client:
   ```
   client/fastLinkJS
   ```
   This compiles the Scala.js client code.

5. **View the application**: Open `client/index-dev.html` in your browser to see the development version.

## Examples

* **Test the server API**:
  * `curl http://localhost:8080/ping` - health check endpoint

* **Development workflow**: Keep `server/run` and `client/fastLinkJS` running, then refresh `index-dev.html` as you make changes.

