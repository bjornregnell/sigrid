# Sigrid

## About

A help queue web app for lab sessions and tutorials named after the doll [Sigrid](https://www.youtube.com/watch?v=cc-TAuKWdTI) in  "Beppes godnattstund".

### About the 2.x branch

This branch is a rewrite of Sigrid from scratch using:
- [Scala](https://www.scala-lang.org/), version 3.7.3
- [OpenJDK](https://openjdk.org/), version 21
- [SBT](https://www.scala-sbt.org/) as build tool (multi-project setup)
- [Cask](https://github.com/com-lihaoyi/cask) web framework for the server
- [Scala.js](https://www.scala-js.org/) for client-side development
- [Vite](https://vite.dev/) for client development server and bundling
- [Laminar](https://laminar.dev/) for the Scala.js client

## Project Structure

The project uses an sbt multi-project setup with three core subdirectories:

- **`common/`** - Shared code that runs on both JVM (server) and JavaScript (client)
- **`server/`** - Scala/JVM backend using the cask web framework
- **`client/`** - Scala.js frontend using Laminar for reactive UI

## Development

### Prerequisites

To get the development environment running you'll need to first install the following tools:

- [SBT](https://www.scala-sbt.org/), a simple Scala build tool that manages this project
- [Node.js](https://nodejs.org/en), a JavaScript runtime environment (for developing and building the client)

Some optional but recommended tools:

- [Scala Metals extension](https://open-vsx.org/vscode/item?itemName=scalameta.metals), a language server with some nice functionality (if using Visual Studio Code)

### Set up

1. **Clone and setup**:
   ```bash
   git clone <repo-url>
   cd sigrid
   git checkout v2.x
   ```

2. **Start sbt**:
   ```bash
   sbt
   ```

3. **Start the SBT services**: In the sbt shell, run these commands in sequence:

   First, start the server:
   ```
   server/run
   ```
   This starts the server at `localhost:8080`. Keep this running.

   Then, in the same sbt shell, compile the client:
   ```
   ~client/fastLinkJS
   ```
   This compiles the Scala.js client code and re-compiles it when you make changes.

4. **Start Vite**: 

   In another terminal, go to the `client` directory. Install Node.js dependencies with:
   ```
   npm install
   ```

   Then, run the client development server with:
   ```
   npm run dev
   ```

The browser app is now available at `http://localhost:5173`! 

## How to build

SBT does most of the heavy lifting for you, but Vite provides additional goodies as for building the client. After having completed the setup, simply go to the `client` subdir and run:
```
npm run build
```
Vite will first make SBT compile the source code to optimized JavaScript, then it will bundle it together with a single generated HTML file, a single minified JavaScript file, and a single minified CSS file. The bundle can be found in `client/dist`. One advantage here is that only the bundle is ever going to be needed to host the client anywhere.

