# sigrid

## About

A help queue web app for lab sessions and tutorials named after the doll [Sigrid](https://www.youtube.com/watch?v=cc-TAuKWdTI) in  "Beppes godnattstund".

## About the v1.x branch

Sigrid v1.x will be legacy when v2.x is production ready. For more information see Releases.

## How to use a running sigrid server

When sigrid is running it responds to the following requests:

* `/sigrid` provides a login page for students. No password is required. Each user will get a unique user name. When a student have logged in, these states are selectable:
  - `Köar inte` - not standing in any queue; used when working or getting help.
  - `Hjäälp!!!` - when standing in the help queue.
  - `Fäärdiig!` - when standing in the approval queue.
  - `Loggar ut` - when being approved and the leaving the room.

* After selecting a new state the user must press `Uppdatera` to make the state change take effect.

* `/sigrid/monitor` provides a list of all rooms and all their queues, in alphabetical order based on course code and then room name. This page reloads automatically at a regular intervall. 

* `/beppe` provides a login page for supervisors. No password is required. Each user will get a unique user name. When you have logged in you can choose between these actions:
  - `Jobbar` when supervisor is working.
  - `Töm hjälpkö!` will empty the help queue
  - `Töm redovkö!` will empty the approval queue
  - `Hej då!` the supervisor will leave the room, but the room will continue to exist.
  - `Radera!` the room will be inactivated. 

## How to build and start sigrid

* Install `git` and `sbt` if needed

* `git clone` this repo and `cd` into the `sigrid` folder

* start `sbt`

* type `run`

* Your server is now running at `localhost:8091` open to the world if that port is forwarded in your router.



