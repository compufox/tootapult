# tootapult
### _ava fox_

mastodon to twitter crossposter

## Installation

download binary from release page

copy the example config file and edit it with your tokens.

to get twitter tokens you'll need to go to https://dev.twitter.com and sign in/up and go through the process there to generate an app.

(this will hopefully be fixed in a later version)

to get your mastodon token you'll need to:
- go to your instance
- log in
- open your settings
- click 'development' in the sidebar
- create a new application
- enter anything for the name, and then check the box next to the read scope
- scroll down and click create
- click on the name of the newly created application
- copy your access token
- put it in your tootapult config!

(this will hopefully be fixed in a later version)

## Usage

`./tootapult`

if you don't pass it any arguments it will try and load `tootapult.config`

passing an argument will cause tootapult to load a file with that name as a config file

`./tootapult fox.config`

## Building

requires a lisp be installed (preferably [roswell](https://github.com/roswell/roswell) or SBCL)

clone this repo

`git clone https://github.com/compufox/tootapult`

and run make

`make`

unless any errors occur this should generate a binary at `bin/tootapult`

## Todo

- prompt user for mastodon authentication
- prompt user for twitter authentication

## License

NPLv1+

