# tootapult
### _ava fox_

mastodon to twitter crossposter

## Features

- automatic crossposting from mastodon to twitter
- long mastodon posts get turned into twitter threads
- mastodon threads get threaded properly on twitter
- self-retweets when a self-boost is detected
- crossposts pictures and (applicable) media
- configurable privacy levels
- deleting mastodon post deletes the corresponding tweet
- content warning is preserved

## Installation

download binary from release page

copy the example config file and edit it.

at the very least you *need* to set `mastodon-url`

## Usage

`./tootapult -c CONFIG`

loads file from path pointed to in `CONFIG` and starts the crossposter

for more information run `./tootapult --help`

## Building

requires a lisp be installed (preferably [roswell](https://github.com/roswell/roswell) or SBCL)

clone this repo

`git clone https://github.com/compufox/tootapult`

and run make

`make`

unless any errors occur this should generate a binary at `bin/tootapult`

to properly use this with twitter, you'll need to generate an application at [Twitter's Dev Site](https://dev.twitter.com) and provide the `consumer-secret` and `consumer-key` values in your config

## License

NPLv1+
