# tootapult
### _ava fox_

mastodon to twitter crossposter

## Installation

download binary from release page

copy the example config file and edit it with your tokens.

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

