# Pascal IRC Log Bot

> [!Important]
> Since I was lazy enough to not bother with the boilerplate, I asked some `AI` bots to help me.\
> You can find the collected answers on the [research](research) folder.

An attempt at producing an `IRC` bot that logs a channel's content.

## Preamble

In terms of internet protocols we can choose between `Indy` and `Synapse` as the libraries available. I decided that I was going to use `Indy` due to the fact that it already has an `IRC` client and I didn't want to do all the necessary string parsing for the `IRC` protocol.

## Features

The initial idea is to have all the logging of the `IRC` channel dumped into a `SQLite` database. To query the database, the objective is twofold:

1. Via commands on `IRC`
2. Via a web site that the bot, itself, serves

Not sure if it's worth having the bot itself serve the website, but due to the fact that `SQLite` will be locked during the bot's operation, then this will have to be the path, for the time being.

# Usage

```console
$ paslogbot -h
Usage:
  paslogbot [PARAMS]

PARAMS:
    -h/--help         This help message.
    -c/--config=FILE  Use provided FILE as config. ( default: ~/.pasirclogbot )
```