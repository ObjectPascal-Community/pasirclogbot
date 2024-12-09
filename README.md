# Pascal IRC Log Bot
[![Build Status](https://github.com/ObjectPascal-Community/pasirclogbot/actions/workflows/main.yaml/badge.svg?branch=main)](https://github.com/ObjectPascal-Community/pasirclogbot/actions)
[![Supports Windows](https://img.shields.io/badge/support-Windows-blue?logo=Windows)](https://github.com/ObjectPascal-Community/pasirclogbot/releases/latest)
[![Supports Linux](https://img.shields.io/badge/support-Linux-yellow?logo=Linux)](https://github.com/ObjectPascal-Community/pasirclogbot/releases/latest)
[![Supports macOS](https://img.shields.io/badge/support-macOS-black?logo=macOS)](https://github.com/ObjectPascal-Community/pasirclogbot/releases/latest)
[![License](https://img.shields.io/github/license/ObjectPascal-Community/pasirclogbot)](https://github.com/ObjectPascal-Community/pasirclogbot/blob/main/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/ObjectPascal-Community/pasirclogbot?label=latest%20release)](https://github.com/ObjectPascal-Community/pasirclogbot/releases/latest)
[![Downloads](https://img.shields.io/github/downloads/ObjectPascal-Community/pasirclogbot/total)](https://github.com/ObjectPascal-Community/pasirclogbot/releases)

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

## Usage

```console
$ paslogbot -h
Usage:
  paslogbot [PARAMS]

PARAMS:
    -h/--help         This help message.
    -c/--config=FILE  Use provided FILE as config. ( default: ~/.pasirclogbot )
    -d/--debug        Turn debug On. (default: Off)
```

## About the author

I'm usually on `irc.libera.chat` on the `#objectpascal`, `#lazarus` and `#pascal` channels. On `IRC` I use the nick name `[Batch]`.