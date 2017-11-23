# sqlite2go

Command sqlite2go ports SQLite to Go (Work In Progress)

### Installation

To install or update sqlite2go

    $ go get [-u] github.com/cznic/sqlite2go

### Online documentation

GoDoc: [godoc.org/github.com/cznic/sqlite2go](https://godoc.org/github.com/cznic/sqlite2go)

### Usage

To generate a Go SQLite package:

    $ sqlite2go [options]

To generate a standalone Go SQLite shell:

    $ sqlite2go -shell [options]

### Options

    -D<name>
    -D<name>=<value>

Define preprocessor macros. The first form is equivalent of -D<name>=1.

    -h

Print usage to standard error and exit with status 2.

    -o <path>

Write the result to <path>. Defaults to standard output.

    -package <name>

Name the generated package. Ignored when -shell is used.

    -shell

Generate the standalone SQLite shell program in package main.
