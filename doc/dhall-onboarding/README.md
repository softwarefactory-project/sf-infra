Get started with dhall
======================

Install the toolchain (this package works with f30):
  https://download.copr.fedorainfracloud.org/results/tdecacqu/dhall/fedora-rawhide-x86_64/01246620-dhall/dhall-1.30.0-1.fc33.x86_64.rpm

Read the `Getting Started` tutorial (skip the install section):
  https://docs.dhall-lang.org/tutorials/Getting-started_Generate-JSON-or-YAML.html


# Syntax cheatsheet

Here is a recap of the operator used in sf-infra:

* // : non-recursively merges record values

```console
$ dhall <<< '{ name = "website" } // { image = "CentOS" }'
{ image = "CentOS", name = "website" }
```

* #  : append list (see below for comments)

```console
$ dhall <<< '[ { name = "website" } ] # [ { name = "backup" } ]'
[ { name = "website" }, { name = "backup" } ]
```

* ++ : append text

```console
$ dhall <<< '"Hello " ++ "Dhall!"'
"Hello Dhall!"
```

* -- : comment   (or {- comment -})

Note: comments are only valid at the begining of a file, or right after a `let`

```dhall
-- Top level comment are ok

let {- this is local comment -} var = 42

in var
```

# Usage

Here are some examples command line usage:

* Run a quick test:

  dhall <<< "let package = ./package.dhall in package.seq 10"

* Evaluate a file:

  dhall --file $path

* Render yaml vars:

  dhall-to-yaml --file playbooks/vars/infra-sf.dhall

* Explain errors using the `--explain` command arguments:

  dhall --explain <<< '[ True, 1 ]'


# FAQ

Each time dhall output does not make sense, it should be documented in this directory.
Look at the other files and grep the error message. If it is not documented, please add a new file.
