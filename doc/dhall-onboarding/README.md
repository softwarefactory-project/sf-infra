# Get started with dhall

Install the toolchain (this package works with f30):
  https://copr-be.cloud.fedoraproject.org/results/tdecacqu/dhall/fedora-rawhide-x86_64/01192146-dhall/dhall-1.29.0-1.fc32.x86_64.rpm

Read the `Getting Started` tutorial (skip the install section):
  https://docs.dhall-lang.org/tutorials/Getting-started_Generate-JSON-or-YAML.html

# Usage

In sf-infra, we use:

* To run a quick test:

  dhall <<< "let package = ./package.dhall in package.seq 10"

* To evaluate a file:

  dhall --file $path

* To render playbooks vars:

  dhall-to-yaml --file playbooks/vars/infra-sf.dhall

* Get the list of server:

   dhall text <<< 'let Infra = ./package.dhall in Infra.Prelude.Text.concatSep "\n" (Infra.mapServerText (\(s : Infra.Server.Type) -> s.name) Infra.servers)'

Interesting dhall command arguments:

  --explain  : add documentation when evaluation fails



# FAQ

Each time dhall output does not make sense, it should be documented in this directory.
Look at the other files and grep the error message. If it is not documented, please add a new file.
