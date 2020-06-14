# mpv-remote-servant

This program connects to a local instance of MPV and allows users on the local
network to remote-control the player through a web UI

## Installation

- requirements: stack

- clone the repository and navigate into it:
  
  ```
  $ git clone https://github.com/delearyus/mpv-remote-servant.git
  $ cd mpv-remote-servant
  ```

- build and copy binaries
  
  ```
  $ stack build
  $ stack install
  ```

  This will place the compiled binary in `$HOME/.local/bin`, which you may
  need to add to your $PATH.

## Configuration

- TODO: currently configuration is only available through editing the config
  values in the source and recompiling. In the future there will be
  command-line options for (at the very least) socket path and port.

## Usage

- make sure MPV is running first, and listening on the configured socket (by
  default, `/tmp/mpvsock`:
  
  `$ mpv --input-ipc-server=/tmp/mpvsock *.mkv`

- start the mpv-remote-servant server
  
  `$ mpv-remote-servant`

- navigate on a separate device (e.g. a phone) to
  `http://<host>:8383/index.html`
