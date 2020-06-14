# mpv-remote-servant

This program connects to a local instance of MPV and allows users on the local
network to remote-control the player through a web UI

## Instructions

- make sure MPV is running first, and listening on the configured socket (by
  default, `/tmp/mpvsock`:
  
  `$ mpv --input-ipc-server=/tmp/mpvsock *.mkv`

- start the mpv-remote-servant server
  
  `$ mpv-remote-servant`

- navigate on a separate device (e.g. a phone) to
  `http://<host>:8383/index.html`
