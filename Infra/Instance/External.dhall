let Server = ../Server/package.dhall

in  { skip_os_server_task = True, server = Server::{ image = "unknown" } }
