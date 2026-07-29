#!/usr/bin/env Rscript
# vellumplot MCP server launcher.
#
# A thin entry point: load the package and run the stdio JSON-RPC loop. All logic
# lives in the package (vellumplot::mcp_serve); this file only exists so an MCP
# client can spawn the server with a stable path:
#
#   Rscript "$(Rscript -e 'cat(system.file("mcp/server.R", package="vellumplot"))')"
#
# See inst/mcp/README.md for client configuration.

suppressMessages(library(vellumplot))
vellumplot::mcp_serve()
