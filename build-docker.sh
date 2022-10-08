#!/usr/bin/env sh



BINLOCATION=$(stack path --local-install-root)
BINLOCATION=$(realpath --relative-to=. "$BINLOCATION")
docker build -t filefighter/filehandler:feature . --build-arg BINLOCATION="$BINLOCATION"
