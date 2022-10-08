FROM debian:testing-slim

ARG BINLOCATION
ENV FILESYSTEMSERVICE_URL=fss
ENV FILESYSTEMSERVICE_PORT=8080
ENV APP_PROFILE=prod
ENV ENCRYPTION_PASSWORD=null
ENV DB_USERNAME=filehandler
ENV DB_PASSWORD=changeThis
ENV DB_CONTAINER_NAME=db
ENV DB_NAME=filehandler

# Copy over the source code and make it executable.
ADD $BINLOCATION/bin/FileHandlerYesod /usr/local/bin/filehandler-exe
RUN chmod +x /usr/local/bin/filehandler-exe

# TODO: because we want to write to a host directory we must run as root, or change the permissions of the directory
# create group and user, then the working dir and add permissions to it
#RUN groupadd -g 999 appuser && useradd -r -u 999 -g appuser appuser && mkdir -p /workdir && chown appuser /workdir
#USER appuser

# We're all ready, now just configure our image to run the server on
# launch from the correct working directory.
# using exec solves ctl + c issues
CMD exec /usr/local/bin/filehandler-exe
WORKDIR /workdir
EXPOSE 5000
