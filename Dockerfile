FROM ubuntu:latest

ARG BINLOCATION
ENV RESTURL=ptsv2.com

RUN apt update && apt upgrade -y

# Copy over the source code and make it executable.
ADD $BINLOCATION/bin/Filehandler-exe /usr/local/bin/filehandler-exe
RUN chmod +x /usr/local/bin/filehandler-exe

# create group and user, then the working dir and add permissions to it
RUN groupadd -g 999 appuser && useradd -r -u 999 -g appuser appuser && mkdir -p /workdir && chown appuser /workdir
USER appuser

# We're all ready, now just configure our image to run the server on
# launch from the correct working directory.
CMD exec /usr/local/bin/filehandler-exe ${RESTURL} "prod"
WORKDIR /workdir
EXPOSE 5000