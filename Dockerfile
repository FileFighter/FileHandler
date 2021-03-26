FROM ubuntu:latest

ARG BINLOCATION
ENV RESTURL=ptsv2.com

# Copy over the source code and make it executable.
ADD $BINLOCATION/bin/Filehandler-exe /usr/local/bin/filehandler-exe
RUN chmod +x /usr/local/bin/filehandler-exe


# We're all ready, now just configure our image to run the server on
# launch from the correct working directory.
CMD /usr/local/bin/filehandler-exe ${RESTURL} "prod" > /dev/stdout
WORKDIR /workdir
EXPOSE 5000