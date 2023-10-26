# Use an official Go runtime as the base image
FROM golang:1.17

WORKDIR /app

COPY jsonnetfile.json .
COPY generate_json.sh .

RUN go install github.com/google/go-jsonnet/cmd/jsonnet@latest
RUN go install -a github.com/jsonnet-bundler/jsonnet-bundler/cmd/jb@latest
RUN jb install

RUN chmod +x generate_json.sh

ENTRYPOINT ["/app/generate_json.sh"]