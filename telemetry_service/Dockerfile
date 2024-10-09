FROM erlang:alpine

RUN mkdir /telemetry_service
WORKDIR /telemetry_service

COPY . .

RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /telemetry_service/_build/prod/rel/telemetry_service /service

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/service/bin/telemetry_service", "foreground"]
