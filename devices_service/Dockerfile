FROM erlang:alpine

RUN mkdir /service
WORKDIR /service

COPY . .

CMD ["rebar3", "shell"]
