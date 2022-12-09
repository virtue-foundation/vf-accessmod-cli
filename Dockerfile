FROM ghcr.io/virtue-foundation/vf-accessmod-cli_env:main

COPY src/ /workspaces/vf-accessmod-cli/src/
WORKDIR /workspaces/vf-accessmod-cli/src

RUN mkdir "/geodata"

#ENV FLASK_APP="/workspaces/vf-accessmod-cli/src:app"

ENTRYPOINT flask run --host=0.0.0.0
