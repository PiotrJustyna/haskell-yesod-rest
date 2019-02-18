FROM haskell:8
COPY . /opt/servant-api
WORKDIR /opt/servant-api
RUN stack setup
RUN stack build
CMD ["stack","exec","foa-exe"]
