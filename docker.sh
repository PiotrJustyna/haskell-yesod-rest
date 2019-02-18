#!/bin/bash
docker build -t stack-haskell:8.6.3 .
docker run --name stack-haskell -d -p 3000:3000 stack-haskell:8.6.3
# docker stop 6233115cd509ebd02d45ad26a9ca16c6bcf5a8c11ee7d3977ca6e7b7c687bb59
# docker rm 6233115cd509ebd02d45ad26a9ca16c6bcf5a8c11ee7d3977ca6e7b7c687bb59