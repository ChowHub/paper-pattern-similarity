FROM machow/r-onbuild

RUN install2.r --error RCurl

CMD ["make"]
