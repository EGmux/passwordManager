

# How to build

## **Make sure to have Docker installed** and run the following snippet of code in the source code directory.

``` bash
docker build -f build.dockerfile -t passwordmanager .
```

### Expect to take between 1-2min if you have a modern machine

# How to run

``` bash
docker container run -i -p 4040:4040 --rm -t passwordmanager
```

### if everything goes well the WEB interface should be available at localhost:4040

# Gotchas

Currently data is not persistent, so every time the page is refreshed a
new Master Password is generated

Some UI inconsistencies are present, such as showing NIL when instead
nothing should be shown
