# hubic-auth-swift

[hubiC](https://hubic.com/) is a file hosting service that supports accessing the files via the OpenStack [Swift](http://docs.openstack.org/developer/swift/) API. Unfortunately, hubiC does not support the standard Swift authentication mechanism and instead uses one based on OAuth 2.0. Consequently, it is not possible to use most of Swift client applications with hubiC. `hubic-auth-swift` is a simple solution to that problem: it provides a single stand-alone executable that acts as an authentication gateway, enabling applications to access a hubiC account via the standard Swift v1 authentication method.

__DISCLAIMER__: I am not affiliated with hubiC or OVH. This is a tool I developed for my personal use and is not officially supported by hubiC.

### Getting started

1. [Download](https://github.com/redneb/hubic-auth-swift/releases) the executable for your OS (precompiled binaries for Linux, Mac OS X, and Windows are provided).
2. Execute it.
3. Visit [http://localhost:8080/](http://localhost:8080/) with your favorite browser.
4. Follow the instructions.

### Features

* A single stand-alone executable.
* Cross platform (Linux, Mac OS X, Windows, and others).
* Caches authentication tokens.
* Includes an in-browser setup guide.
* Can be used behind a reverse proxy.
* Supports multiple hubiC applications (i.e. apps with different client IDs).

### Usage

    Usage: hubic-auth-swift [OPTION]...

      -p NUM   --port=NUM           port to listen on (default: 8080)
      -a HOST  --address=HOST       address to listen on (default: 127.0.0.1)
      -t NUM   --max-cache-ttl=NUM  maximum time (in minutes) to cache auth tokens (default: 30)
      -u URL   --base-url=URL       the URL that this server is reachable at if reverse proxied
      -h       --help               display this help and exit


### Compiling from source

In order to compile `hubic-auth-swift` you need a haskell development environment. For that, you can use the [haskell platform](https://www.haskell.org/platform/). Alternatively, you can just install `GHC` and `cabal-install`. After you have setup the haskell environment, `hubic-auth-swift` can be compiled as follows:

    cabal update
    cd /path/to/hubic-auth-swift/sources
    cabal sandbox init
    cabal install
    
The resulting binary will be available at `.cabal-sandbox/bin`.

### Similar projects

* [node-hubic-swiftauth](https://github.com/gierschv/node-hubic-swiftauth): a very similar program written in node.js.
* [hubic2swiftgate](https://github.com/oderwat/hubic2swiftgate): a solution written in PHP that can be run in an apache HTTP server.
