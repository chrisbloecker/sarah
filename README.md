# Smart Augmented Reality Automated Home

SARAH is my approach at a smart home system. But why another smart home system, are there not enough out there already? Yes, there are click-together things out there and probably everything you need to make a home smart can be plugged together. Plus, Amazon, Google, Microsoft (and probably others) are releasing their assistants you can talk to and, e.g. ask to turn on the lights or read the news to you. But this will always require compatible hardware, and I'd like to make the hardware smart that I already have. Besides, the main point in this is that I want to build my smart home myself. At the moment, this project is still very much a work in progress.

## Components
SARAH consists of different components, each of which is responsible for specific tasks, providing different services.

### Frontend
The frontend is written in [elm](http://elm-lang.org/) and can be used to control the smart home system through a web interface. The frontend interacts with the middleware through a REST interface that is provided by the middleware.

### Middleware
The middleware is written in [Haskell](https://www.haskell.org/) using [Servant](http://haskell-servant.readthedocs.io/) for providing a REST interface and [Cloud Haskell](http://haskell-distributed.github.io/) for distribution. The middleware interacts with the persist backend through a REST interface that is provided by the backend.

There is one master node in the middleware that is responsible for talking to the persist backend and for taking input from the frontend through the provided REST interface. On every device that is part of the smart home system, there is one slave node running that connects to and communicated with the master node.

### Persist Backend
The persist backend is written in [Haskell](https://www.haskell.org/) using [Servant](http://haskell-servant.readthedocs.io/) for providing a REST interface and [Persistent](http://www.yesodweb.com/book/persistent) for interaction with a database backend. At the moment, the database that is used is a MySQL database, but it could be really anything that Persistent can talk to.

### Hardware
The Hardware I use is multiple Raspberry Pi 3 and some prototyping hardware such as sensors and IR-LEDs to interact with, e.g., TV and AC.
