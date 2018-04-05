## Install
The application was written in Haskell using Snap framework and uses Stack tool, to run the server we need to setup the environment for Haskell. [Here](https://wiki.haskell.org/Haskell_in_5_steps) you can find instructions on how to install it on your platform.

Once Haskell tools are installed we have to download the sources at [https://github.com/raynerd/adserver.git](https://github.com/raynerd/adserver.git), build and run the server.

```
#git clone https://github.com/raynerd/adserver.git
#cd adserver
#stack setup
#stack build --exec adserver
```

The application will use the `8000` port by default but if you need to start it on a custom one you can run this way:

```
#stack build --exec 'adserver --port xxxx'
``` 
Now that the server is running you can make GET requests using any browser or the provided testing page under `/testing` route. The URL sould be something like:  `http://localhost:8000/api/3?country=Germany&language=German&interest=Baseball&interest=Albums`

Here you can see following the `/api` route a 3 for the channel id, then all remaining request parameters. If you use the testing page you will be able to create and send requests and visualize results in the same view.

> Note there is a small set of testing data on `FakeData.hs` used to test functionalities, if you need some more examples you can reach me.