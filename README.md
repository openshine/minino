#Minino

* [What is Minino?](#about)
* [Authors](#authors)
* [Get Minino](#getminino)
  * [Download](#download) 
  * [Build](#build)
* [Create a Minino Application](#createapp)
* [Run Minino Server](#runserver)
* [Examples](#examples)
* [Documentation](#doc)


## What is Minino? <a name="about"></a>

An Erlang web miniframework.

## Authors <a name="authors"></a>

This is an [Openshine](http://www.openshine.com) project developed by:
  * Pablo Vieytes

##  Get Minino  <a name="getminino"></a>
### Download <a name="download"></a>
Not available yet.

### Build

Get the source and build it.

```sh
$ git clone git@github.com:openshine/minino.git
$ cd minino
$ make
```

##  Create a Minino Application <a name="createapp"></a>

```sh
$ mkdir kitty
$ cd kitty
$ cp /path/to/minino/bin/minino .
$ ./minino create-app id=kitty
$ ./rebar get-deps compile
```
##  Run Minino Server <a name="runserver"></a>

```sh
$ ./minino runserver [Port]
```
Port is an optional argument, it is 8000 by default.

Check minino running at [http://localhost:8000](http://localhost:8000)


##  Examples <a name="examples"></a>
Check the examples [examples](./examples)


##  Documentation <a name="doc"></a>
To create the minino documentation type the following command.

```sh
$ make doc
```
Please check doc/index.html



