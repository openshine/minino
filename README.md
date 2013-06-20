#Minino [![Build Status](https://secure.travis-ci.org/openshine/minino.png)](http://travis-ci.org/openshine/minino)

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
Get the binary.
```sh
$ wget http://github.com/openshine/minino/wiki/minino
$ chmod 744 minino 
```

### Build

Get the source and build it.

```sh
$ git clone git@github.com:openshine/minino.git
$ cd minino
$ make
```

##  Create a Minino Application <a name="createapp"></a>

Create an application directory and copy the minino binary there.
 
```sh
$ mkdir kitty
$ cd kitty
$ cp /path/to/minino/bin/minino .
$ ./minino create-app id=kitty
```
##  Run Minino Server <a name="runserver"></a>

```sh
$ ./minino runserver [Port]
```
Port is an optional argument, it is 8000 by default.

Check minino running at [http://localhost:8000](http://localhost:8000)


##  Examples <a name="examples"></a>
Check some code [examples](./examples)


##  Documentation <a name="doc"></a>
To create the minino documentation type the following command.

```sh
$ make doc
```
Please check doc/index.html



