#Minino

an Erlang miniframework
* [What is Minino?](#about)
* [Authors](#authors)
* [Get Minino](#getminino)
  * [Download](#download)    
  * [Build](#build)    
* [Create a minino application](#createapp)
  

## What is Minino? <a name="about"></a>

An Erlang miniframework

## Authors <a name="authors"></a>

This is an [Openshine](http://www.openshine.com) project developed by:
  * Pablo Vieytes

##  Get Minino  <a name="getminino"></a>
### Download <a name="download"></a>
Not available yet.

### Build

Get the source and build it.

```sh
$ git clone git@github.com:pvieytes/minino.git
$ cd minino
$ ./rebar get-deps compile
$ ./bootstrap
```

##  Create a Minino Application <a name="createapp"></a>


```sh
$ mkdir kitty
$ cd kitty
$ cp /path/to/minino/bin/minino .
$ ./minino create-app id=kitty
$ ./rebar get-deps compile
$ ./minino start
```








