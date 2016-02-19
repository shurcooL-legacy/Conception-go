# Conception-go [![Build Status](https://travis-ci.org/shurcooL/Conception-go.svg?branch=master)](https://travis-ci.org/shurcooL/Conception-go) [![GoDoc](https://godoc.org/github.com/shurcooL/Conception-go?status.svg)](https://godoc.org/github.com/shurcooL/Conception-go)

This is a work in progress Go implementation of [Conception](https://github.com/shurcooL/Conception#demonstration).

Conception is an experimental project. It's a platform for researching software development tools and techniques. It is driven by a set of guiding principles. Conception-go targets Go development.

My "short term" goal is to implement some of the ideas shown off in Bret Victor's [Learnable Programming](http://worrydream.com/LearnableProgramming/) article. Another goal is to try to beat my current Sublime Text + GoSublime development environment setup (which sets a really high bar).

Watch this repo to follow the project's development.

Screenshot
----------

A screenshot showing off a single aspect of the project that was recently added or improved.

![Early 2015 Overview](http://dmitri.shuralyov.com/projects/Conception/images/Go/early-2015-overview.png)

Installation
------------

### OS X

```bash
# Install latest Go, git (if you don't already have them).
...

# Step back and enjoy 1 command that installs Conception-go and all its dependencies.
go get -u github.com/shurcooL/Conception-go

# Run it.
$GOPATH/bin/Conception-go
```

### Linux

```bash
# Install latest Go (if you don't already have it).
sudo apt-get install --yes curl
curl -L https://golang.org/dl/go1.6.linux-amd64.tar.gz | sudo tar zx -C /usr/local/
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/MyGoWorkspace # Set your GOPATH as described at https://golang.org/doc/code.html#GOPATH (if you didn't already).

# Install git, OpenGL headers (if you don't already have them).
sudo apt-get install --yes git
sudo apt-get install --yes libgl1-mesa-dev xorg-dev # OpenGL headers.

# Step back and enjoy 1 command that installs Conception-go and all its dependencies.
go get -u github.com/shurcooL/Conception-go

# Run it.
$GOPATH/bin/Conception-go
```

License
-------

-	[MIT License](http://opensource.org/licenses/mit-license.php)
