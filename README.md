<h1 align="center"><img src="static/logo.svg" width=90px> Capybara </h1>
<h3 align="center"> interactive graph reduction for lambda calculus </h3>
<br>

[Capybara](https://yang-zhu.github.io/capybara) is a single-page web application that allows interactive graph reduction of lambda expressions. It is designed to make the understanding and comparison of different evaluation strategies (Call-By-Name, Call-By-Value and Call-By-Need) easier.

The implementation is completely written in [Haskell](https://www.haskell.org/). The frontend is programmed using [miso](https://haskell-miso.org/). [Bootstrap](https://getbootstrap.com/) is used to style the interface. The graphs are displayed using the Scalable Vector Graphics (SVG) format.

This is a project for the lecture [Fortgeschrittene Funktionale Programmierung](https://www.tcs.ifi.lmu.de/lehre/ws-2021-22/fun) at LMU in WS21/22.

## What features does Capybara have?

The interface is rather self-explanatory (hopefully XD), but I will still list the features here.

The user can type a lambda expression in the input box and choose an evaluation strategy. For the lambda symbol, you can simply type a backslash, it will be automatically turned into λ when evaluating.

Variable names can be letters except for λ, digits and underscores. But they must start with a letter.

Supercombinators without parameters are also supported. Some supercombinators defining Church encodings are already provided. You can add your own ones of course.

When there are errors in your term or supercombinator definitions, the corresponding box will light up and an error message is displayed.

Bound variables will be renamed by attaching subscripts to them.

When graphs are displayed, you can click on the ◄ and ► buttons or press left and right arrow keys to go through them. The reducible expression of every beta-reduction step is marked in blue. Unfolding the definition of a supercombinator is considered one step of reduction.

## What if I want to build Capybara locally?

No problem if you have a Linux System! (Unfortunately there is currently a [bug](https://github.com/dmjio/miso/issues/639) in miso, which makes building on macOS impossible.)

You first need to install [`Nix`](https://nixos.org/) on your machine.
```
$ sh <(curl -L https://nixos.org/nix/install) --no-daemon
```
Next step is to install [`Cachix`](https://www.cachix.org/) to make the build process faster.
```
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use miso-haskell
```
I assume you have already cloned this repository. Then just go to the directory. I have already prepared a [`Makefile`](Makefile) for you. A simple `make` should do the trick.

Now we are all set! Simply open `result-webpage/index.html` with your favourite browser, Capybara will take it from there :)

## Why do you name it Capybara?

Because I love them! They are the largest rodents in the world. But what really makes them stand out is how chilled they are. If you have never heard of this animal, check out this [video](https://www.youtube.com/watch?v=CdMUOsf2QNc). Don't they brighten you up?

I hope my little application could also bring you some joy and make you feel at ease with this fairly abstract topic. Have fun!