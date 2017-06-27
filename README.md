Readme
======

A tool built around [pandoc] to convert a markdown gitit wiki to any other formats. 


Prerequisites
-------------

Mandatory

 -  pandoc
 -  pandoc-types >= 1.16

    Because of binary incompatible change to its interfaces. See the *common errors*
    section below.

    If not met it would break most pandoc filters' compilation.

 -  plantuml

    Required at runtime by `plantuml_filter.hs`.

 -  graphviz

    For the `dot` command required at runtime by `dot_filter.hs`.

Optional (but strongly recommanded)

 -  nix

 -  nixpkgs > revision `551296a`

    This tool was tested against a [nixpkgs repository] at revision `551296a` taken at date
    2016/04/07. If this minimal requirement is met (implying proper nix installation), then
    all other prerequisites will be automatically installed by the `nix-shell` calls documented
    below.
    
Please take a look at the [detailed setup section](#detailed-setup) for per platform steps to
perform as prerequisites.


Running the tool
----------------

For the moment, only *html* output is supported.

~~~
$ git clone https://my/url/to/jrg_wiki_pandoc_converter
$ cd jrg_wiki_pandoc_converter
$ nix-shell
[nix-shell]$ ./run.sh
~~~

You should find the result under `out_html`.

Alternatively, the following last two command can be combined as one:

~~~
$ nix-shell --run ./run.sh
~~~

Note the `nix-shell` invocation. This is not strictly required, but it allows us to use the
[nix package manager] in order to bring into scope all of the required dependencies. If you
do not want to use nix, you can manually install all the required dependencies manually on
your system. Please look at `./shell.nix` for the complete listing.

The script also support optional parameters. Please look directly at `./run.sh` to
see what those are. By default, the *html* output format is selected, `wiki_in` is
cloned inside this repo and the output ends up under `./out_html`. 


Pandoc filters
--------------

The pandoc document conversion tools allows for [custom filters]. Those filters are located
in the `./filters` sub directory. The filter will be automatically compiled (depending on
the used filter language) by the `run.sh` script.

Currently used filters are:

 -  `filters/adapt_local_page_link_filter.hs`: A [pandoc filter] that, depending on output format, 
    transforms link target url without extensions (used as internal page link) to links with
    appropriate path extensions. This is currently useful for the *html* output format so that it
    is possible to navigate the result locally without any needs for a backend server.
    
    The filter also transform absolute local urls into relative path.

    Note that if the documentation is meant to be consulted through a properly configured http
    server, the transformation may optionnally be dropped.

 -  `dot_filter.hs`: A [pandoc filter] that transform graphiz dot code block (with class `.dot`)
    into an image by runing `dot` cli tool.

 -  `plantuml_filter.hs`: A [pandoc filter] that transform plantuml code block (with class 
    `.plantuml`) into an image by runing the `plantuml` cli tool.

TODO
----

 -  `adapt_local_page_link_filter`: Tranform only when output format is *html*, or *html5*.
 -  Filters for all other used gitit plugins: ditaa, diagrams, etc.
 -  Non default, better looking css for the html output?
 -  Support other output format (e.g.: mediawiki, dokuwiki, etc).
 -  Support other wiki urls as input.


Derived tools
-------------

### `sync_wiki_out_html.sh`

This tool will sync a local copy of [wiki_out_html repository] with the latest
[wiki_in repository]'s content.

The local copy is by default placed inside this repository at `./wiki_out_html`.

Note that the user still need to manually add file to git staging area, commit and push.

~~~
./sync_wiki_out_html.sh
~~~


Common errors
-------------

> ERROR: Could not find minimal `1.16` `pandoc-types`  library version required to build `pandoc` 
> filters. Current version  is ``

It means no `pandoc-types` version is installed in the current environement.
You probably forgot to enter the `nix-shell` environement before launching the `run.sh`
command.

> ERROR: Could not find minimal `1.16` `pandoc-types`  library version required to build `pandoc`
> filters. Current version  is `1.15.4`.

It means the currently installed version of the haskell library `pandoc-types` is not recent
enough to meet the requirements of the tool's pandoc filter. If the version was lower than
`1.16` the pandoc filters would not compile. This is because `1.16` introduce binary incompatible
change in interfaces related with the addition of an attributes field to both `Image` and
`Link` constructors. See [pandoc-types - changelog].

Please make sure your [nixpkgs repository] is up-to-date. We tested this tool against a nixpkgs
at revision `551296a` taken at date 2016/04/07. This revision came with a `pandoc-types`
library at version `1.16.1`.



Detailed setup {#detailed-setup}
--------------

### Windows

Here is the minimal setup steps required assuming filters are prebuilt for you platform:

 -  Install [Git for windows](https://git-for-windows.github.io/).

 -  Install [pandoc](http://pandoc.org/installing.html).

 -  Install [graphviz](http://www.graphviz.org/Download_windows.php)

    You should ensure the binaries for this package (e.g: `C:\Program Files (x86)\Graphviz2.38\bin`)
    are added to `PATH`.

 -  Install [jre](https://www.java.com/en/download/).

 -  Install [plantuml](http://plantuml.com/download).

    The environement variable `PLANTUML_JAR` should point on the `*.jar` file.


Additionnally, if the filters are not prebuilt for your platform or you mean to modify them, 
here's the supplementary setup steps to perform:

 -  Install [haskell-platform-full](https://www.haskell.org/downloads).

    Installing the full version is important as it is non trivial to build the network package
    under windows.

 -  From a Cmd shell, install the required haskell packages using cabal:

    ~~~
    cabal update
    cabal install pandoc-types process-streaming pipes-transduce SHA utf8-string hslogger
    ~~~




References
----------

[pandoc]: http://pandoc.org/
[pandoc filter]: http://pandoc.org/scripting.html
[pandoc-types - changelog]: https://github.com/jgm/pandoc-types/blob/master/changelog
[custom filters]: http://pandoc.org/scripting.html
[nix package manager]: https://nixos.org/nix/
[nixpkgs repository]: https://github.com/NixOS/nixpkgs

