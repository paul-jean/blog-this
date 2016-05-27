blog-this
=========

WordPress blogging client for Mathematica

usage
=====

Open the file test-run.nb in Mathematica.  You'll see some Mathematica code like this:

```mathematica
    PrependTo[$Path, NotebookDirectory[]];

    << BlogClient`

    nbfile = FileNameJoin[{NotebookDirectory[], "example-notebook.nb"}];

    PostNotebook[nbfile, "SiteAddress" -> "WP_SITE_ADDRESS", 
     "Port" -> ""(* usually *), "XMLRPCServerPath" -> "xmlrpc.php", 
      "Title" -> "Mapping random walks to a closed path", 
       "Username" -> "YOUR_WP_USERNAME", "Password" -> "YOUR_WP_PASSWORD", 
        "Categories" -> {}, "Debug" -> True]
```

Fill in your user name and password in the "Username" and "Password" options.  Also set your blog post title in the "Title" option.
You can also add categories using the "Categories" option.

You may also need to change the WordPress path and port number options, depending on how you set up your WordPress installation.

Now evaluate those cells.  If the "Debug" option is set True, you'll see a bunch of log messages showing the progress of the HTML and image export, like this:

![example export][images/export-output.png "Export output"]

more info
=========

See my [blog post](http://paul-jean.github.io/2011/02/11/weblog-client.html) describing the XML-RPC encoding/decoding.


