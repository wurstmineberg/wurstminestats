## This is wurstminedata

It started out as [wurstminestats](https://github.com/wurstmineberg/wurstminestats), which pretty much only was a bunch or R scripts to collect [Wurstmineberg](http://wurstmineberg.de) [API](http://api.wurstmineberg.de) data, combined it with some descriptive elements from our [assets](http://assets.wurstmineberg.de) and generated a bunch of [plots](http://i.wurstmineberg.de/wurstminestats/) for your amusement.  

But primarily, it's my first larger(ish) project in R. Consequently I discovered the possibilities that [shiny](http://shiny.rstudio.com/gallery/) offered and wanted to try something to the likes of that. So now there's wurstminedata, currently living in the wurstminestats repository since it mooches off of a lot of it. Technically I should probably just give this a separate repository and use wurstminestats as a git submodule or something, but idunno, I'll see how it goes.

### Status

This is in early development, and I mean *early*. I just figured out how the basics of shiny work and combined with my lack of basic webdesign skills, there's a bunch of stuff left to figure out. Besides that, there's the whole under-the-hood stuff, soo… Yeah. It'll take me a while until this is something actually usefull. But in the end, I'd like it to completely replace the clusterfuck of .png glory that is wurstminestats/Plots.
