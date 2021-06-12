There really is nothing quite like Markdown. I've certainly never seen anything
quite so elegant or proud or beautiful. Here are some great reasons to love
Markdown:

1. You get automatic numbering.
3. I don't have to number _in order_.
4. I'll come up with another eventually.

## Code

Here's some code, just as an example:

```
loadMarkdown :: FilePath -> IO Pandoc
loadMarkdown filepath = do
    fileContents <- Text.IO.readFile filepath
    Pandoc.runIOorExplode (Pandoc.readMarkdown Pandoc.def fileContents)
```
