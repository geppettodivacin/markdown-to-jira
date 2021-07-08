Markdown To Jira
================

As someone who uses Jira a lot, there have been a few things that have annoyed
me about the process of creating a series of tasks:

* Once I start task creation, I have to finish that one before working on the
  next.
* Once I finish a task, I can't go back to earlier ones to add new information
  if I remember something later.
* It's hard to double check links between new issues, such as blocking tasks,
  since they're not all on the same screen.
* Personal nitpicks: I can't use my own text editor, and I have to use Jira's
  awkward one-off markup language that's only used in Atlassian products.

This tool is provides a simple workflow to add a batch of Jira issues. The
batch is written as a single Pandoc-flavored Markdown file with a header for
each new issue. The description and the issue metadata (issue type, priority,
epic, links, etc.) go under the headers, and all Markdown gets converted to the
equivalent Jira markup. The output is a CSV file, ready to be imported by
Jira's CSV Import tool.

For more information about Pandoc-flavored Markdown, see [this
page][pandoc-markdown].

For more information about Jira CSV Import, see [this page][jira-csv-import].

[pandoc-markdown]: https://pandoc.org/MANUAL.html#pandocs-markdown (Pandoc Markdown)

[jira-csv-import]: https://support.atlassian.com/jira-cloud-administration/docs/import-data-from-a-csv-file/ (Import data from a CSV file)

Help
----

```
Usage: markdown-to-jira INPUT [-o|--output OUTPUT]
  Converts input markdown to a Jira-consumable CSV file

Available options:
  INPUT                    Path to input markdown file to convert
  -o,--output OUTPUT       Path to output csv file path (default: stdout)
  -h,--help                Show this help text
```

Examples
--------

### Simple Issue
Every level 1 header becomes its own task.

```markdown
Create a simple issue
====================

Finish explaining issue to users
================================
```

### Full markdown descriptions
Everything that comes beneath the level 1 header becomes the issue description.

    # Create a task with a long description

    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
    incididunt ut labore et dolore magna aliqua. Arcu ac tortor dignissim convallis
    aenean et. Quam adipiscing vitae proin sagittis. Lacus viverra vitae congue eu
    consequat ac.

    Aliquet enim tortor at auctor urna. Etiam sit amet nisl purus in.
    Mattis enim ut tellus elementum sagittis vitae et. Eu mi bibendum neque egestas
    congue. Laoreet non curabitur gravida arcu ac tortor dignissim. Ac turpis
    egestas sed tempus urna et pharetra pharetra.

    1. Mauris vitae ultricies leo integer malesuada nunc. Mi ipsum faucibus vitae
       aliquet nec ullamcorper sit amet risus.
    2. Consequat mauris nunc congue nisi vitae suscipit tellus. Elementum nibh
       tellus molestie nunc non blandit massa enim nec.

    ```
    void foo ()
    {
        bar ();
    }
    ```

### Jira Metadata
Jira metadata is represented by the Pandoc Markdown extension definition lists.
These definition lists can be listed anywhere after the header, and will not be
included in the description. As definition lists, any metadata field that can
take multiple values can have multiple entries in the list, without
re-specifying the metadata key.

```markdown
A bug with high priority, linked to an existing task
====================================================

Issue Type
: Bug

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. 

Priority
: High

Relates To
: TASK-123
```

See Pandoc's documentation on [definition lists][pandoc-definition] for more
information on that. See section below for more info about
[issue links](#issue-links).

[pandoc-definition]: https://pandoc.org/MANUAL.html#definition-lists (Definition lists)

Importing the generated CSV file
--------------------------------

The `markdown-to-jira` tool creates a CSV file that is suitable for importing
into Jira. To do this import:

1. Select `Issues->Import CSV` from the main navigation bar.
2. You will be prompted for a CSV file and, optionally, a template file. (You
   can create the template from the last step of this process, to save the
   settings from the import wizard.)
3. You will be prompted to input general settings. All these can remain the
   same, except make sure your Project Name is correct.
4. You will be prompted to map each of the columns to a value in Jira. This
   mapping is intended for mapping values that do not exactly conform to Jira's
   format, and is useful for [issue links](#issue-links). DO NOT map the
   Description, or you will lose the description formatting.
5. Click Import. You will be given the option here to save a template file
   (Keeps the project and all the mappings you made, along with any other
   settings you changed in the wizard) and / or to view the created tasks.
   It is recommended that you verify that the task import worked as expected.

Known working markdown extensions
---------------------------------

Pandoc made several additions to Markdown in order to make it easier to more
semantically convert from Markdown to various formats other and back. These
are all enabled by default in Pandoc Markdown. Since Pandoc does all the real
format conversion here, I'm at its mercy to do this correctly. Those tested to
work are listed here, others should be used at your own risk. (And then if they
work, let me know!)

* `fenced_code_blocks`: Works for both triple tilde `~` and triple backtick
  `` ` ``. Note that the language specification is finnicky: for example, "C++"
  works, but "c++" does not (defaulting to java).

      # Task with a code block

      See the C++ code below:

      ~~~C++
      class foo_t
      {
          void foo ();
      };
      ~~~

* `line_blocks`: Works correctly

      # Task with a poem

      | I wrote a haiku
      | To show these lines will break right
      | In one paragraph.

* `fancy_lists`: Converts all numeral types (whether Roman numerals, alphabetic,
  etc.) to arabic numerals.

      # Task with some fancy nested lists

      (a) This will be number one.
      (b) This will be number two.

          - This is an extra level
          - This is the second bullet in the second level

      (d) This will be number three.

* `startnum`: Does not work correctly --- all Jira numbered lists must start at 1.

* `task_lists`: Maybe works. I haven't tried importing it yet, but it converts
  checkboxes into Unicode checkboxes...

      # Task with checkboxes
      - [ ] This is a checkbox item
      - [ ] This is another checkbox

* `example_lists`: Does not work 100% correctly --- all Jira numbered lists must
  start at 1, so "continued" numbering throughout the description will not
  occur. However, references to numbered items _does_ work, so you can
  shorthand a numbered reference.

      # Task with numbered reference

      (@useful) This item will be useful later.
      (@) Other item isn't so useful.

      Look back at item (@useful) for an example of what to  do.

* `simple_tables`: Works to create tables! Alignment is lost, however.

      # Task with a table in it

         Key      Data
      ------      -------
         foo      1
         bar      2
         baz      3

* `multiline_tables`: Works, but still loses alignment. Newlines are not
  preserved, flattening the table rows.

      # Task with a multi-line table in it

      I pulled the below table from the Pandoc documentation.

      -------------------------------------------------------------
       Centered   Default           Right Left
        Header    Aligned         Aligned Aligned
      ----------- ------- --------------- -------------------------
         First    row                12.0 Example of a row that
                                          spans multiple lines.

        Second    row                 5.0 Here's another one. Note
                                          the blank line between
                                          rows.
      -------------------------------------------------------------

* `grid_tables` and `pipe_tables`: Work, same caveats about alignment.

* `subscript` and `superscript`: Might work. It looks like it converts it, but
  I'm not positive the syntax converted to will be effective when imported.

* `tex_math_dollars`: It works about as well as it can, italicizing all
  variables, converting all symbols to unicode, and correctly sub- or
  superscripting anything as needed. If there's anything that Jira really can't
  display, like a fraction, it will just omit it from the output.

      # Issue with TeX math

      See Euler's Identity $e^{i\pi} + 1 = 0$

      Trying a fraction like \frac{1+x}{x} results in a blank space.

Issue Links
-----------

Links between issues is one of the core difficulties of creating tasks
one-by-one. It's hard to see the links, and you can only look at one task at a
time, making the process of verification correct.

Because Jira allows links to be preserved on import, we can create links via the
usual metadata. Any metadata field can be mapped to a "Link" field in Jira via
the import settings. Links can be to existing issues (by specifying their issue
key) or to new issues (by creating a unique Issue Id metadata item).

First, prepare your file similar to this:

```markdown
#Foo Issue

Issue Id
: foo

Epic Link
: PROJECT-123

Blocks
: bar
: PROJECT-321

#Bar Issue

Issue Id
: bar

Epic Link
: PROJECT-123
```

When importing the tasks, map Issue Id to `Issue Id`, and map Blocks to `Link
"Blocks"`. In this instance, the second task "bar" will be blocked by the first
task "foo." The "Issue Id" field does _not_ get used directly as the issue's
key; that gets assigned sequentially based on the project name and the highest
issue number for that project. Multiple values can be assigned for a single
link type.

Note that Epic Link and (for subtasks) Parent Id should work similarly, both for
new and old tasks.

For more information on linking issues, see [this][atlassian-links].

[atlassian-links]: https://confluence.atlassian.com/jirakb/import-issue-links-from-a-csv-file-in-jira-server-740262715.html (Import issue links from a CSV File in Jira server)
