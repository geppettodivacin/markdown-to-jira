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

This tool is intended to provide a better workflow to add a batch of Jira
issues. The batch is written as a single Pandoc-flavored Markdown file with a
header for each new issue. The description and the issue metadata (issue type,
priority, epic, links, etc.) go under the headers. The Markdown is converted to
a CSV file, ready to be imported by Jira's CSV Import tool.

For more information about Pandoc-flavored Markdown, see [this
page][pandoc-markdown].

For more information about Jira CSV Import, see [this page][jira-csv-import].

[pandoc-markdown]: https://pandoc.org/MANUAL.html#pandocs-markdown (Pandoc Markdown)

[jira-csv-import]: https://support.atlassian.com/jira-cloud-administration/docs/import-data-from-a-csv-file/ (Import data from a CSV file)
