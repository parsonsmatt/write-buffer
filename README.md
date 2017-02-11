# `write-buffer`

A stupid set of libraries for taking advantage of the speedup from bulk saving data.

Suppose you're saving, like, thousands of rows to a database. If you're saving each row one at a time, then that is kind of terribly slow. You can get big speedups by saving your rows in bulk.

However, that's kinda annoying to do. So this library makes it easier.
