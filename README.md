# `write-buffer`

A stupid set of libraries for taking advantage of the speedup from bulk saving data.

Suppose you're saving, like, thousands of rows to a database. If you're saving each row one at a time, then that is kind of terribly slow. You can get big speedups by saving your rows in bulk.

However, that's kinda annoying to do. So this library makes it easier.

The `write-buffer-example` folder has a dumb example of this. We setup a web server to listen for POST requests of a record we want to save, and we collect up to 1000 of these requests in a queue. Every second, we save all of the stuff in the queue using a bulk insert.
