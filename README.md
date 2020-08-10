# mastodon-hn

Toots two HN posts every two hours.

The two posts are:
1. The highest ranked post not tooted yet
2. One random post from the top 50 not tooted yet, score-weighted.

Takes two lines from `stdin` on startup. The first is the instance name (i.e. `botsin.space`), the second is the application token.

PRs welcome. If you want to run this for your own use, let me know; we can add tests/add documentation/make it more extensible.

https://botsin.space/@hnbot
