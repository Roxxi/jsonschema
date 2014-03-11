# Catechism of jsonschema

Shawn:

yo! Can you help me understand merge and simplify in jsonschema?

Matt:

yes. In a sentence -- they both merge/combine two schemas, but merge
is very strict and simplify is very loose. Also, 'merge' and
'simplify' are terrible names. Alex came up with them and I couldn't
think of anything better to change them to. So, I'm sorry in advance
for that... it always drives me nuts when things are named poorly...

Simplify will come up with a single unifying type which can represent
all of the records.

Merge will come up with a set of distinct schemas observed, being as
anal-retentive as possible about the distinction between schemas.

Maybe check the unit tests for more concrete examples.

Shawn:

ah nice, thanks. That intuition behind merge/simplify helps a lot.

Matt:

yeah. So, e.g., for yoda we use simplify, because we want to derive a
schema which everything can fit into.

Shawn:

ok, that makes sense.

Matt:

what's the situation you're thinking of using it in?

Shawn:

for schema diagnosis in beastload. Very similar to the yoda use
case. Given two tables, generate a schema that can accomodate both
schemas (or at least, diagnose the differences).

Matt:
You want simplify. You described the signature of the function
"simplify" there :)

Shawn:

ok perfect. Since you use simplify in yoda, that was a big hint that
it should work for this too.

Matt:

Sounds like, if you've written the code to munge db-types into
jsonschema-types, you're done.

Shawn:

nice! yes that part is done. now its just a matter of hooking it into
BL, which I'm doing now.

Matt:

I did a lot of work on jsonschema in the fall... I am so glad that it
is finally getting used more. What I did was add metadata tracking,
e.g. if you merge a String field (min observed length=20, max observed
length=30) with a String field (min=20, max=50), you get a String(20,50).

Why do we care? So that we know how wide to make our Varchar columns!
lol. Version 1 of jsonschema would just say, "Oh you have two strings?
Well, when you merge them, you still get a string" with no metadata.
But now we can say, "Oh hey, I looked at every record in this file,
and this one field looks like a string, and the longest observed
string-value for that field was a string of 542 characters", and
similarly for ints (min/max) and reals (min/max) and date formats.

Shawn:

nice! That will definitely help with the diagnosis resolution being
smarter so we're not just generating tons of varchar(65000) fields or
bigints all over the place.

Matt:

yes. We can also prevent truncation by detecting it before it
happens. i.e. when we got our 10 millionth member, customer ids were
getting truncated in yoda because the customer id field (which was a
varchar...) was too narrow.

Shawn:

oh right, I remember that.

Matt:

but in theory, we could have looked over the data on the first day
where customerId was > 10 MM, and said, "Hey... the schema for the
table has customerId as String(max length = 7) but the schema for the
CSV file has customerId as String(min length=0, max length=8). Let's
resize the column width in the table to prevent truncation of the
records we're about to load."

Anyway, the point was that if we want, we can make the fields exactly
as wide as they need to be and resize them as we encounter data that
doesn't quite fit.

So having the metadata opens up the road for lots of little wins.

## Usage

This stuff is mostly for tinkering with in the REPL and is FAR from production ready, at the moment.

## License

Copyright © 2013 Alex Bahouth

Licensed under the GNU Public License v3.0 (GPLv3)
