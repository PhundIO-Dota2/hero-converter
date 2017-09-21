# hero-converter

A tool to convert descriptions of Dota 2-style heroes between various formats.

Currently undergoing initial development; you can't use it yet.

## Planned Features

Read a hero description from one of the following formats,
and output it in a different format.

Additionally, provide an interface to the [dotaconcept.com](http://dotaconcept.com) website.

### Markdown format

Read from a subset of Markdown, with fairly lenient formatting.

Output clean GitHub- or Reddit-flavored Markdown.

### Dota KV format

Output to the simple text-based KV format used by Dota 2.

In file-output mode (the recommended mode), will create the following files:

 - `game/scripts/heroes/<hero_id>.txt`, containing the hero's core attributes and reference to its abilities.
 - Several files of the form `game/scripts/abilities/<ability_name>.txt`: one for each of the hero's abilities, containing
   that ability's attributes (targeting type, etc) and values (e.g. damage, duration, etc)
 - For each ability with baseclass `ability_lua`, a lua script file is created at `game/scripts/vscripts/<lua_path>`.
   If not otherwise specified, `lua_path` defaults to `heroes/hero_<hero_name>/<ability_name>.lua`.
 - If the hero has talents specified, for each talent:
    - First, checks if the talent already exists.
    - Second, if the talent does not exist but a similar one does (e.g. a hero has +2.5 mana regen;
      there are mana regen talents, but none with that exact value), create a new talent based on the
      existing one with the `value` field already filled in.
    - Lastly, if the talent does not exist yet in any form (likely because it is specific to one of
      the hero's abilities), create a new talent based on `special_bonus_undefined` and fill in the
      correct `value`. If there is no associated `value`, it will be set to `1`.
 - `addon_english.txt`, containing a localization key for the hero's name and each ability's name, description and values.

In console-output mode, will instead output these files to the terminal and make no changes to the filesystem.

It is also possible to import from KV. Importing will follow references in KV, however it also supports an
extended syntax that reads abilities directly from the KV file.

## JSON / dotaconcept.com format

JSON input/output in the format used by dotaconcept.com. Note that this JSON is a subset of the internal
representation, and is thus not suitable as a storage format.

The executable has additional functions for downloading concepts from the website and/or posting them there.

## Stretch goal: Pandoc support

This tool uses [pandoc](http://pandoc.org) to read and write Markdown, so it should not be difficult to
extend it to support other formats that pandoc can read/write.