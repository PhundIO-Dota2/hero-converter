# Tentative DotaConcept API Docs

Documentation for the [dotaconcept.com](http://dotaconcept.com) API, obtained by
monitoring requests made by the site during use and judicious use of `curl`.

There appear to be two API base URLs:
    
 - `http://dotaconcept.com/api`
 - `http://dotaconcept.com/commands`

## GET a hero concept

    http://dotaconcept.com/api/getConcept/{concept-id}

Returns a JSON representation of a hero concept, specified via `concept-id`. An example of the JSON format can be found at <aura.json>.

## POST update a hero concept

    http://dotaconcept.com/api/setConcept/{concept-id}

Payload: the hero concept to save to that ID, in JSON format. Uses the same JSON format as `/getConcept`,
but with the following fields omitted: `conceptExists`, `conceptOwner`, `conceptPrivate`, `imgCache`.

Returns a JSON indicating success/failure.

## GET create a new hero concept

    http://dotaconcept.com/commands/hero/newConcept

Returns nothing.

When called, creates a new hero concept and redirects to the user's personal library. The new hero concept
will appear at the bottom of the concept list.

## POST vote on a hero concept

    http://dotaconcept.com/api/vote/concherodota/{concept-id}/{vote}

Payload: an empty JSON object (`{}`).

Vote on a hero concept.

Parameter | Description
---|---
`concept-id` | The ID of the concept to vote on
`vote` | The direction of the vote (`"up"` or `"down"`)