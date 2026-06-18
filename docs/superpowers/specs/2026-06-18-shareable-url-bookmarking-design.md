# Shareable URL via Shiny Bookmarking — Design

**Date:** 2026-06-18
**Author:** Alan Brookhart (with Claude)
**Status:** Approved, pending implementation plan

## Goal

Let a user of the Study Size Planning Shiny app save the current configuration of all sidebar inputs into a URL that can be bookmarked or shared. Pasting that URL into a browser restores every input to its saved value.

## Approach

Use Shiny's built-in bookmarking with `enableBookmarking = "url"`. Shiny serializes every registered input into the URL's query string when the user clicks a `bookmarkButton()`, and restores them when the URL is loaded. No server-side storage required.

Rejected alternative: a custom pretty query string (`?p1=0.1&alpha=0.05&...`). It produces shorter, more readable URLs but requires hand-rolled parse/encode code that must be updated whenever a new input is added. Not worth the maintenance overhead for an internal/research tool.

## User Experience

1. The user adjusts inputs in the sidebar as today.
2. At the bottom of the sidebar there is a new **"Bookmark…"** button.
3. Clicking it opens a modal containing the current URL, pre-selected.
4. The user copies the URL (Cmd-C / Ctrl-C) and pastes it wherever they want — email, doc, browser bookmark.
5. Opening that URL later loads the app with all inputs (including the active tab and IPTW Simulation's `beta` slider) restored.

## Implementation Changes

Three small edits to `app.R`:

1. **Wrap `ui` in a function.** Shiny bookmarking requires `ui` to be `function(request) { ... }` rather than a static object. The body of the existing `fluidPage(...)` becomes the body of that function.

2. **Add `bookmarkButton()`** at the bottom of `sidebarPanel`, after the IPTW Variance Inflation `numericInput("v2", ...)`, before `sidebarPanel`'s closing paren. Optionally wrap with a separator (`hr()`) for visual grouping.

3. **Enable bookmarking** by changing the final line from
   ```r
   shinyApp(ui = ui, server = server)
   ```
   to
   ```r
   shinyApp(ui = ui, server = server, enableBookmarking = "url")
   ```

## Edge Case: the `v1`/`v2` Observer

`app.R:890-891` updates `v1` and `v2` reactively from `beta` (the IPTW Simulation tab's slider) via `updateNumericInput`. When restoring a bookmark, Shiny populates all inputs from the URL — but that observer may then fire because `beta` is also restored, overwriting the restored `v1`/`v2` with whatever values the observer recomputes.

**Fix:** Use Shiny's `onRestore()` hook to set a one-shot suppression flag that skips the observer for the first reactive flush after a restore. Concretely:

- Inside `server`, declare `restoring <- reactiveVal(FALSE)`.
- Register `onRestore(function(state) { restoring(TRUE) })`.
- At the top of the `v1`/`v2` observer, `if (isTRUE(restoring())) { restoring(FALSE); return() }`.

This preserves normal interactive behavior (changing `beta` still updates `v1`/`v2`) while letting saved `v1`/`v2` values survive a bookmark restore.

## URL Shape

Example (truncated):
```
https://app.host/?_inputs_&alpha=%220.05%22&p1=0.1&n1=1000&n2=1000&calc_mode=%22power%22&effect_measure=%22RD%22&RDrange=%5B0%2C0.1%5D&...
```

Shiny encodes JSON types in the query string so values round-trip correctly (strings quoted, numerics raw, vectors as JSON arrays).

## What's Out of Scope

- No automatic URL-update-as-you-type. Button only.
- No "copy to clipboard" customization — the default modal is one click plus Cmd-C.
- No persistence of outputs, plots, or table state in the URL. Only inputs.
- No server-side bookmarks (`enableBookmarking = "server"`); URL mode requires no storage.
- No new tests. This is UI wiring.

## Risk

Very low. Bookmarking is a stable, well-documented Shiny feature. The change is additive and reversible by removing the three edits.

## Files Touched

- `app.R` — three edits as described.

No new files. No changes to `R/statistical_functions.R`, `simulation_study.R`, `results.csv`, or any documentation other than this spec.
