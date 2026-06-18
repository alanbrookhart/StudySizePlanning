# Shareable URL Bookmarking Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let users save and share their current sidebar input configuration via a URL by enabling Shiny's built-in bookmarking and adding a Bookmark button to the sidebar.

**Architecture:** Three additive edits to `app.R`. (1) Wrap the `ui` object in `function(request) { ... }` so Shiny can replay state into it on restore. (2) Add a `bookmarkButton()` at the bottom of `sidebarPanel`. (3) Pass `enableBookmarking = "url"` to `shinyApp()` so all input state is serialized into the query string. No new files; no changes to statistical code, simulation data, or tests.

**Tech Stack:** R, Shiny (built-in bookmarking, `bookmarkButton`, `enableBookmarking`).

## Global Constraints

- Only modify `app.R`. Do not touch `R/statistical_functions.R`, `simulation_study.R`, `results.csv`, `www/`, or `tests/`.
- The existing visual layout, color scheme, MathJax setup, and tab structure must be unchanged.
- Bookmark mode is **`"url"`** (not `"server"`).
- All existing inputs must continue to work exactly as today; the change is additive.
- Run the app locally to verify behavior before committing the final change.

---

### Task 1: Wrap UI in a request-function and enable bookmarking

**Files:**
- Modify: `app.R:45` (the `ui <- fluidPage(...)` block) and `app.R:` last line (`shinyApp(...)`).

**Interfaces:**
- Consumes: nothing from prior tasks.
- Produces: `ui` is now `function(request) fluidPage(...)`. The app starts with bookmarking enabled in URL mode, so any `bookmarkButton()` placed in the UI in Task 2 will function.

**Why this comes first:** Bookmark mode and the request-aware UI must be in place before adding the button; otherwise `bookmarkButton()` produces a UI element that does nothing.

- [ ] **Step 1: Inspect the current UI assignment**

Run: `grep -n "^ui <- fluidPage" app.R`
Expected output:
```
45:ui <- fluidPage(
```

- [ ] **Step 2: Wrap the UI in a function(request) wrapper**

Edit `app.R` line 45. Change:

```r
ui <- fluidPage(
```

to:

```r
ui <- function(request) fluidPage(
```

Then, find the matching closing paren of `fluidPage(...)`. It is the closing `)` of the top-level `fluidPage()` call — i.e., the line that closes the entire UI block, just before `# --- 3. Server Logic ---`. In the current file that is the line containing `)` at approximately app.R:541.

That closing `)` stays as-is — `function(request) fluidPage(...)` is a single expression, so no extra closing brace is needed because R lets a one-line function body omit braces.

- [ ] **Step 3: Inspect the current shinyApp call**

Run: `grep -n "shinyApp" app.R`
Expected output (line number may differ slightly):
```
<N>:shinyApp(ui = ui, server = server)
```

- [ ] **Step 4: Enable URL bookmarking on shinyApp**

Edit `app.R` last line. Change:

```r
shinyApp(ui = ui, server = server)
```

to:

```r
shinyApp(ui = ui, server = server, enableBookmarking = "url")
```

- [ ] **Step 5: Verify the app still parses and starts**

Run from the project root:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = FALSE, port = 4321)' &
APP_PID=$!
sleep 6
curl -sS -o /dev/null -w "%{http_code}\n" http://127.0.0.1:4321/
kill $APP_PID 2>/dev/null
```

Expected: `200`. If you get a parse error or non-200, re-check the edits.

- [ ] **Step 6: Commit**

```bash
git add app.R
git commit -m "feat: enable URL bookmarking and wrap UI in request function"
```

---

### Task 2: Add the Bookmark button to the sidebar

**Files:**
- Modify: `app.R:300-302` (the bottom of `sidebarPanel`, right after the `numericInput("v2", ...)` line).

**Interfaces:**
- Consumes: URL bookmarking enabled in Task 1.
- Produces: a visible "Bookmark…" button at the bottom of the sidebar. Clicking it opens a modal containing the current shareable URL.

- [ ] **Step 1: Locate the IPTW Variance Inflation inputs**

Run: `grep -n 'numericInput("v2"' app.R`
Expected:
```
301:      numericInput("v2", "IPTW Variance Inflation (Group 2):", 1.3, min = 1, step = 0.1)
```

- [ ] **Step 2: Add a separator and bookmarkButton after v2**

In `app.R`, replace this block (around lines 300-302):

```r
      numericInput("v1", "IPTW Variance Inflation (Group 1):", 1.3, min = 1, step = 0.1),
      numericInput("v2", "IPTW Variance Inflation (Group 2):", 1.3, min = 1, step = 0.1)
    ),
```

with:

```r
      numericInput("v1", "IPTW Variance Inflation (Group 1):", 1.3, min = 1, step = 0.1),
      numericInput("v2", "IPTW Variance Inflation (Group 2):", 1.3, min = 1, step = 0.1),

      hr(),
      bookmarkButton(label = "Bookmark this configuration")
    ),
```

Note the comma added at the end of the `v2` line — without it the file will not parse.

- [ ] **Step 3: Verify the file parses**

Run:

```bash
Rscript -e 'parse("app.R"); cat("ok\n")'
```

Expected: `ok`. If you see a parse error, re-check that the comma was added after the `v2` `numericInput(...)` line.

- [ ] **Step 4: Verify the button appears**

Run:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = FALSE, port = 4321)' &
APP_PID=$!
sleep 6
curl -sS http://127.0.0.1:4321/ | grep -o '_bookmark_' | head -1
kill $APP_PID 2>/dev/null
```

Expected: `_bookmark_` (Shiny tags the bookmark button with the `_bookmark_` input ID in the rendered HTML).

- [ ] **Step 5: Commit**

```bash
git add app.R
git commit -m "feat: add Bookmark button to sidebar"
```

---

### Task 3: Manual end-to-end verification

**Files:**
- No edits. This task is a manual verification gate, with the result captured as a follow-up note in the spec if any issue surfaces.

**Interfaces:**
- Consumes: Tasks 1 and 2 complete.
- Produces: confidence the feature works for users.

- [ ] **Step 1: Start the app and open it in a browser**

Run from the project root:

```r
shiny::runApp(".")
```

(Or use RStudio's "Run App" button if working in RStudio.)

- [ ] **Step 2: Change several inputs**

In the browser:
1. Set **Calculation Mode** to "Calculate Sample Size".
2. Change **Target Estimand** to "Risk Ratio (RR)".
3. Set **Alpha** to `0.01`.
4. Set **Risk in Group 1 (p1)** to `0.15`.
5. Move the **Target Power** slider to `0.90`.
6. Set **Censoring Risk (Group 1)** to `0.30`.

- [ ] **Step 3: Click the Bookmark button**

Click **"Bookmark this configuration"** at the bottom of the sidebar.
Expected: a modal appears containing a URL like
`http://127.0.0.1:NNNN/?_inputs_&...&alpha=%220.01%22&p1=0.15&...`.

Copy the URL.

- [ ] **Step 4: Open the URL in a new browser tab**

Paste the URL into a new tab.
Expected:
- Calculation Mode = "Calculate Sample Size"
- Target Estimand = "Risk Ratio (RR)"
- Alpha = `0.01`
- p1 = `0.15`
- Target Power = `0.90`
- Censoring Risk (Group 1) = `0.30`

All other inputs match what they were when you bookmarked.

- [ ] **Step 5: Stop the app**

Press `Ctrl-C` in the terminal (or click the Stop button in RStudio).

- [ ] **Step 6: If verification passed, no commit needed (no edits made)**

If you find a defect during verification, add a new task to this plan describing the fix and pause for review rather than improvising.

---

## Self-Review Notes

- **Spec coverage:** Spec sections "Approach", "User Experience", and "Implementation Changes" are all addressed by Tasks 1-2. The "URL Shape" section is verified by Task 3 Step 3. "Non-Issue: the v1/v2 Observer" requires no implementation work (confirmed in this plan's preamble).
- **Out-of-scope items** from the spec (no auto-URL-update, no custom clipboard handling, no output state, no server-side bookmarks, no new tests) are honored by this plan — none of them appear as tasks.
- **No placeholders** remain; every code/command block is concrete.
