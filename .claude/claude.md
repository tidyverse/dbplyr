## SQL

### Research workflow

CRITICAL: SQL correctness is paramount in dbplyr. Before implementing SQL for any backend, you MUST follow this research workflow:

1. **Search**: Use WebSearch to find official documentation for "{dialect} {command}"
   - Prioritize official database documentation and reputable sources
   - Search for both syntax and behavior, including edge cases
   - Look for version-specific differences if relevant

2. **Document**: Create `research/{dialect}-{command}.md` with:
   - Minimal, focused summary relevant to translating R code to SQL
   - Syntax examples from official sources
   - Important notes about behavior, limitations, or gotchas for dbplyr's use case
   - ALL citations with URLs (REQUIRED - no exceptions)
   - NO comparisons with other databases
   - Keep it as concise as possible

3. **Verify**: Cross-reference multiple sources when:
   - Documentation seems incomplete or unclear
   - Behavior differs across database versions
   - Edge cases aren't well documented

4. **Implement**: Only after completing research and documentation

Example research file structure:
```markdown
# {Dialect} - {Command}

## Summary
[1-2 sentence summary focused on R-to-SQL translation]

## Syntax
[Minimal syntax examples from official sources]

## Key behaviors
[Only the behaviors that matter for dbplyr translation]

## Limitations
[Only restrictions that affect dbplyr usage]

## Sources
- [Source name](URL)
```

## R package development

### Key commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run tests for R/{name.R}
Rscript -e "devtools::test(filter = '{name}', reporter = 'llm')"

# To document the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To format code
air format .
```

### Coding

* Always run `air format .` after generating code
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`)

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`. 
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Whenever you add a new documentation topic, also add the topic to `_pkgdown.yml`. 
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

### Writing

- Use sentence case for headings.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing. 

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading. 

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
