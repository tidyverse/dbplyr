---
name: create-skill
description: Guide for creating new Claude Code skills. Use when you need to create a new skill to package expertise or workflow into a reusable capability that Claude can automatically invoke.
---

# Create Skill

Use this skill when creating new Claude Code skills that package expertise, workflows, or domain knowledge into reusable capabilities.

## Overview

Skills are autonomous capabilities that Claude Code can invoke automatically based on the user's request. Each skill consists of a SKILL.md file with YAML frontmatter and markdown instructions, plus optional supporting files.

## Skill structure

### Directory layout

```
.claude/skills/
└── your-skill-name/
    ├── SKILL.md          # Required: Main skill definition
    ├── reference.md      # Optional: Additional documentation
    ├── examples.md       # Optional: Example usage
    ├── templates/        # Optional: Template files
    │   └── template.txt
    └── scripts/          # Optional: Helper scripts
        └── helper.py
```

### SKILL.md format

Every SKILL.md file must have:

1. **YAML frontmatter** (required)
2. **Markdown content** with instructions

#### YAML frontmatter

```yaml
---
name: skill-name
description: Brief description of what the skill does and when to use it (max 1024 chars)
---
```

**Requirements:**
- `name`: lowercase letters, numbers, and hyphens only (max 64 characters)
- `description`: Clear description for Claude to understand when to invoke this skill
  - Should explain WHAT the skill does
  - Should explain WHEN to use it
  - The description is critical for discoverability

#### Markdown content

Structure your skill instructions clearly:

```markdown
# Skill Name

Brief introduction of when to use this skill.

## Overview

High-level explanation of what this skill does.

## Workflow

### Step 1: First step
- Details
- Instructions

### Step 2: Second step
- More details

## Key concepts

Important concepts the user needs to understand.

## Examples

Concrete examples showing how to use the skill.

## Checklist

- [ ] Verification steps
- [ ] Required actions
```

## Creating a new skill

### 1. Identify the need

Create a skill when:
- You have a repeating workflow that requires multiple steps
- You want to package domain expertise (like SQL translation, code review patterns)
- You need to ensure consistent processes are followed
- You want to make complex tasks accessible

### 2. Design the skill

Plan your skill by answering:
- **What**: What does this skill do?
- **When**: When should Claude invoke it?
- **How**: What are the step-by-step instructions?
- **Why**: What expertise or knowledge does it encode?

### 3. Write the SKILL.md

**YAML frontmatter:**
- Choose a descriptive `name` (kebab-case)
- Write a clear `description` that helps Claude understand when to use it
- The description should be specific enough to avoid false positives

**Content structure:**
- Start with a brief introduction
- Break down the workflow into clear, numbered steps
- Include examples from the actual codebase when relevant
- Provide a checklist for verification
- Keep instructions concise but complete

### 4. Add supporting files (optional)

If your skill needs:
- **Templates**: Add to `templates/` directory
- **Scripts**: Add to `scripts/` directory
- **Reference docs**: Create `reference.md`
- **Examples**: Create `examples.md`

### 5. Test the skill

Test that Claude invokes your skill by:
1. Asking a question that should trigger the skill
2. Verifying Claude loads and follows the skill instructions
3. Checking that the workflow produces correct results

Iterate on the description if Claude doesn't invoke it at the right times.

## Best practices

### Description writing

✅ **Good descriptions:**
- "Guide for adding SQL function translations to dbplyr backends. Use when implementing new database-specific R-to-SQL translations."
- "Code review checklist for security vulnerabilities. Use after writing authentication, database, or API code."

❌ **Bad descriptions:**
- "SQL stuff" (too vague)
- "Use this for everything related to databases" (too broad)

### Instruction writing

- **Be specific**: Provide concrete steps, not vague guidance
- **Be concise**: Remove unnecessary words
- **Use examples**: Show, don't just tell
- **Reference real files**: Point to actual codebase examples
- **Include verification**: Add checklists to ensure completeness

### Naming conventions

- Use kebab-case for skill names
- Choose names that describe the action or domain
- Examples: `sql-translation`, `create-skill`, `review-security`, `deploy-production`

## Skill invocation

### How skills work

1. **Discovery**: Claude reads the skill's description
2. **Decision**: Claude decides if the skill matches the user's request
3. **Loading**: The SKILL.md file is loaded into the conversation context
4. **Execution**: Claude follows the skill's instructions
5. **Context**: Supporting files are available if referenced

### Automatic vs manual invocation

- **Automatic** (preferred): Claude invokes based on description match
- **Manual**: User explicitly requests the skill (not common)

Most skills should be designed for automatic invocation.

## Examples

### Minimal skill

```yaml
---
name: format-code
description: Format code using air format. Use after writing or modifying R code files.
---

# Format Code

Run `air format .` to format all R code in the project.

## Checklist

- [ ] Run `air format .`
- [ ] Verify no formatting errors
```

### Workflow skill

```yaml
---
name: add-test
description: Add tests for new R functions. Use when creating new functions in R/ directory.
---

# Add Test

Add tests for new R functions following dbplyr conventions.

## Workflow

### 1. Identify test file
- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`

### 2. Write tests
- Place new tests next to similar existing tests
- Keep tests minimal with few comments
- Use `expect_snapshot()` for SQL translation tests

### 3. Run tests
```bash
Rscript -e "devtools::test(filter = '{name}', reporter = 'llm')"
```

## Checklist

- [ ] Created/updated test file
- [ ] Tests are minimal and focused
- [ ] All tests pass
```

## Common patterns

### Research workflows

For skills that require research before implementation:
1. Specify search steps with WebSearch
2. Require documentation with citations
3. Only implement after research is complete

See `sql-translation` skill for an example.

### Multi-step processes

For complex workflows:
1. Break into numbered steps
2. Use subsections for each step
3. Include verification at each stage
4. Provide a final checklist

### Domain expertise

For packaging specialized knowledge:
1. Explain key concepts upfront
2. Provide reference information
3. Include decision trees or flowcharts
4. Link to external documentation

## Troubleshooting

**Skill not being invoked:**
- Check description clarity
- Make description more specific
- Verify YAML syntax

**Skill invoked at wrong times:**
- Description too broad
- Add specifics about when NOT to use it

**Instructions unclear:**
- Add more concrete examples
- Break down complex steps
- Reference actual files from the codebase

## Checklist

Before completing a new skill:

- [ ] Created `.claude/skills/{skill-name}/` directory
- [ ] Created `SKILL.md` with YAML frontmatter
- [ ] `name` field uses kebab-case (lowercase, hyphens only)
- [ ] `description` clearly explains what and when (max 1024 chars)
- [ ] Content has clear structure with sections
- [ ] Workflow broken into numbered steps
- [ ] Examples included where helpful
- [ ] Checklist provided for verification
- [ ] Tested that Claude invokes the skill correctly
- [ ] Supporting files added if needed
