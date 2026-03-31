# Exporting Functions for Deployment

Turn your L4 rules into live REST API endpoints on [Legalese Cloud](https://legalese.cloud) that applications and AI agents can call.

**Audience:** L4 authors ready to share their rules with the world
**Prerequisites:** Basic L4 knowledge ([Your First L4 File](../getting-started/first-l4-file.md))
**Time:** 20 minutes
**Goal:** Mark functions for export, deploy them from VS Code to Legalese Cloud, and call them via the REST API

---

## What You'll Build

An insurance premium calculator with two exported functions: one to compute a premium and one to check discount eligibility. You will deploy them from VS Code to the Legalese Cloud and call them as a REST API.

**Complete example:** [insurance-premium.l4](insurance-premium.l4)

---

## Step 1: Mark Functions with `@export`

The `@export` annotation tells L4 which functions should be exposed as API endpoints. Place it directly above a `DECIDE` or `MEANS` definition:

```l4
@export Calculate the insurance premium for an applicant
GIVEN applicant IS A Applicant @desc The applicant's details
GIVETH A NUMBER
DECIDE `calculate premium` IS
    IF applicant's `risk score` > 0.7
    THEN applicant's `age` * 100
    ELSE applicant's `age` * 60
```

Without `@export`, a function stays internal — only usable within the file, never exposed to the outside world.

### The `@export` annotation for functions explained

| Syntax                          | Meaning                                                                      |
| ------------------------------- | ---------------------------------------------------------------------------- |
| `@export <description>`         | Export this function with a human-readable description                       |
| `@export default <description>` | Export as the **default** function (used when no function name is specified) |
| `@desc <description>`           | Internal description only — **not** exported                                 |

The description text becomes the function's documentation in the generated API schema. Use it above the GIVEN/GIVETH parameter or return type definitions.

### Parameter descriptions with `@desc`

Add inline `@desc` annotations to `GIVEN` parameters so API consumers can learn what each input means:

```l4
@export Check whether the applicant qualifies for a discount
GIVEN applicant IS A Applicant @desc The applicant to check
GIVETH A BOOLEAN
DECIDE `qualifies for discount` IF
    applicant's `is existing customer`
    AND applicant's `risk score` <= 0.5
```

---

## Step 2: Verify Locally

Before deploying, make sure your file compiles and the exports look right.

### In VS Code

1. Open your `.l4` file
2. Open the **L4 sidebar** panel
3. Click the **Deploy** tab

The Deploy tab shows a preview of all exported functions discovered in the active file — their names, descriptions, parameters, and return types. If the list is empty, your file either has no `@export` annotations or contains errors.

---

## Step 3: Deploy from VS Code

1. Make sure your extension is properly connected to your free [Legalese Cloud](https://legalese.cloud) account.
2. Verify the exported functions preview looks correct
3. Click **Deploy**
4. Enter a **deployment name** (e.g. `insurance-premium`) — this becomes part of the API URL. The name must be 1–36 characters, using only letters, numbers, hyphens, and underscores.
5. Click **Deploy Now**

The extension uploads your file to the L4 service, which compiles it and makes the functions available as REST endpoints.

### Deployment status

After deploying, the status progresses through:

| Status        | Meaning                                     |
| ------------- | ------------------------------------------- |
| **Pending**   | Uploaded, waiting to compile                |
| **Compiling** | Being compiled by the service               |
| **Ready**     | Live and accepting requests                 |
| **Failed**    | Compilation error — check the error message |

### Updating an existing deployment

When you deploy to a name that already exists, the extension checks for **breaking changes** — such as removed parameters, changed return types, or renamed functions. If breaking changes are detected, you will see a warning before confirming the update.

---

## Step 4: Manage Deployments

Switch to the **Deployments** tab in the sidebar to see all active deployments. Each deployment shows:

- The deployment name
- The number of exported rules
- Each function's name, description, and parameters

From here you can:

- **Expand/collapse** deployments to inspect their functions
- **Undeploy** a deployment to remove it (with a confirmation prompt, since this breaks existing integrations)

---

## Step 5: Call the API

Once a deployment is **Ready**, its functions are available as REST endpoints. You can visit your Legalese Cloud home directory to see them at `https://{your-org}.legalese.cloud`.

### List available functions

Here are some ways you can use the REST API from your command line:

```bash
curl https://{your-org}.legalese.cloud/deployments/insurance-premium/functions
```

### Evaluate the default function

```bash
curl -X POST http://your-service/deployments/insurance-premium/functions/calculate_premium/evaluation \
  -H "Content-Type: application/json" \
  -d '{
    "arguments": {
      "applicant": {
        "age": 35,
        "risk score": 0.4,
        "is existing customer": true
      }
    }
  }'
```

### Evaluate a specific function

```bash
curl -X POST http://your-service/deployments/insurance-premium/functions/qualifies_for_discount/evaluation \
  -H "Content-Type: application/json" \
  -d '{
    "arguments": {
      "applicant": {
        "age": 35,
        "risk score": 0.4,
        "is existing customer": true
      }
    }
  }'
```

### Interactive query plans

Not sure which inputs matter? Use the query-plan endpoint to ask only the questions that affect the outcome:

```bash
curl -X POST http://your-service/deployments/insurance-premium/functions/calculate_premium/query-plan \
  -H "Content-Type: application/json" \
  -d '{"arguments": {"applicant": {"is existing customer": true}}}'
```

This returns which inputs are still needed, ranked by their impact on the result.

---

## Step 6: Expose to AI Agents with WebMCP

Once deployed, your L4 functions are automatically available as [WebMCP](https://webmachinelearning.github.io/webmcp/) tools. WebMCP is a W3C standard that lets websites expose structured tools to browser-based AI agents — no scraping or DOM inspection required.

The L4 service provides a JavaScript snippet you can embed on any web page:

```html
<!-- Expose all deployed functions as WebMCP tools -->
<script src="https://{your-org}.legalese.cloud/webmcp.js"></script>

<!-- Scope to specific deployments -->
<script
  src="https://your-service/webmcp.js"
  data-scope="insurance-premium/*"
></script>
```

The script registers discovery tools (`search_rules`, `get_rule_schema`, `evaluate_rule`) that AI agents can call to find and invoke your L4 rules. When the number of functions is small (≤ 20), direct per-function tools are also registered.

You can explore your deployments and grab the embed snippet from the service's built-in deployment explorer at the service root URL.

---

## Summary

| Step     | What you do                                              |
| -------- | -------------------------------------------------------- |
| Annotate | Add `@export` (and optionally `default`) above functions |
| Describe | Add `@desc` to parameters for API documentation          |
| Preview  | Check the Deploy tab in VS Code                          |
| Deploy   | Click Deploy, choose a name, confirm                     |
| Manage   | Use the Deployments tab to inspect and undeploy          |
| Call     | Use the REST API or embed WebMCP on a web page           |

---

## Common Mistakes

### Forgetting `@export`

```l4
-- ❌ No @export — this function won't be deployed
GIVEN x IS A NUMBER
GIVETH A NUMBER
squared x MEANS x * x

-- ✅ With @export — available via the API
@export Calculate the square of a number
GIVEN x IS A NUMBER @desc The input number
GIVETH A NUMBER
squared x MEANS x * x
```

### Placing `@export` in the wrong position

`@export` must appear directly above the function definition (before `GIVEN` or the function name), not between `GIVETH` and `DECIDE`:

```l4
-- ❌ Wrong — @export between GIVETH and DECIDE
GIVEN x IS A NUMBER
GIVETH A NUMBER
@export Square a number
squared x MEANS x * x

-- ✅ Right — @export before the function
@export Square a number
GIVEN x IS A NUMBER
GIVETH A NUMBER
squared x MEANS x * x
```

### Missing parameter descriptions

Without `@desc` on parameters, API consumers won't know what inputs mean. Always describe your parameters:

```l4
-- ❌ No parameter descriptions
@export Calculate premium
GIVEN applicant IS A Applicant
GIVETH A NUMBER
DECIDE `calculate premium` IS ...

-- ✅ With parameter descriptions
@export Calculate premium
GIVEN applicant IS A Applicant @desc The applicant's details
GIVETH A NUMBER
DECIDE `calculate premium` IS ...
```

---

## Next Steps

- [Common Patterns](../getting-started/common-patterns.md) — Frequently used L4 patterns
- [Reference: Annotations](../../reference/GLOSSARY.md) — All L4 annotations including `@export` and `@desc`
