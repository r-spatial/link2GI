# Orfeo ToolBox (OTB) helpers: introspection and command construction

Public helpers to introspect OTB applications (by parsing \`-help\`
output) and to build/modify command lists consumable by \[runOTB()\].

Calls the OTB application with \`-help\` and parses the \`Parameters:\`
block into a parameter table.

Normalizes the parsed parameter table (from \[otb_capabilities()\]) into
a stable schema used for command building.

Convenience accessor based on \[otb_args_spec()\].

Like \[otb_required()\], but can ensure that a best-effort output key is
included.

Convenience accessor based on \[otb_args_spec()\]. Returns a named list
of optional parameters with \`NA_character\_\` placeholders or default
values.

Creates a command list suitable for \[runOTB()\], with mandatory
parameters always present as \`NA_character\_\` placeholders. Optional
parameters can be omitted, filled with defaults, or included as
\`NA_character\_\`.

Prints a compact overview (help line count, parameter count) and the
full normalized parameter spec table.

Updates \`cmd\[\[key\]\]\` to a normalized output path. If the parameter
is pixel-typed (\`\[pixel\]\` in OTB help), the value is set as
\`c(path, pixel_type)\`.

## Usage

``` r
otb_capabilities(algo, gili = NULL, include_param_help = FALSE)

otb_args_spec(algo, gili = NULL)

otb_required(algo, gili = NULL)

otb_required_with_output(algo, gili = NULL, enforce_output = TRUE)

otb_optional(algo, gili = NULL, with_defaults = TRUE)

otb_build_cmd(
  algo,
  gili = NULL,
  include_optional = c("none", "defaults", "all_na"),
  require_output = TRUE
)

otb_show(algo, gili = NULL)

otb_set_out(
  cmd,
  gili = NULL,
  key = "out",
  path,
  pixel_type = NULL,
  overwrite = TRUE,
  create_dir = TRUE
)
```

## Arguments

- algo:

  Character scalar. OTB application name.

- gili:

  Optional list from \[linkOTB()\]. If \`NULL\`, \[linkOTB()\] is
  called.

- include_param_help:

  Logical. If \`TRUE\`, additionally queries \`-help \<param\>\` for
  each parameter and returns these blocks as a named list.

- enforce_output:

  Logical. If \`TRUE\`, attempts to add an output key from a small set
  of common output parameter names (e.g. \`"out"\`, \`"io.out"\`).

- with_defaults:

  Logical. If \`TRUE\`, populate optional parameters with their default
  where available; otherwise use \`NA_character\_\`.

- include_optional:

  One of \`"none"\`, \`"defaults"\`, \`"all_na"\`.

- require_output:

  Logical. If \`TRUE\`, ensures that a best-effort output key
  placeholder exists if the application exposes one of the common output
  keys.

- cmd:

  Command list as produced by \[otb_build_cmd()\].

- key:

  Character scalar. Output parameter key to set (default \`"out"\`).

- path:

  Character scalar. Output file path.

- pixel_type:

  Optional character scalar pixel type (e.g. \`"float"\`). Only used if
  the output parameter is pixel-typed; if \`NULL\`, uses the pixel
  default from spec or \`"float"\` as fallback.

- overwrite:

  Logical. If \`FALSE\`, error if the file already exists.

- create_dir:

  Logical. If \`TRUE\`, create the output directory if missing.

## Value

A list with components: - \`text\`: character vector of help lines. -
\`params\`: data.frame parsed from the \`Parameters:\` block. -
\`param_help\`: \`NULL\` or named list of character vectors
(per-parameter help).

A data.frame with (at least) the columns: \`key\`, \`type\`,
\`mandatory\`, \`has_pixel\`, \`pixel_default\`, \`has_default\`,
\`default\`, \`class\`, \`desc\`.

Character vector of mandatory parameter keys.

Character vector of required parameter keys (including output if
enforced).

Named list of optional parameters.

A command list with \`cmd\[\[1\]\] == algo\` and named entries for
parameters.

Invisibly returns a list with components \`caps\` and \`spec\`.

The modified command list.

## Details

The functions in this family are \*\*non-invasive\*\*: they do not
mutate \`PATH\` or global environment variables. They rely on a valid
OTB descriptor as returned by \[linkOTB()\]. On Linux/macOS the
implementation expects a working launcher (\`gili\$launcher\`) and uses
an explicit environment map internally.

The common command representation is a list where: - \`cmd\[\[1\]\]\` is
the application name (character scalar), e.g.
\`"DimensionalityReduction"\`. - subsequent named entries represent CLI
parameters \*\*without\*\* a leading dash, e.g. \`cmd\[\["in"\]\]\`,
\`cmd\[\["out"\]\]\`, \`cmd\[\["method"\]\]\`. - values are character
scalars, \`NA_character\_\` (placeholder), or for pixel-typed output
parameters a character vector \`c(path, pixel_type)\`.

This function performs basic checks on directory existence and overwrite
policy.

## Introspection

\- \[otb_capabilities()\] returns the raw help text and a parsed
parameter table. - \[otb_args_spec()\] normalizes the parsed table into
a stable schema used by the helper functions below.

## Command helpers

\- \[otb_required()\], \[otb_required_with_output()\],
\[otb_optional()\] - \[otb_build_cmd()\] creates a template command list
using the spec. - \[otb_set_out()\] sets/validates an output parameter
path (optionally pixel-typed). - \[otb_show()\] prints a compact
overview for interactive use.

## See also

\[linkOTB()\], \[runOTB()\], \[runOTB_isolated()\]
