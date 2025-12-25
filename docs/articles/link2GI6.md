# A Deterministic GDAL CLI Context for Reproducible Workflows

## Motivation

GDAL is available in R through high-level packages such as **sf** and
**terra**, and through wrapper packages like **gdalUtilities**. These
approaches work well for many use cases, but they share a fundamental
limitation:

> **They bind against a specific GDAL build, not necessarily the GDAL
> binaries actually used on the system.**

In practice, this leads to several recurring problems:

- Different GDAL versions on different machines
- Mismatch between linked libraries and CLI binaries
- Silent changes in available drivers, options, or defaults
- Poor traceability of *what actually ran* in batch or HPC contexts

The goal of the `link2GI` GDAL context is **not** to replace existing
packages, but to **expose and control the system GDAL CLI
deterministically**.

------------------------------------------------------------------------

## Design Goals

The GDAL context introduced here follows a small set of strict
principles:

1.  **Use the linked GDAL binaries explicitly**  
    No implicit reliance on `PATH`, no guessing, no fallback to other
    builds.

2.  **Be system-honest**  
    The context reflects what the system *can actually do*, not what a
    package was compiled against.

3.  **Remain thin and non-intrusive**  
    No re-implementation of GDAL semantics, no competing wrapper API.

4.  **Enable reproducibility**  
    Every run can be fingerprinted and logged, including environment and
    versions.

5.  **Support interactive and batch workflows equally**  
    From exploratory work to large batch runs or CI pipelines.

------------------------------------------------------------------------

## Core Concept: The GDAL Context

At the heart of the approach is a **GDAL CLI context**:

- A fixed `binDir` containing GDAL executables
- A per-call environment (`PATH`, `GDAL_DATA`, `PROJ_LIB`, …)
- A resolved set of available GDAL utilities
- Optional logging and help caching

This context is **immutable for a run** and can be reused safely.

------------------------------------------------------------------------

## Creating a Context from `linkGDAL()`

The recommended entry point is
[`linkGDAL()`](https://r-spatial.github.io/link2GI/reference/linkGDAL.md):

``` r
gdal_link <- linkGDAL()
```

This locates a valid GDAL installation on the system. From this result,
a deterministic CLI context is created:

``` r
ctx <- gdal_context_from_link(gdal_link)
```

At this point:

- The exact GDAL binaries are fixed
- The effective environment is known
- No global state has been modified

------------------------------------------------------------------------

## Inspecting Capabilities (Fingerprinting)

To document *what this GDAL can do*, a fingerprint can be collected:

``` r
fp <- gdal_fingerprint(ctx)
```

This records, among other things:

- GDAL version string
- Available raster and vector formats
- Spatial reference behaviour (axis order probe)
- Context identifier

Optionally, the fingerprint can be written to an NDJSON log for
auditability.

------------------------------------------------------------------------

## Running GDAL Commands Deterministically

A GDAL utility can now be executed explicitly:

``` r
res <- run_gdal(
  ctx,
  cmd  = "gdalinfo",
  args = c("--formats")
)
```

Key properties:

- The resolved executable path is used
- The environment is passed explicitly
- Output and exit status are captured
- No reliance on global `PATH`

This makes the call reproducible across machines *as long as the same
binaries are used*.

------------------------------------------------------------------------

## Help-Driven Skeleton Generation

One recurring pain point when using the GDAL CLI is remembering valid
flags and argument structure.

The context provides a **help-based skeleton**:

``` r
sk <- gdal_skeleton(ctx, "gdalwarp")
```

This skeleton is derived directly from `gdalwarp --help` and contains:

- The primary `Usage:` line
- A flat list of supported flags
- A minimal argument template

Importantly:

> The skeleton does **not** guess semantics or defaults. It reflects
> exactly what this GDAL build exposes.

------------------------------------------------------------------------

## Building Arguments Programmatically

Skeletons can be combined with structured options:

``` r
args <- gdal_build_args(
  sk,
  opts = list(
    t_srs = "EPSG:25832",
    r     = "bilinear"
  ),
  positional = c("in.tif", "out.tif")
)
```

This avoids error-prone copy-paste and keeps CLI usage explicit.

------------------------------------------------------------------------

## NDJSON Logging for Reproducibility

Every GDAL call can optionally be logged as **one JSON object per
line**:

- Command and arguments
- Resolved executable
- Environment variables
- Runtime, exit code, stdout/stderr
- Host and system metadata

This format is:

- Append-only
- Stream-friendly
- Easy to post-process or archive

It enables answering questions like:

> *Which GDAL, with which environment, produced this output?*

------------------------------------------------------------------------

## Relationship to Existing Packages

This approach is **complementary**, not competitive:

- **sf / terra** Excellent for in-memory workflows and analysis; rely on
  linked libraries.

- **gdalUtilities** Provides R wrappers for many CLI tools, but still
  depends on system setup.

- **link2GI GDAL context** Focuses on *deterministic execution*,
  *capability introspection* and *traceability*.

You can freely mix these approaches in one project.

------------------------------------------------------------------------

## When Is This Useful?

Typical use cases include:

- Batch processing pipelines
- HPC or containerised workflows
- Teaching and documentation
- Cross-platform projects
- Debugging GDAL behaviour differences
- Long-term reproducibility requirements

If you already rely heavily on the GDAL CLI, this approach removes
ambiguity without adding conceptual overhead.

------------------------------------------------------------------------

## Summary

The GDAL CLI context in `link2GI` provides:

- Explicit control over GDAL binaries
- Minimal but reliable skeleton generation
- Deterministic execution
- Optional, structured logging
- Zero interference with existing R geospatial packages

It is intentionally small, honest, and system-centric.
