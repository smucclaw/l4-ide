# Non-Working @export Examples

This directory contains L4 files demonstrating @export annotation placements that **do not work**.

These are kept as documentation to show what syntax is not supported and to prevent regressions if the parser/export logic changes in the future.

## Common Issues

- @export must appear **before** GIVEN, not between GIVETH and DECIDE
- @export placement with MEANS may have different requirements than DECIDE
