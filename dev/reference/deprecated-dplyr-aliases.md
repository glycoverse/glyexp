# Deprecated dplyr-style aliases

**\[deprecated\]**

The `_obs()` and `_var()` helpers have been renamed to `_col()` and
`_row()`, respectively. Use the new names in new code.

## Usage

``` r
arrange_obs(...)

arrange_var(...)

filter_obs(...)

filter_var(...)

mutate_obs(...)

mutate_var(...)

rename_obs(...)

rename_var(...)

select_obs(...)

select_var(...)

slice_obs(...)

slice_var(...)

slice_head_obs(...)

slice_head_var(...)

slice_tail_obs(...)

slice_tail_var(...)

slice_sample_obs(...)

slice_sample_var(...)

slice_max_obs(...)

slice_max_var(...)

slice_min_obs(...)

slice_min_var(...)

left_join_obs(...)

left_join_var(...)

inner_join_obs(...)

inner_join_var(...)

semi_join_obs(...)

semi_join_var(...)

anti_join_obs(...)

anti_join_var(...)
```
