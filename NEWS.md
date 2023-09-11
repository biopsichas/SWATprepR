# SWATprepR 0.1.4

* `load_swat_weather2()` function was added to provide quicker option for weather file loading. More 20 times faster comparing to `load_swat_weather()` function. 

# SWATprepR 0.1.3

* `load_template()` function extended to load point source data template. 
* `prepare_ps()` function added to write point source SWAT+ model input files.

# SWATprepR 0.1.2

* `transform_to_list()` function made internal and integrated into `interpolate()` function.
* `load_climate_lst()` renamed to `load_netcdf_weather()`.
* A small correction in `extract_rotation()`.
* Updated *Land use*, *Climate projections* and *Weather data* pages.

# SWATprepR 0.1.1

* A new function `load_swat_weather()` added.
* *Climate projections* article page updated. 

# SWATprepR 0.1.0

* A new `prepare_climate()` function was added.

# SWATprepR 0.0.9 

* New functions `get_usersoil_table()`, `load_climate_lst()` and `plot_wgn_comparison()` were added. 

# SWATprepR 0.0.8

* Some bugs fixed in `get_soil_parameters()` and `get_atmo_dep()`.

# SWATprepR 0.0.7

* Fixed *SOL_BD* calculation in `get_soil_parameters()`. Added function `usersoil_to_sol()`. 

# SWATprepR 0.0.6

* Updated functions `interpolate()`, `prepare_wgn()`, `add_weather()` and `load_template()` to deal with different data. Added new function `fill_with_closest()`. Bug fixes. 

# SWATprepR 0.0.5

* Different bugs in various functions corrected. Plotting functions updated. Added function `add_missing_pcp_zero()`.

# SWATprepR 0.0.4

* Land use data functions added `get_lu_points()` and `extract_rotation()` as well as *Land use* article page to explain how to use them.

# SWATprepR 0.0.3

* Atmospheric deposition data functions added `get_atmo_dep()`, `write_atmo_cli()` and `add_atmo_dep()` as well as *Atmospheric deposition* article page to explain how to use them.

# SWATprepR 0.0.2

* `add_weather()`, `clean_wq()`, `clean_outliers()` and `get_hsg()` functions were added. 

# SWATprepR 0.0.1

* This is the first release of package SWATprepR.
