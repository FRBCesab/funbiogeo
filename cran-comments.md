## Test environments

* Local
  * Fedora Linux 7.0.14-201.fc44.x86_64 (R 4.6.0)
  * Debian 13 Linux 6.12.74+deb13+1-amd64 (R 4.6.0)
  * Windows 11 26200 (R 4.4.2)
  
* GitHub Actions
  * macOS 15.7.7 24G720, R-release (R 4.6.0)
  * Windows Server 2025 10.0.26100, R-release (R 4.6.0)
  * Ubuntu 24.04.4 LTS, R-devel, R-release (R 4.6.0), R-oldrel


## R CMD check results

```
0 errors | 0 warnings | 1 note
```

* NOTES
  * New submission for Nicolas Casajus <nicolas.casajus@fondationbiodiversite.fr>


## Downstream dependencies

There are currently no downstream dependencies for this package.


## Resubmission comments

Hi,

Thanks for reporting this error:

```
Found the following (possibly) invalid file URI:
  URI: vignettes/upscaling.Rmd
    From: inst/doc/special_cases.html
```

This new submission fixes this issue.

Best,
Nicolas
