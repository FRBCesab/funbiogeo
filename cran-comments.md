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

* Note

Maintainer: 'Nicolas Casajus <nicolas.casajus@fondationbiodiversite.fr>'

New submission

Possibly misspelled words in DESCRIPTION:
  Biogeography (3:32)
  biogeography (16:54)
  upscaling (19:22)


## Downstream dependencies

There are currently no downstream dependencies for this package.


## Resubmission comments

Hi,

This resubmission fixes these two notes:

- Invalid URI

```
Found the following (possibly) invalid file URI:
  URI: vignettes/upscaling.Rmd
    From: inst/doc/special_cases.html
```

- Checktime > 10 min

All vignettes have been removed from the build but are still available on the 
package website. A new vignette is shipped with the package and provides links
to the official documentation.


Best,
Nicolas
