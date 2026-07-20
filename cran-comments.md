## Test environments

* Local
  * Fedora Linux 7.1.3-201.fc44.x86_64 (R 4.6.0)
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

This resubmission fixes:

- The `\dontrun{}` wrapper

Only the `fb_make_report()` function used this wrapper in the example section. 
It has been replaced by `if (interactive()) {}` as this function requires an 
interaction with the user.

- Write by default in the current directory

The default value of the argument `path` (current directory) of the 
`fb_make_report()` function has been removed. Now the user must provide a path
otherwise the function will throw an error.


Best,
Nicolas
