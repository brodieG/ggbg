## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:


## R CMD check --as-cran

Status: OK

### Test Environments

* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-03-30 r74497)
    * R version 3.4.4 (2017-01-27)
    * R version 3.2.5 (2017-01-27)
* Winbuilder
    * R version 3.4.4 (2017-01-27)
    * R version 3.5.0 alpha (2018-03-27 2018-03-30 r74498)
* r-debug docker container, level 2 valgrind
    * R devel (2018-03-16 r74417)
* Locally on Mac OS 10.12.6
    * R version 3.4.1 (2017-06-30)
* rhub i386-pc-solaris2.10 (32-bit):
    * R version 3.4.1 Patched (2017-07-15 r72924)


