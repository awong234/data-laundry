#!/bin/bash

# Run through all the work and make sure no errors appear. Updated to support
# per-project R versions with `rig`. Each directory has a .r-version file
# (similar to uv .python-version file) indicating the version of R to use.
# This is passed to rig to initialize with the right version.

function run {
    rig run -r "$(cat .r-version)" -f $1 > log.txt 2>&1
}

function report {
    if [ $? -ne 0 ]; then
        echo "Error in $(pwd)"
        cat log.txt
    else
        date > .testok
    fi
}

pushd datasets/202403-cuyahoga-county-voter-registration-locations
run sample_registration_locations.R
report
popd

pushd datasets/202404-tsa-passenger-volume
run clean_tsa.R
report
popd

pushd datasets/202405-fuel-economy
run sample.R
report
popd

pushd datasets/202406-nexrad
run nexrad.R
report
popd

pushd datasets/202410-hurrdat2
run fetch_hurricane_data.R
report
popd

# Needs to be manually inspected because it is a shiny app
pushd datasets/202502-netflix-engagement
run app.R
report
popd

pushd datasets/202504-rpg-dice-rolls/
run script.R
report
popd

pushd datasets/202510-cle-open-data/
run setup.R
run burials.R
report
popd

pushd demos/202512-threed-rotations
run setup.R
run rotate_candy_cane.R
report
popd
