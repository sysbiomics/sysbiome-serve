docker run --rm -p 6644:3838 --user shiny --group-add $(id -g) --mount type=bind,source="$(pwd)"/ext_data,target=/sysmiome yumyai/sysmiome-serve:0.0.1-dev
