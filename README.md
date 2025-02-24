# SYSMIOME Toolbox

SYSMIOME is an integrated toolbox for microbiome data analysis designed for clinical research. It provides a comprehensive workflow for processing, exploring, and visualizing microbiome datasets while ensuring data security through a Docker-based local implementation.

## Requirements & Dependencies

- **System Requirements:**  
  - Linux environment  
  - R 
  - Docker (for local deployment)

- **Data Requirements:**  
  - OTU abundance tables  
  - Corresponding sample metadata

## Implementation

### Visualization Modules

- **Alpha Diversity visualization**  
- **Beta Diversity visualization**  
- **Taxonomy visualization**  
- **Correlation analyis**

### Deployment with docker

SYSMIOME offers a [Docker image](https://hub.docker.com/r/yumyai/sysmiome-serve) for local deployment, ensuring that analyses are conducted within a secure and controlled environment—a critical feature for clinical projects handling sensitive data.

``` bash
wget --content-disposition https://osf.io/gfhx2/download && tar -xzf sysmiome_data.tar.gz
docker run --rm -p 6644:3838 --user shiny --group-add $(id -g) --mount type=bind,source="$(pwd)"/sysmiome_dta,target=/sysmiome yumyai/sysmiome-serve:0.0.7-dev
```
The local installation should be http://localhost:6644/.

## Demo
You can view a live demo [here](https://demo.techumya.net/)

## License

This project is licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).

## Contact
For questions or support, please contact yumyai@gmail.com or open an issue on the GitHub repository.
