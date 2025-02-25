# SYSMIOME Toolbox

SYSMIOME is an integrated toolbox for microbiome data analysis designed for clinical research. It provides a comprehensive workflow for processing, exploring, and visualizing microbiome datasets while ensuring data security through a Docker-based local implementation.

## 1. Requirements & Dependencies

- **System Requirements:**  
  - Linux environment  
  - R 
  - Docker (for local deployment)

- **Data Requirements:**  
  - OTU abundance tables  
  - Corresponding sample metadata

## 2. Demo
You can view a live demo [here](https://demo.techumya.net/)

### Visualization Modules

- **Alpha Diversity visualization**  
- **Beta Diversity visualization**  
- **Taxonomy visualization**  
- **Correlation analyis**

### 3. Deployment with docker

SYSMIOME offers a [Docker image](https://hub.docker.com/r/yumyai/sysmiome-serve) for local deployment, ensuring that analyses are conducted within a secure and controlled environment—a critical feature for clinical projects handling sensitive data. Note: The data downloaded via the wget command includes the supplementary datasets used to illustrate the toolbox in two prospective clinical studies.

``` bash
# Download and extract the data first (if not already done)
wget --content-disposition https://osf.io/gfhx2/download && tar -xzf sysmiome_data.tar.gz && rm sysmiome_data.tar.gz

# Copy files from local extracted folder into the volume
docker run --rm \
	-v sysmiome_data:/data \
	-v "$(pwd)/sysmiome_data":/from_host \
	alpine:3.21.3 cp -r /from_host/. /data/ &&
	rm -r ./sysmiome_data

docker run --rm \
  -v sysmiome_data:/data \
  alpine:3.21.3 sh -c "chmod -R a+rwX /data"

docker run --rm -p 6644:3838 \
	--user shiny \
	--group-add $(id -g) \
	-v sysmiome_data:/sysmiome \
	yumyai/sysmiome-serve:0.0.7-dev
```

The local installation should be http://localhost:6644/.

## License

This project is licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).

## Contact
For questions or support, please contact yumyai@gmail.com or open an issue on the GitHub repository.
