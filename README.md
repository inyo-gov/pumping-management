# Pumping Management

Welcome to the Pumping Management repository.

## Website

For more information and to view the website, please visit: [Pumping Management Website](https://inyo-gov.github.io/pumping-management/)

## Introduction

The Water Agreement and Green Book established protocols for determining the operational status of LADWP pumping wells, based on soil moisture and vegetation measurements. To fulfill monitoring requirements under the Agreement, the Water Department regularly assesses soil moisture content and vegetation leaf area index (in collaboration with LADWP) at 22 permanent sites within wellfields and seven sites in control areas. Soil moisture monitoring occurs monthly, while vegetation monitoring takes place annually in June. Each monitoring site is associated with one or more LADWP pumping wells.

On July 1st and October 1st, if soil moisture levels are inadequate to support vegetation needs at a site, the linked pumping wells go into off status. They remain offline until soil moisture levels surpass the threshold required by the vegetation at the time the linked wells went into off status. This operational approach is known as the On/Off management strategy.

### Prerequisites

-   R and RStudio
-   Git
-   Git LFS (for handling large files)

### Installing

1.  **Clone the Repository Using RStudio:**

    -   **Open RStudio.**
    -   **Create a New Project:**
        -   Go to the `File` menu and select `New Project...`.
        -   Choose `Version Control`.
        -   Select `Git`.
    -   **Enter Repository URL:**
        -   In the `Repository URL` field, enter the URL of your GitHub repository (e.g., `https://github.com/inyo-gov/pumping-management.git`).
        -   Choose a directory on your local machine to store the repository.
        -   Click `Create Project`.

2.  **Open the Project in RStudio:**

    -   Navigate to the directory where you cloned the repository.
    -   Double-click on the `pumping-management.Rproj` file to open the project in RStudio.

3.  **Install Git LFS and Pull Large Files:**

    If your repository uses Git LFS for handling large files, make sure to install and set it up:

    -   **Install Git LFS:**

        ``` sh
        git lfs install
        ```

    -   **Pull Large Files:**

        ``` sh
        git lfs pull
        ```

## Contributing

To contribute to this project, follow these steps:

1.  **Fork the Repository:**
    -   Go to the GitHub page of the repository and click the `Fork` button at the top right to create a copy of the repository under your own GitHub account.
2.  **Clone Your Fork:**
    -   Open RStudio and create a new project from version control, using the URL of your forked repository.
3.  **Create a New Branch:**
    -   In your local repository, create a new branch to work on. For example:

        ``` sh
        git checkout -b feature-branch
        ```
4.  **Make Your Changes:**
    -   Implement your changes or additions in RStudio.
5.  **Commit and Push Your Changes:**
    -   Once you have made your changes, commit them with a descriptive message:

        ``` sh
        git add .
        git commit -m "Description of the changes"
        git push origin feature-branch
        ```
6.  **Create a Pull Request:**
    -   Go to the original repository on GitHub and you should see an option to create a pull request from your branch. Click on `Compare & pull request`, provide a detailed description of the changes, and submit the pull request.
7.  **Review Process:**
    -   One of the project maintainers will review your pull request. You might be asked to make further changes. Once everything is satisfactory, your changes will be merged into the main branch.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Dependencies

-   `lubridate`
-   `dygraphs`
-   `zoo`
-   `readxl`
-   `tidyverse`
-   `here`
