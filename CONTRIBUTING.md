Contributing to CloudFlux
Thank you for your interest in contributing to CloudFlux! This project is currently part of an active research initiative.

How to Contribute
1. Reporting Bugs
If you find a bug (e.g., a specific LiDAR format failing to load or a UI crash), please open an Issue on GitHub. Include:

A brief description of the problem.

A reproducible example if possible.

Your operating system and GPU specs (for ICP-related issues).

2. Feature Requests
We welcome ideas for new LiDAR processing workflows! Please open an issue and tag it as a feature request.

3. Pull Requests
We are open to community contributions! If you would like to submit code:

Fork the repository.

Create a new branch for your feature or fix.

Ensure your code follows the golem package structure.

If you are modifying the CFCore backend, please ensure R6 class methods remain compatible with the CloudFlux UI.

Submit a Pull Request with a clear description of your changes.

Development Setup
To modify CloudFlux, you will need:

RStudio

Roxygen2 for documentation.

Conda/Miniconda for Python dependency management.

R Code Style
We generally follow the tidyverse style guide. Please use meaningful variable names and document all new functions using Roxygen tags.

Academic Attribution
CloudFlux is developed as part of a doctoral thesis. If you use this software in your research, please cite the associated manuscript (citation details available in the README).
