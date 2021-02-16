Welcome to your very own Domino quick-start project! This project has examples for running batch files, iterating using notebooks, publishing models as APIs, and publishing Python/Flask and R/Shiny web applications.

_Before you get started, you might want to check out [our product tour video](https://www.dominodatalab.com/resource/demo_video)._

### Run Batch Files
Domino makes it easy to run a file written in your language of choice. By default, it will run on the default hardware tier but this can be easily changed from the "Settings" link on the left.

In the `main.*` files, you will find code samples in Python, R, and Matlab to write to stdout, chart a histogram with random numbers, and [write key run statistics to dominostats.json](http://support.dominodatalab.com/hc/en-us/articles/204348169).

Try running one of these files twice and use the [compare run functionality](http://support.dominodatalab.com/hc/en-us/articles/204348199-Run-Comparison) to see how the output, charts, and key statistics changed!

### Iterate with Notebooks
Notebooks are a popular way to do and share quantitative research. Domino lets you spin up a notebook server with one click from the "Notebooks" dropdown on the "Runs" page.

`main.ipynb` is a sample Jupyter notebook with code similar to `main.py`. Take note how the notebook lets you intermingle Markdown, input, and output code all in one document. When browsing files from the "Files" page, Domino will render your notebook as you'd expect and you can even schedule your notebook to run and email you using [scheduled runs](http://support.dominodatalab.com/hc/en-us/articles/204843165-Scheduling-Runs).

### Publish a Model as an API
Domino makes it easy to [publish your R or Python model as a web service](http://support.dominodatalab.com/hc/en-us/articles/204173149-API-Endpoints-Model-Deployment) too.

`model.py` and `model.R` have `my_model` functions. By simply specifying the file and function name on the "Publish" page, you can have a REST API for your model set up in no time.

### Publish an App
You can also use Domino to [deploy a web app](http://support.dominodatalab.com/hc/en-us/articles/209150326-Getting-Started-with-App-publishing) to publish dashboards and enable broader data and information sharing.

`app.R` is an interactive R/Shiny example and `app.py` is a Python/Flask example of this functionality. `app.sh` is where you tell Domino which file contains your web app. Like API endpoints, App publishing is done from the "Publish" page of your project.

### Knowledge Management and Sharing
Right now, you're reading the `README.md` file which is a great place to explain what your project is all about.  We suggest every project have a README file -- it helps others onboard onto your project quickly and helps you remember what the point of your project was in the first place :)

README files get shown on a project's overview page and, like all files, get automatically versioned for your convenience and time traveling needs. You can even embed images from the web or your project, for example `benefits.png`:

![Benefits of Domino](raw/latest/benefits.png?inline=true "Benefits of Domino")

Want to share this project with others? Domino makes it easy for multiple people to collaborate on a project, or to share your projects publicly. [Learn more about our sharing and collaboration features here](http://support.dominodatalab.com/hc/en-us/articles/205032985-Sharing-and-Collaborating).

_____

**Need some extra help?**

* Simply email support@dominodatalab.com or click the blue circular help icon to ask for help without leaving Domino.
* We've also got a ton of great content on [support.dominodatalab.com](https://support.dominodatalab.com)
