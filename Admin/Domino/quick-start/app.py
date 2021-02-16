# This is a sample Python/Flask app showing Domino's App publishing functionality
#   learn more at http://support.dominodatalab.com/hc/en-us/articles/209150326

import json
import flask
from flask import request, redirect, url_for
import numpy as np

class ReverseProxied(object):
  def __init__(self, app):
      self.app = app
  def __call__(self, environ, start_response):
      script_name = environ.get('HTTP_X_SCRIPT_NAME', '')
      if script_name:
          environ['SCRIPT_NAME'] = script_name
          path_info = environ['PATH_INFO']
          if path_info.startswith(script_name):
              environ['PATH_INFO'] = path_info[len(script_name):]
      return self.app(environ, start_response)

app = flask.Flask(__name__)
app.wsgi_app = ReverseProxied(app.wsgi_app)

# Homepage which uses a template file
@app.route('/')
def index_page():
  return flask.render_template("index.html")
  
# Sample redirect using url_for
@app.route('/redirect_test')
def redirect_test():
  return redirect( url_for('another_page') )

# Sample return string instead of using template file
@app.route('/another_page')
def another_page():
  msg = "You made it with redirect( url_for('another_page') )." + \
        "A call to flask's url_for('index_page') returns " + url_for('index_page') + "."
  return msg

@app.route("/random")
@app.route("/random/<int:n>")
def random(n = 100):
  random_numbers = list(np.random.random(n))
  return json.dumps(random_numbers)
