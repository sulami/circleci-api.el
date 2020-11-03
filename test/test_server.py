#!/usr/bin/env python3

from flask import Flask, jsonify
app = Flask(__name__)

@app.route('/')
def root():
    return 'Hey'

@app.route('/api/v2/pipeline')
def pipelines():
    return jsonify(id='fooo')

@app.route('/api/v2/project/<vcs>/<owner>/<repo>')
def project(vcs, owner, repo):
    return jsonify(project_slug='/'.join([vcs, owner, repo]))

app.run()
