#!/usr/bin/env python3

from flask import Flask, jsonify, request
app = Flask(__name__)

@app.route('/')
def root():
    return 'Hey'

@app.route('/api/v2/pipeline')
def pipelines():
    org_slug = request.args.get('org-slug', None)
    if org_slug is None:
        return jsonify(message='You must provide an org-slug.')
    page_token = request.args.get('page-token', None)
    print(page_token)
    if page_token == 'abc':
        return jsonify(next_page_token=None,
                       pipelines=[{'id': 'baar'}])
    if page_token:
        return jsonify(message='Invalid pagination token'), 400
    return jsonify(next_page_token='abc',
                   pipelines=[{'id': 'fooo'}])

@app.route('/api/v2/project/<vcs>/<owner>/<repo>')
def project(vcs, owner, repo):
    return jsonify(project_slug='/'.join([vcs, owner, repo]))

app.run()
