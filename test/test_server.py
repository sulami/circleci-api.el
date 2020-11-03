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
    mine = request.args.get('mine', None)
    if mine:
        return jsonify(next_page_token=None,
                       items=[{'id': 'quux'}])
    page_token = request.args.get('page-token', None)
    if page_token == 'abc':
        return jsonify(next_page_token=None,
                       items=[{'id': 'baar'}])
    if page_token:
        return jsonify(message='Invalid pagination token'), 400
    return jsonify(next_page_token='abc',
                   items=[{'id': 'fooo'}])

@app.route('/api/v2/project/<vcs>/<owner>/<repo>')
def project(vcs, owner, repo):
    return jsonify(project_slug='/'.join([vcs, owner, repo]))

@app.route('/api/v2/pipeline/<pipeline_id>')
def pipeline_by_id(pipeline_id):
    return jsonify(id=pipeline_id)

@app.route('/api/v2/pipeline/<pipeline_id>/config')
def pipeline_config(pipeline_id):
    return jsonify(source='source',
                   compiled='compiled')

@app.route('/api/v2/pipeline/<pipeline_id>/workflow')
def pipeline_workflows(pipeline_id):
    page_token = request.args.get('page-token', None)
    if page_token == 'abc':
        return jsonify(next_page_token=None,
                       items=[{'id': 'baar'}])
    if page_token:
        return jsonify(message='Invalid pagination token'), 400
    return jsonify(next_page_token='abc',
                   items=[{'id': 'fooo'}])

@app.route('/api/v2/project/<vcs>/<owner>/<repo>/pipeline')
def project_pipelines(vcs, owner, repo):
    page_token = request.args.get('page-token', None)
    if page_token == 'abc':
        return jsonify(next_page_token=None,
                       items=[{'id': 'baar'}])
    if page_token:
        return jsonify(message='Invalid pagination token'), 400
    return jsonify(next_page_token='abc',
                   items=[{'id': 'fooo'}])

@app.route('/api/v2/project/<vcs>/<owner>/<repo>/pipeline/mine')
def my_project_pipelines(vcs, owner, repo):
    page_token = request.args.get('page-token', None)
    if page_token == 'abc':
        return jsonify(next_page_token=None,
                       items=[{'id': 'baaz'}])
    if page_token:
        return jsonify(message='Invalid pagination token'), 400
    return jsonify(next_page_token='abc',
                   items=[{'id': 'quux'}])

app.run()
