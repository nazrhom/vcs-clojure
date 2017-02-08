#!/usr/bin/env coffee
require 'shelljs/global'
fs = require 'fs'
_ = require 'lodash'
async = require 'async'

if not which 'git'
  echo 'Sorry, this script requires git'
  exit 1

cd(__dirname)

repos = _.take(JSON.parse(fs.readFileSync('./repos.json')), 10)

cd("../test/repos")

async.eachSeries repos, (repo, cb) ->
  if not test('-d', repo.name)
    exec('git clone ' + repo.url)
  else
    cd(repo.name)
    exec('git pull')
    cd('..')
  cb()
, (err) ->
  console.log('Done')
