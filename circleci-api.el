;;; circleci-api.el --- Bindings for the CircleCI API. -*- lexical-binding: t -*-

;; Author: Robin Schroer
;; Maintainer: Robin Schroer
;; Version: 0.1
;; Homepage: https://github.com/sulami/circleci-api
;; Package-Requires: ((emacs "25.1") (request "0.3.2"))


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Bindings for the CircleCI API.

;;; Code:

(require 'cl-macs)
(require 'request)

;; Customisation:

(defgroup circleci nil
  "Bindings for the CircleCI API.")

(defcustom circleci-api-token ""
  "The CircleCI API token."
  :type 'string
  :group 'circleci)

(defcustom circleci-api-host "https://circleci.com"
  "The CircleCI API host."
  :type 'string
  :group 'circleci)

;; Routes:

(defun circleci--route--api-root ()
  "Return the API root."
  (concat circleci-api-host "/api"))

(defun circleci--route--api-v2 ()
  "Return the APIv2 root."
  (concat (circleci--route--api-root) "/v2"))

(defun circleci--route--pipeline ()
  "Return the API route for pipelines."
  (concat (circleci--route--api-v2) "/pipeline"))

(defun circleci--route--project (project-slug)
  "Return the API route for the project at PROJECT-SLUG."
  (concat (circleci--route--api-v2) "/project/" project-slug))

(defun circleci--route--project-pipelines (project-slug)
  "Return the API route for pipelines of the project at PROJECT-SLUG."
  (concat (circleci--route--project project-slug)
          "/pipeline"))

(defun circleci--route--pipeline-by-id (pipeline-id)
  "Return the API route for the pipeline with PIPELINE-ID."
  (concat (circleci--route--pipeline) "/" pipeline-id))

(defun circleci--route--pipeline-config (pipeline-id)
  "Return the API route for the config of the pipeline with PIPELINE-ID."
  (concat (circleci--route--pipeline-by-id pipeline-id)
          "/config"))

(defun circleci--route--pipeline-workflows (pipeline-id)
  "Return the API route for the workflows of the pipeline with PIPELINE-ID."
  (concat (circleci--route--pipeline-by-id pipeline-id)
          "/workflow"))

;; Plumbing:

(cl-defun circleci--default-handler (&key symbol-status circleci-responses &allow-other-keys)
  "Default RESPONSE handler for CircleCI requests.

Currently just prints some info.

SYMBOL-STATUS is provided by request.

CIRCLECI-RESPONSES is filled with a list of responses if the request
was paginated."
  (message "CircleCI request done: %s" symbol-status)
  (when circleci-responses
    (message "Ran %s requests" (length circleci-responses))))

(cl-defun circleci-run-request (route &key
                                      (method "GET")
                                      (token circleci-api-token)
                                      (page-token nil)
                                      (params nil)
                                      (handler #'circleci--default-handler)
                                      (sync nil))
  "Run the request at ROUTE with authN.

Returns data parsed from JSON.

METHOD is a string, defaulting to \"GET\".

TOKEN is the CircleCI API token, defaulting to the value of
`circleci-api-token'.

PAGE-TOKEN is the optional pagination token for list endpoints.

PARAMS is appended to the HTTP query parameters.

HANDLER is the handler function to run on success, defaulting to
`circleci--default-handler'.

If SYNC is non-nil, this request is run synchronously."
  (request
    route
    :params (cl-concatenate
             'list
             (when params params)
             (when page-token (list (cons "page-token" page-token))))
    :type method
    :headers (list (cons "Circle-Token" token))
    :parser 'json-read
    :complete handler
    :sync sync))

(cl-defun circleci--pagination-handler (&key
                                        data
                                        error-thrown
                                        symbol-status
                                        response
                                        circleci-route
                                        circleci-args
                                        circleci-handler
                                        circleci-pages
                                        circleci-responses
                                        &allow-other-keys)
  "Response handler function for pagination.

DATA, ERROR-THROWN, SYMBOL-STATUS, and RESPONSE are request-provided
fields.

CIRCLECI-ROUTE and CIRCLECI-ARGS are passed through the handler to be
used in subsequent calls to `circleci-run-request'.

CIRCLECI-HANDLER is as well, but only used once the end has been
reached, either by reaching the caller-define limit, or running out of
pages.

CIRCLECI-PAGES and CIRCLECI-RESPONSES are accumulation variables
passed through here to count and aggregate responses."
  (let ((page-token (alist-get 'next_page_token data)))
    (if (and (not (equal 1 circleci-pages))
             page-token)
        ;; But wait, there's more.
        (apply #'circleci-run-request
               circleci-route
               :page-token page-token
               :handler (lambda (&rest args)
                          (apply #'circleci--pagination-handler
                                 :circleci-route circleci-route
                                 :circleci-args circleci-args
                                 :circleci-handler circleci-handler
                                 :circleci-pages (- circleci-pages 1)
                                 :circleci-responses (cons response circleci-responses)
                                 args))
               :allow-other-keys t
               circleci-args)
      ;; Reached the end, apply original handler.
      ;; TODO merge previous responses?.
      (apply circleci-handler
             :data data
             :error-thrown error-thrown
             :status-symbol symbol-status
             :response response
             :circleci-responses (cons response circleci-responses)
             '()))))

(cl-defun circleci-run-paginated-request (route &rest args
                                                &key
                                                (handler #'circleci--default-handler)
                                                (pages 1)
                                                &allow-other-keys)
  "Run a request on ROUTE and keep paginating for PAGES pages.

Use 0 for for PAGES to keep paginating until the end, if you dare.

ARGS is passed to `circleci-run-request'.

HANDLER is the handler function to run on the final result. Signature
TBD."
  (apply #'circleci-run-request
         route
         :handler (lambda (&rest handler-args)
                    (apply #'circleci--pagination-handler
                           :circleci-route route
                           :circleci-args args
                           :circleci-pages pages
                           :circleci-handler handler
                           handler-args))
         :allow-other-keys t
         args))

;; External interface:

(defun circleci-org-slug (vcs owner)
  "Construct the org slug VCS/OWNER."
  (concat vcs "/" owner))

(defun circleci-project-slug (vcs owner repo)
  "Construct the project slug VCS/OWNER/REPO."
  (concat vcs "/" owner "/" repo))

(cl-defun circleci-get-pipelines (org-slug &rest args
                                           &key
                                           (mine nil)
                                           &allow-other-keys)
  "Get recent pipelines for the org with ORG-SLUG.

If MINE is non-nil, only returns pipelines for the authenticated user.

ARGS is passed to `circleci-run-request'.

Supply PAGES as a keyword argument to fetch several pages. See
`circleci-run-paginated-request' for more info."
  (apply
   #'circleci-run-paginated-request
   (circleci--route--pipeline)
   :params (cl-concatenate
            'list
            (list (cons "org-slug" org-slug))
            (when mine (list (cons "mine" mine))))
   args))

(cl-defun circleci-get-project (project-slug &rest args &allow-other-keys)
  "Get the project with PROJECT-SLUG.

ARGS is passed to `circleci-run-request'."
  (apply
   #'circleci-run-request
   (circleci--route--project project-slug)
   args))

(cl-defun circleci-get-pipeline (pipeline-id &rest args &allow-other-keys)
  "Get a pipeline by PIPELINE-ID.

ARGS is passed to `circleci-run-request'."
  (apply
   #'circleci-run-request
   (circleci--route--pipeline-by-id pipeline-id)
   args))

(cl-defun circleci-get-pipeline-config (pipeline-id &rest args &allow-other-keys)
  "Get the config for the pipeline with PIPELINE-ID.

ARGS is passed to `circleci-run-request'."
  (apply
   #'circleci-run-request
   (circleci--route--pipeline-config pipeline-id)
   args))

(cl-defun circleci-get-pipeline-workflows (pipeline-id &rest args &allow-other-keys)
  "Get the workflows for the pipeline with PIPELINE-ID.

ARGS is passed to `circleci-run-paginated-request'.

Supply PAGES as a keyword argument to fetch several pages. See
`circleci-run-paginated-request' for more info."
  (apply
   #'circleci-run-paginated-request
   (circleci--route--pipeline-workflows pipeline-id)
   args))

(cl-defun circleci-get-project-pipelines (project-slug &rest args &allow-other-keys)
  "Get the pipelines for the project with PROJECT-SLUG.

ARGS is passed to `circleci-run-paginated-request'.

Supply PAGES as a keyword argument to fetch several pages. See
`circleci-run-paginated-request' for more info."
  (apply
   #'circleci-run-paginated-request
   (circleci--route--project-pipelines project-slug)
   args))

(provide 'circleci-api)

;;; circleci-api.el ends here
