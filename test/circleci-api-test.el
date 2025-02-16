;;; circleci-api-test.el --- tests for circleci-api.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for circleci-api.

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 'ert)
(require 'circleci-api)

(defun anth (idx array)
  "Reversed version of `aref' for threading."
  (aref array idx))

(ert-deftest circleci-api-test/sanity-test ()
  (should (equal 1 1)))

(ert-deftest circleci-api-test/test-project-slug ()
  (should (equal "gh/sulami/circleci-api"
                 (circleci-api-project-slug "gh" "sulami" "circleci-api"))))

(cl-defmacro circleci-api-test/with-test-host (&body body)
  "Run BODY against the local test API.

Attempts to bring up the test server using nix-shell, run the test
with the appropriate bindings, and kill the server."
  `(let ((_ (get-buffer-create "*circleci-test-server*"))
         (_ (with-current-buffer "*circleci-test-server*" (erase-buffer)))
         (host-process (if (getenv "CI")
                           (start-process "circleci-test-server"
                                          "*circleci-test-server*"
                                          (executable-find "python3")
                                          (expand-file-name "test/test_server.py"))
                         (start-process "circleci-test-server"
                                        "*circleci-test-server*"
                                        (executable-find "nix-shell")
                                        "-p"
                                        "pythonPackages.flask"
                                        "--command"
                                        (concat "python " (expand-file-name "test_server.py")))))
         (circleci-api-host "http://localhost:5000")
         (circleci-api-token "test-token"))
     (unwind-protect
         (progn
           (cl-loop with buffer = (process-buffer host-process)
                    repeat 30
                    do (accept-process-output host-process 0.1 nil t)
                    for str = (with-current-buffer buffer (buffer-string))
                    do (cond
                        ((string-match "Running on" str)
                         (cl-return str))
                        ((not (eq 'run (process-status host-process)))
                         (error "Test server startup failure")))
                    finally do (error "Test server startup crash"))
           ,@body)
       (kill-process host-process))))

(ert-deftest circleci-api-test/test-test-host ()
  (circleci-api-test/with-test-host
   (request
     circleci-api-host
     :sync t
     :parser #'buffer-string
     :complete (cl-function
                (lambda (&key data &allow-other-keys)
                  (should (equal "Hey" data)))))))

(ert-deftest circleci-api-test/test-pipeline-list ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipelines
    (circleci-api-org-slug "gh" "sulami")
    :sync t
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 1 (length circleci-responses)))
                (should (equal "fooo"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-paginated-pipeline-list ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipelines
    (circleci-api-org-slug "gh" "sulami")
    :sync t
    :pages 2
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 2 (length circleci-responses)))
                (should (equal "baar"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-paginated-pipeline-list-running-out ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipelines
    (circleci-api-org-slug "gh" "sulami")
    :sync t
    :pages 3
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 2 (length circleci-responses)))
                (should (equal "baar"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-my-pipelines ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipelines
    (circleci-api-org-slug "gh" "sulami")
    :sync t
    :mine t
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 1 (length circleci-responses)))
                (should (equal "quux"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-project ()
  (circleci-api-test/with-test-host
   (circleci-api-get-project
    (circleci-api-project-slug "gh" "sulami" "circleci-api")
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "gh/sulami/circleci-api"
                               (alist-get 'project_slug data))))))))

(ert-deftest circleci-api-test/test-pipeline-by-id ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipeline
    "123"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "123"
                               (alist-get 'id data))))))))

(ert-deftest circleci-api-test/test-pipeline-config ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipeline-config
    "123"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "source"
                               (alist-get 'source data)))
                (should (equal "compiled"
                               (alist-get 'compiled data))))))))

(ert-deftest circleci-api-test/test-pipeline-workflows ()
  (circleci-api-test/with-test-host
   (circleci-api-get-pipeline-workflows
    "123"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "fooo"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-project-pipelines ()
  (circleci-api-test/with-test-host
   (circleci-api-get-project-pipelines
    (circleci-api-project-slug "gh" "sulami" "circleci-api")
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "fooo"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-my-project-pipelines ()
  (circleci-api-test/with-test-host
   (circleci-api-get-my-project-pipelines
    (circleci-api-project-slug "gh" "sulami" "circleci-api")
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "quux"
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-workflow-by-id ()
  (circleci-api-test/with-test-host
   (circleci-api-get-workflow
    "wonf"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "wonf"
                               (alist-get 'id data))))))))

(ert-deftest circleci-api-test/test-workflow-jobs ()
  (circleci-api-test/with-test-host
   (circleci-api-get-workflow-jobs
    "wonf"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal 1
                               (->> data
                                    (alist-get 'items)
                                    (anth 0)
                                    (alist-get 'number)))))))))

(ert-deftest circleci-api-test/test-trigger-pipeline-branch ()
  (circleci-api-test/with-test-host
   (circleci-api-trigger-pipeline
    (circleci-api-project-slug "gh" "sulami" "circleci-api")
    :branch "master"
    :pipeline-parameters '((foo . bar))
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "master"
                               (alist-get 'branch data)))
                (should (equal "bar"
                               (->> data
                                    (alist-get 'parameters)
                                    (alist-get 'foo)))))))))

(ert-deftest circleci-api-test/test-trigger-pipeline-tag ()
  (circleci-api-test/with-test-host
   (circleci-api-trigger-pipeline
    (circleci-api-project-slug "gh" "sulami" "circleci-api")
    :tag "v1.2"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "v1.2"
                               (alist-get 'tag data))))))))

(ert-deftest circleci-api-test/test-trigger-pipeline-both ()
  (should-error
   (circleci-api-trigger-pipeline
    (circleci-api-project-slug "gh" "sulami" "circleci-api")
    :branch "master"
    :tag "v1.2"
    :pipeline-parameters '((foo . bar))
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "v1.2"
                               (alist-get 'tag data)))
                (should (equal "bar"
                               (->> data
                                    (alist-get 'parameters)
                                    (alist-get 'foo)))))))))

(ert-deftest circleci-api-test/test-cancel-workflow ()
  (circleci-api-test/with-test-host
   (circleci-api-cancel-workflow
    "workflow-id"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "workflow-id"
                               (alist-get 'id data))))))))

(ert-deftest circleci-api-test/test-rerun-workflow ()
  (circleci-api-test/with-test-host
   (circleci-api-rerun-workflow
    "workflow-id"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "workflow-id"
                               (alist-get 'id data)))
                (should (equal :json-false
                               (alist-get 'from_failed data))))))))

(ert-deftest circleci-api-test/test-rerun-workflow-from-failed ()
  (circleci-api-test/with-test-host
   (circleci-api-rerun-workflow
    "workflow-id"
    :from-failed t
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "workflow-id"
                               (alist-get 'id data)))
                (should (alist-get 'from_failed data)))))))

(ert-deftest circleci-api-test/test-approve-job ()
  (circleci-api-test/with-test-host
   (circleci-api-approve-job
    "workflow-id"
    "job-id"
    :sync t
    :handler (cl-function
              (lambda (&key data &allow-other-keys)
                (should (equal "workflow-id"
                               (alist-get 'workflow_id data)))
                (should (equal "job-id"
                               (alist-get 'job_id data))))))))

(provide 'circleci-api-test)

;;; circleci-api-test.el ends here
