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
                 (circleci-project-slug "gh" "sulami" "circleci-api"))))

(cl-defmacro circleci-api-test/with-test-host (&body body)
  "Run BODY against the local test API.

Attempts to bring up the test server using nix-shell, run the test
with the appropriate bindings, and kill the server."
  `(let ((_ (get-buffer-create "*circleci-test-server*"))
         (_ (with-current-buffer "*circleci-test-server*" (erase-buffer)))
         (host-process (start-process "circleci-test-server"
                                       "*circleci-test-server*"
                                       (executable-find "nix-shell")
                                       "-p"
                                       "pythonPackages.flask"
                                       "--command"
                                       (concat "python3 " (expand-file-name "test_server.py"))))
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
   (circleci-get-pipelines
    (circleci-org-slug "gh" "sulami")
    :sync t
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 1 (length circleci-responses)))
                (should (equal "fooo"
                               (->> data
                                    (alist-get 'pipelines)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-paginated-pipeline-list ()
  (circleci-api-test/with-test-host
   (circleci-get-pipelines
    (circleci-org-slug "gh" "sulami")
    :sync t
    :pages 2
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 2 (length circleci-responses)))
                (should (equal "baar"
                               (->> data
                                    (alist-get 'pipelines)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-paginated-pipeline-list-running-out ()
  (circleci-api-test/with-test-host
   (circleci-get-pipelines
    (circleci-org-slug "gh" "sulami")
    :sync t
    :pages 3
    :handler (cl-function
              (lambda (&key error-thrown data circleci-responses &allow-other-keys)
                (should (not error-thrown))
                (should (eq 2 (length circleci-responses)))
                (should (equal "baar"
                               (->> data
                                    (alist-get 'pipelines)
                                    (anth 0)
                                    (alist-get 'id)))))))))

(ert-deftest circleci-api-test/test-project ()
  (circleci-api-test/with-test-host
   (circleci-get-project
    (circleci-project-slug "gh" "sulami" "circleci-api")
    :sync t
    :handler (cl-function
              (lambda (&key response data &allow-other-keys)
                (should (equal '((project_slug . "gh/sulami/circleci-api"))
                               data)))))))

(ert-deftest circleci-api-test/test-pipeline-by-id ()
  (circleci-api-test/with-test-host
   (circleci-get-pipeline
    "123"
    :sync t
    :handler (cl-function
              (lambda (&key response data &allow-other-keys)
                (should (equal '((id . "123"))
                               data)))))))


(provide 'circleci-api-test)

;;; circleci-api-test.el ends here
