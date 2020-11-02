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
(require 'ert)
(require 'circleci-api)

(ert-deftest circleci-api-test/sanity-test ()
  (should (equal 1 1)))

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
         (circleci-api-host "http://localhost:5000"))
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

(provide 'circleci-api-test)

;;; circleci-api-test.el ends here
