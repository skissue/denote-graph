;;; denote-graph.el --- Graph UI for Denote -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (denote "3"))
;; Homepage: https://github.com/skissue/denote-graph


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Graph UI for Denote, reminiscent of `org-roam-ui' (WIP)

;;; Code:

(require 'denote)

(defgroup denote-graph nil
  "Graph UI for Denote."
  :group 'tools)

(defcustom denote-graph-include-file-predicate #'always
  "Predicate for if a node should be included in the exported graph data.
The predicate will receive the file name and should return non-nil if
the node should be included."
  :type 'function)

(defun denote-graph--get-filtered-ids ()
  "Return a hashset of IDs passing `denote-graph-include-file-predicate'."
  (let ((ids (make-hash-table :test #'equal))
        (files (denote-directory-files)))
    (dolist (file files)
      (when-let* ((id (denote-retrieve-filename-identifier file))
                  ((funcall denote-graph-include-file-predicate id)))
        (puthash id t ids)))
    ids))

(defun denote-graph--get-filtered-files (ids)
  "Return list of files to consider for graph.
IDS should be a hashset of IDs to include, as returned by
`denote-graph--get-filtered-ids'."
  (cl-loop for file in (denote-directory-files)
           for id = (denote-retrieve-filename-identifier file)
           when (gethash id ids)
           collect file))

(defun denote-graph--build-graph-nodes (ids)
  "Build a list of nodes with necessary metadata.
IDS should be a hashset of IDs to include, as returned by
`denote-graph--get-filtered-ids'."
  (cl-loop for file in (denote-graph--get-filtered-files ids)
           for id = (denote-retrieve-filename-identifier file)
           for type = (denote-filetype-heuristics file)
           collect
           `((id . ,id)
             (name . ,(denote-retrieve-title-or-filename file type))
             (tags . ,(denote-retrieve-filename-keywords file)))))

(defun denote-graph--build-graph-links (ids)
  "Build a list of links with necessary metadata.
IDS should be a hashset of IDs to include, as returned by
`denote-graph--get-filtered-ids'."
  ;; Built off of code from https://github.com/pprevos/denote-explore
  (cl-loop for match in (xref-matches-in-files
                         "\\[denote:[0-9]\\{8\\}T[0-9]\\{6\\}\\]"
                         (denote-graph--get-filtered-files ids))
           for source-file = (thread-last
                               match
                               (xref-match-item-location)
                               (xref-location-group))
           for source = (denote-retrieve-filename-identifier source-file)
           for text = (substring-no-properties
                       (xref-match-item-summary match))
           for target = (save-match-data
                          (string-match denote-id-regexp text)
                          (match-string 0 text))
           when (and (gethash source ids)
                     (gethash target ids))
           collect
           `((source . ,source)
             (target . ,target))))

(defun denote-graph--build-graph ()
  "Build a JSON graph of Denote links."
  (let* ((ids (denote-graph--get-filtered-ids))
         (nodes (denote-graph--build-graph-nodes ids))
         (links (denote-graph--build-graph-links ids)))
    (json-encode `((nodes . ,nodes)
                   (links . ,links)))))

(provide 'denote-graph)

;;; denote-graph.el ends here
