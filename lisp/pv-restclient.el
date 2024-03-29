;;; pv-restclient.el --- Summary

;;; Commentary:
;;; Setup restclient
;;; Code:

;;; restclient
(use-package restclient
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	      ("C-c C-f" . json-mode-beautify)))

(defun restclient-get-header-from-response (header)
  "Get HEADER from the response buffer of restclient.
HEADER should be just the name of the header, e.g.
  \"content-type\" (it is case insensitive)."
  (let* ((case-fold-search t)
         (search-string (format "// %s: " header))
         (match (string-match search-string
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
    (goto-char match)
    (forward-char (length search-string))
    (buffer-substring-no-properties (point)
                                    (progn
                                      (move-end-of-line 1)
                                      (point)))))

(provide 'pv-restclient)

;;; pv-restclient.el ends here
