;;; linky-tests.el --- tests for linky


(ert-deftest linky/store->html ()
  (should
   (string-match-p
    "<a href=\"http://172.30.1.11:8018\">ecom</a>"
    (let ((linky/store (make-hash-table :test 'equal)))
      (puthash "nic"
               '(("http://172.30.1.11:8018" "ecom")
                 ("http://172.30.1.11:8015" "gnudoc"))
               linky/store)
      (linky/store->html "nic")))))

(ert-deftest linky/store-add ()
  (should
   (string-match-p
    "<a href=\"http://172.30.1.8090\">skinny</a>"
    (let ((linky/store (make-hash-table :test 'equal)))
      (puthash "nic"
               '(("http://172.30.1.11:8018" "ecom")
                 ("http://172.30.1.11:8015" "gnudoc"))
               linky/store)
      (linky/store-add "nic" "http://172.30.1.8090" "skinny")
      (linky/store->html "nic")))))

(provide 'linky-tests)

;;; linky-tests.el ends here
