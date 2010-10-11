(defun my-python-test-template (name)
  (interactive "BName of buffer to add python test template to: ")
  (let* ((testname (concat (car (split-string name "\\.")) "TestCase"))
        (testnamesuite (concat testname "Suite")))
    (with-current-buffer (get-buffer-create name)
      (message (concat "Inserting python test template in buffer: " name))
      (python-mode)
      (insert
       (concat
        "import unittest\n\n"
        "class " testname
        "(unittest.TestCase):\n\n"
        "    def setUp(self):\n        pass\n\n"
        "    def tearDown(self):\n        pass\n\n"
        "    def test_CHANGE(self):\n        pass\n\n"
        "def " testnamesuite "():\n"
        "    return unittest.TestLoader().loadTestsFromTestCase(" testname ")\n\n"
        "if __name__ == '__main__':\n"
        "    suite = " testnamesuite "()\n"
        "    unittest.TextTestRunner(verbosity=2).run(suite)"))
      )))

