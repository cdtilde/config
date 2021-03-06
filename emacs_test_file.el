/**
* Copyright (c) 2015-present, Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD-style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/
'use strict';

var transformSource = require('./jestSupport/scriptPreprocess.js').transformSource;
var linterTransform = require('./lint/linterTransform');

linterTransform.setLinterTransform(transformSource);

// Run the original CLI
require('eslint/bin/eslint');

(defun lispbox-file (relative-pathname)
  (merge-pathnames
   relative-pathname
   (make-pathname
    :directory (butlast (pathname-directory *load-pathname*))
    :name nil
    :type nil
    :defaults *load-pathname*)))




protected String getUserAgent() {
return "Java/" + VERSION;
    }


this is a string "inside double quotes";
this is a string 'inside single quotes';

