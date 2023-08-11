open OUnit2

let _ = run_test_tt_main Dual.tests
let _ = run_test_tt_main Quaternion.tests
let _ = run_test_tt_main Dualcomplex.tests
let _ = run_test_tt_main Dualquaternion.tests
