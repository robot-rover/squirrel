#[macro_export]
macro_rules! test_foreach {
    ($func:tt) => {
        mod squirrel_repo {
            use super::*;
            const CLASS_PATH: &str = "squirrel_repo/class.nut";
            const CLASS_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/class.nut");
            #[test]
            fn test_class() {
                $func(CLASS_PATH, CLASS_CONTENTS);
            }
            const FIBONACCI_PATH: &str = "squirrel_repo/fibonacci.nut";
            const FIBONACCI_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/fibonacci.nut");
            #[test]
            fn test_fibonacci() {
                $func(FIBONACCI_PATH, FIBONACCI_CONTENTS);
            }
            const MATRIX_PATH: &str = "squirrel_repo/matrix.nut";
            const MATRIX_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/matrix.nut");
            #[test]
            fn test_matrix() {
                $func(MATRIX_PATH, MATRIX_CONTENTS);
            }
            const METAMETHODS_PATH: &str = "squirrel_repo/metamethods.nut";
            const METAMETHODS_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/metamethods.nut");
            #[test]
            fn test_metamethods() {
                $func(METAMETHODS_PATH, METAMETHODS_CONTENTS);
            }
            const FLOW_PATH: &str = "squirrel_repo/flow.nut";
            const FLOW_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/flow.nut");
            #[test]
            fn test_flow() {
                $func(FLOW_PATH, FLOW_CONTENTS);
            }
            const LIST_PATH: &str = "squirrel_repo/list.nut";
            const LIST_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/list.nut");
            #[test]
            fn test_list() {
                $func(LIST_PATH, LIST_CONTENTS);
            }
            const GENERATORS_PATH: &str = "squirrel_repo/generators.nut";
            const GENERATORS_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/generators.nut");
            #[test]
            fn test_generators() {
                $func(GENERATORS_PATH, GENERATORS_CONTENTS);
            }
            const TAILSTATE_PATH: &str = "squirrel_repo/tailstate.nut";
            const TAILSTATE_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/tailstate.nut");
            #[test]
            fn test_tailstate() {
                $func(TAILSTATE_PATH, TAILSTATE_CONTENTS);
            }
            const HELLO_PATH: &str = "squirrel_repo/hello.nut";
            const HELLO_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/hello.nut");
            #[test]
            fn test_hello() {
                $func(HELLO_PATH, HELLO_CONTENTS);
            }
            const REGEX_PATH: &str = "squirrel_repo/regex.nut";
            const REGEX_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/regex.nut");
            #[test]
            fn test_regex() {
                $func(REGEX_PATH, REGEX_CONTENTS);
            }
            const ACKERMANN_PATH: &str = "squirrel_repo/ackermann.nut";
            const ACKERMANN_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/ackermann.nut");
            #[test]
            fn test_ackermann() {
                $func(ACKERMANN_PATH, ACKERMANN_CONTENTS);
            }
            const ARRAY_PATH: &str = "squirrel_repo/array.nut";
            const ARRAY_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/array.nut");
            #[test]
            fn test_array() {
                $func(ARRAY_PATH, ARRAY_CONTENTS);
            }
            const DELEGATION_PATH: &str = "squirrel_repo/delegation.nut";
            const DELEGATION_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/delegation.nut");
            #[test]
            fn test_delegation() {
                $func(DELEGATION_PATH, DELEGATION_CONTENTS);
            }
            const LOOPS_PATH: &str = "squirrel_repo/loops.nut";
            const LOOPS_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/loops.nut");
            #[test]
            fn test_loops() {
                $func(LOOPS_PATH, LOOPS_CONTENTS);
            }
            const CLASSATTRIBUTES_PATH: &str = "squirrel_repo/classattributes.nut";
            const CLASSATTRIBUTES_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/classattributes.nut");
            #[test]
            fn test_classattributes() {
                $func(CLASSATTRIBUTES_PATH, CLASSATTRIBUTES_CONTENTS);
            }
            const METHCALL_PATH: &str = "squirrel_repo/methcall.nut";
            const METHCALL_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/methcall.nut");
            #[test]
            fn test_methcall() {
                $func(METHCALL_PATH, METHCALL_CONTENTS);
            }
            const COROUTINES_PATH: &str = "squirrel_repo/coroutines.nut";
            const COROUTINES_CONTENTS: &str = include_str!("../../../resources/scripts/squirrel_repo/coroutines.nut");
            #[test]
            fn test_coroutines() {
                $func(COROUTINES_PATH, COROUTINES_CONTENTS);
            }
        }
        mod parse_test {
            use super::*;
            const NEWLINES_PATH: &str = "parse_test/newlines.nut";
            const NEWLINES_CONTENTS: &str = include_str!("../../../resources/scripts/parse_test/newlines.nut");
            #[test]
            fn test_newlines() {
                $func(NEWLINES_PATH, NEWLINES_CONTENTS);
            }
        }
    };
}
