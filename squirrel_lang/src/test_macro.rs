#[macro_export]
macro_rules! test_foreach {
    ($func:tt) => {
    mod verify {
        use super::*;
        use std::fs;
        mod lexical_structure {
            use super::*;
            use std::fs;
            const KEYWORDS_PATH: &str = "verify/lexical_structure/keywords.nut";
            #[test]
            fn test_keywords() {
                let contents = fs::read_to_string("../resources/scripts/verify/lexical_structure/keywords.nut").expect("Unable to read script source");
                $func(KEYWORDS_PATH, &contents);
            }
            const LITERALS_PATH: &str = "verify/lexical_structure/literals.nut";
            #[test]
            fn test_literals() {
                let contents = fs::read_to_string("../resources/scripts/verify/lexical_structure/literals.nut").expect("Unable to read script source");
                $func(LITERALS_PATH, &contents);
            }
        }
    }
    mod squirrel_repo {
        use super::*;
        use std::fs;
        const CLASS_PATH: &str = "squirrel_repo/class.nut";
        #[test]
        fn test_class() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/class.nut").expect("Unable to read script source");
            $func(CLASS_PATH, &contents);
        }
        const FIBONACCI_PATH: &str = "squirrel_repo/fibonacci.nut";
        #[test]
        fn test_fibonacci() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/fibonacci.nut").expect("Unable to read script source");
            $func(FIBONACCI_PATH, &contents);
        }
        const MATRIX_PATH: &str = "squirrel_repo/matrix.nut";
        #[test]
        fn test_matrix() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/matrix.nut").expect("Unable to read script source");
            $func(MATRIX_PATH, &contents);
        }
        const METAMETHODS_PATH: &str = "squirrel_repo/metamethods.nut";
        #[test]
        fn test_metamethods() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/metamethods.nut").expect("Unable to read script source");
            $func(METAMETHODS_PATH, &contents);
        }
        const FLOW_PATH: &str = "squirrel_repo/flow.nut";
        #[test]
        fn test_flow() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/flow.nut").expect("Unable to read script source");
            $func(FLOW_PATH, &contents);
        }
        const LIST_PATH: &str = "squirrel_repo/list.nut";
        #[test]
        fn test_list() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/list.nut").expect("Unable to read script source");
            $func(LIST_PATH, &contents);
        }
        const GENERATORS_PATH: &str = "squirrel_repo/generators.nut";
        #[test]
        fn test_generators() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/generators.nut").expect("Unable to read script source");
            $func(GENERATORS_PATH, &contents);
        }
        const TAILSTATE_PATH: &str = "squirrel_repo/tailstate.nut";
        #[test]
        fn test_tailstate() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/tailstate.nut").expect("Unable to read script source");
            $func(TAILSTATE_PATH, &contents);
        }
        const HELLO_PATH: &str = "squirrel_repo/hello.nut";
        #[test]
        fn test_hello() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/hello.nut").expect("Unable to read script source");
            $func(HELLO_PATH, &contents);
        }
        const REGEX_PATH: &str = "squirrel_repo/regex.nut";
        #[test]
        fn test_regex() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/regex.nut").expect("Unable to read script source");
            $func(REGEX_PATH, &contents);
        }
        const ACKERMANN_PATH: &str = "squirrel_repo/ackermann.nut";
        #[test]
        fn test_ackermann() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/ackermann.nut").expect("Unable to read script source");
            $func(ACKERMANN_PATH, &contents);
        }
        const ARRAY_PATH: &str = "squirrel_repo/array.nut";
        #[test]
        fn test_array() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/array.nut").expect("Unable to read script source");
            $func(ARRAY_PATH, &contents);
        }
        const DELEGATION_PATH: &str = "squirrel_repo/delegation.nut";
        #[test]
        fn test_delegation() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/delegation.nut").expect("Unable to read script source");
            $func(DELEGATION_PATH, &contents);
        }
        const LOOPS_PATH: &str = "squirrel_repo/loops.nut";
        #[test]
        fn test_loops() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/loops.nut").expect("Unable to read script source");
            $func(LOOPS_PATH, &contents);
        }
        const CLASSATTRIBUTES_PATH: &str = "squirrel_repo/classattributes.nut";
        #[test]
        fn test_classattributes() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/classattributes.nut").expect("Unable to read script source");
            $func(CLASSATTRIBUTES_PATH, &contents);
        }
        const METHCALL_PATH: &str = "squirrel_repo/methcall.nut";
        #[test]
        fn test_methcall() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/methcall.nut").expect("Unable to read script source");
            $func(METHCALL_PATH, &contents);
        }
        const COROUTINES_PATH: &str = "squirrel_repo/coroutines.nut";
        #[test]
        fn test_coroutines() {
            let contents = fs::read_to_string("../resources/scripts/squirrel_repo/coroutines.nut").expect("Unable to read script source");
            $func(COROUTINES_PATH, &contents);
        }
    }
    mod edge_cases {
        use super::*;
        use std::fs;
        const ROOT_DELEGATE_PATH: &str = "edge_cases/root_delegate.nut";
        #[test]
        fn test_root_delegate() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/root_delegate.nut").expect("Unable to read script source");
            $func(ROOT_DELEGATE_PATH, &contents);
        }
        const ITERATION_PATH: &str = "edge_cases/iteration.nut";
        #[test]
        fn test_iteration() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/iteration.nut").expect("Unable to read script source");
            $func(ITERATION_PATH, &contents);
        }
        const FUNC_ENV_PATH: &str = "edge_cases/func_env.nut";
        #[test]
        fn test_func_env() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/func_env.nut").expect("Unable to read script source");
            $func(FUNC_ENV_PATH, &contents);
        }
        const SET_SCOPE_PATH: &str = "edge_cases/set_scope.nut";
        #[test]
        fn test_set_scope() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/set_scope.nut").expect("Unable to read script source");
            $func(SET_SCOPE_PATH, &contents);
        }
        const OVERWRITING_SCOPE_PATH: &str = "edge_cases/overwriting_scope.nut";
        #[test]
        fn test_overwriting_scope() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/overwriting_scope.nut").expect("Unable to read script source");
            $func(OVERWRITING_SCOPE_PATH, &contents);
        }
        const INHERIT_ENV_PATH: &str = "edge_cases/inherit_env.nut";
        #[test]
        fn test_inherit_env() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/inherit_env.nut").expect("Unable to read script source");
            $func(INHERIT_ENV_PATH, &contents);
        }
        const CLASS_MODIFY_PATH: &str = "edge_cases/class_modify.nut";
        #[test]
        fn test_class_modify() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/class_modify.nut").expect("Unable to read script source");
            $func(CLASS_MODIFY_PATH, &contents);
        }
        const DEFAULT_ARGS_PATH: &str = "edge_cases/default_args.nut";
        #[test]
        fn test_default_args() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/default_args.nut").expect("Unable to read script source");
            $func(DEFAULT_ARGS_PATH, &contents);
        }
        const LOST_ENV_PATH: &str = "edge_cases/lost_env.nut";
        #[test]
        fn test_lost_env() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/lost_env.nut").expect("Unable to read script source");
            $func(LOST_ENV_PATH, &contents);
        }
        const PARENT_SCOPE_PATH: &str = "edge_cases/parent_scope.nut";
        #[test]
        fn test_parent_scope() {
            let contents = fs::read_to_string("../resources/scripts/edge_cases/parent_scope.nut").expect("Unable to read script source");
            $func(PARENT_SCOPE_PATH, &contents);
        }
    }
    mod parse_test {
        use super::*;
        use std::fs;
        const NEWLINES_PATH: &str = "parse_test/newlines.nut";
        #[test]
        fn test_newlines() {
            let contents = fs::read_to_string("../resources/scripts/parse_test/newlines.nut").expect("Unable to read script source");
            $func(NEWLINES_PATH, &contents);
        }
    }
    };
}
