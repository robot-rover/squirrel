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
        mod edge_cases {
            use super::*;
            const ROOT_DELEGATE_PATH: &str = "edge_cases/root_delegate.nut";
            const ROOT_DELEGATE_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/root_delegate.nut");
            #[test]
            fn test_root_delegate() {
                $func(ROOT_DELEGATE_PATH, ROOT_DELEGATE_CONTENTS);
            }
            const ITERATION_PATH: &str = "edge_cases/iteration.nut";
            const ITERATION_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/iteration.nut");
            #[test]
            fn test_iteration() {
                $func(ITERATION_PATH, ITERATION_CONTENTS);
            }
            const FUNC_ENV_PATH: &str = "edge_cases/func_env.nut";
            const FUNC_ENV_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/func_env.nut");
            #[test]
            fn test_func_env() {
                $func(FUNC_ENV_PATH, FUNC_ENV_CONTENTS);
            }
            const SET_SCOPE_PATH: &str = "edge_cases/set_scope.nut";
            const SET_SCOPE_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/set_scope.nut");
            #[test]
            fn test_set_scope() {
                $func(SET_SCOPE_PATH, SET_SCOPE_CONTENTS);
            }
            const OVERWRITING_SCOPE_PATH: &str = "edge_cases/overwriting_scope.nut";
            const OVERWRITING_SCOPE_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/overwriting_scope.nut");
            #[test]
            fn test_overwriting_scope() {
                $func(OVERWRITING_SCOPE_PATH, OVERWRITING_SCOPE_CONTENTS);
            }
            const INHERIT_ENV_PATH: &str = "edge_cases/inherit_env.nut";
            const INHERIT_ENV_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/inherit_env.nut");
            #[test]
            fn test_inherit_env() {
                $func(INHERIT_ENV_PATH, INHERIT_ENV_CONTENTS);
            }
            const CLASS_MODIFY_PATH: &str = "edge_cases/class_modify.nut";
            const CLASS_MODIFY_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/class_modify.nut");
            #[test]
            fn test_class_modify() {
                $func(CLASS_MODIFY_PATH, CLASS_MODIFY_CONTENTS);
            }
            const DEFAULT_ARGS_PATH: &str = "edge_cases/default_args.nut";
            const DEFAULT_ARGS_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/default_args.nut");
            #[test]
            fn test_default_args() {
                $func(DEFAULT_ARGS_PATH, DEFAULT_ARGS_CONTENTS);
            }
            const LOST_ENV_PATH: &str = "edge_cases/lost_env.nut";
            const LOST_ENV_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/lost_env.nut");
            #[test]
            fn test_lost_env() {
                $func(LOST_ENV_PATH, LOST_ENV_CONTENTS);
            }
            const PARENT_SCOPE_PATH: &str = "edge_cases/parent_scope.nut";
            const PARENT_SCOPE_CONTENTS: &str = include_str!("../../../resources/scripts/edge_cases/parent_scope.nut");
            #[test]
            fn test_parent_scope() {
                $func(PARENT_SCOPE_PATH, PARENT_SCOPE_CONTENTS);
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
