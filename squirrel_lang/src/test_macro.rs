#[macro_export]
macro_rules! test_foreach {
    ($func:tt) => {
#[rustfmt::skip]
        mod verify {
            use super::$func;
#[rustfmt::skip]
            mod statements {
                use super::$func;
                use crate::test_macro::data::verify::statements::*;
                #[test]
                fn test_loops() {
                    $func(LOOPS_PATH, LOOPS_CONTENTS);
                }
            }
#[rustfmt::skip]
            mod lexical_structure {
                use super::$func;
                use crate::test_macro::data::verify::lexical_structure::*;
                #[test]
                fn test_keywords() {
                    $func(KEYWORDS_PATH, KEYWORDS_CONTENTS);
                }
                #[test]
                fn test_literals() {
                    $func(LITERALS_PATH, LITERALS_CONTENTS);
                }
            }
        }
#[rustfmt::skip]
        mod squirrel_repo {
            use super::$func;
            use crate::test_macro::data::squirrel_repo::*;
            #[test]
            fn test_class() {
                $func(CLASS_PATH, CLASS_CONTENTS);
            }
            #[test]
            fn test_fibonacci() {
                $func(FIBONACCI_PATH, FIBONACCI_CONTENTS);
            }
            #[test]
            fn test_matrix() {
                $func(MATRIX_PATH, MATRIX_CONTENTS);
            }
            #[test]
            fn test_metamethods() {
                $func(METAMETHODS_PATH, METAMETHODS_CONTENTS);
            }
            #[test]
            fn test_flow() {
                $func(FLOW_PATH, FLOW_CONTENTS);
            }
            #[test]
            fn test_list() {
                $func(LIST_PATH, LIST_CONTENTS);
            }
            #[test]
            fn test_generators() {
                $func(GENERATORS_PATH, GENERATORS_CONTENTS);
            }
            #[test]
            fn test_tailstate() {
                $func(TAILSTATE_PATH, TAILSTATE_CONTENTS);
            }
            #[test]
            fn test_hello() {
                $func(HELLO_PATH, HELLO_CONTENTS);
            }
            #[test]
            fn test_regex() {
                $func(REGEX_PATH, REGEX_CONTENTS);
            }
            #[test]
            fn test_ackermann() {
                $func(ACKERMANN_PATH, ACKERMANN_CONTENTS);
            }
            #[test]
            fn test_array() {
                $func(ARRAY_PATH, ARRAY_CONTENTS);
            }
            #[test]
            fn test_delegation() {
                $func(DELEGATION_PATH, DELEGATION_CONTENTS);
            }
            #[test]
            fn test_loops() {
                $func(LOOPS_PATH, LOOPS_CONTENTS);
            }
            #[test]
            fn test_classattributes() {
                $func(CLASSATTRIBUTES_PATH, CLASSATTRIBUTES_CONTENTS);
            }
            #[test]
            fn test_methcall() {
                $func(METHCALL_PATH, METHCALL_CONTENTS);
            }
            #[test]
            fn test_coroutines() {
                $func(COROUTINES_PATH, COROUTINES_CONTENTS);
            }
        }
#[rustfmt::skip]
        mod edge_cases {
            use super::$func;
            use crate::test_macro::data::edge_cases::*;
            #[test]
            fn test_root_delegate() {
                $func(ROOT_DELEGATE_PATH, ROOT_DELEGATE_CONTENTS);
            }
            #[test]
            fn test_iteration() {
                $func(ITERATION_PATH, ITERATION_CONTENTS);
            }
            #[test]
            fn test_freed_locals() {
                $func(FREED_LOCALS_PATH, FREED_LOCALS_CONTENTS);
            }
            #[test]
            fn test_func_env() {
                $func(FUNC_ENV_PATH, FUNC_ENV_CONTENTS);
            }
            #[test]
            fn test_set_scope() {
                $func(SET_SCOPE_PATH, SET_SCOPE_CONTENTS);
            }
            #[test]
            fn test_overwriting_scope() {
                $func(OVERWRITING_SCOPE_PATH, OVERWRITING_SCOPE_CONTENTS);
            }
            #[test]
            fn test_inherit_env() {
                $func(INHERIT_ENV_PATH, INHERIT_ENV_CONTENTS);
            }
            #[test]
            fn test_class_modify() {
                $func(CLASS_MODIFY_PATH, CLASS_MODIFY_CONTENTS);
            }
            #[test]
            fn test_default_args() {
                $func(DEFAULT_ARGS_PATH, DEFAULT_ARGS_CONTENTS);
            }
            #[test]
            fn test_shadow_builtin() {
                $func(SHADOW_BUILTIN_PATH, SHADOW_BUILTIN_CONTENTS);
            }
            #[test]
            fn test_lost_env() {
                $func(LOST_ENV_PATH, LOST_ENV_CONTENTS);
            }
            #[test]
            fn test_parent_scope() {
                $func(PARENT_SCOPE_PATH, PARENT_SCOPE_CONTENTS);
            }
        }
#[rustfmt::skip]
        mod parse_test {
            use super::$func;
            use crate::test_macro::data::parse_test::*;
            #[test]
            fn test_newlines() {
                $func(NEWLINES_PATH, NEWLINES_CONTENTS);
            }
        }
    };
}
#[rustfmt::skip]
pub mod data {
    pub mod verify {
        pub mod statements {
            pub const LOOPS_PATH: &str = "verify/statements/loops.nut";
            pub const LOOPS_CONTENTS: &str = include_str!("../../resources/scripts/verify/statements/loops.nut");
        }
        pub mod lexical_structure {
            pub const KEYWORDS_PATH: &str = "verify/lexical_structure/keywords.nut";
            pub const KEYWORDS_CONTENTS: &str = include_str!("../../resources/scripts/verify/lexical_structure/keywords.nut");
            pub const LITERALS_PATH: &str = "verify/lexical_structure/literals.nut";
            pub const LITERALS_CONTENTS: &str = include_str!("../../resources/scripts/verify/lexical_structure/literals.nut");
        }
    }
    pub mod squirrel_repo {
        pub const CLASS_PATH: &str = "squirrel_repo/class.nut";
        pub const CLASS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/class.nut");
        pub const FIBONACCI_PATH: &str = "squirrel_repo/fibonacci.nut";
        pub const FIBONACCI_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/fibonacci.nut");
        pub const MATRIX_PATH: &str = "squirrel_repo/matrix.nut";
        pub const MATRIX_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/matrix.nut");
        pub const METAMETHODS_PATH: &str = "squirrel_repo/metamethods.nut";
        pub const METAMETHODS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/metamethods.nut");
        pub const FLOW_PATH: &str = "squirrel_repo/flow.nut";
        pub const FLOW_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/flow.nut");
        pub const LIST_PATH: &str = "squirrel_repo/list.nut";
        pub const LIST_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/list.nut");
        pub const GENERATORS_PATH: &str = "squirrel_repo/generators.nut";
        pub const GENERATORS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/generators.nut");
        pub const TAILSTATE_PATH: &str = "squirrel_repo/tailstate.nut";
        pub const TAILSTATE_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/tailstate.nut");
        pub const HELLO_PATH: &str = "squirrel_repo/hello.nut";
        pub const HELLO_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/hello.nut");
        pub const REGEX_PATH: &str = "squirrel_repo/regex.nut";
        pub const REGEX_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/regex.nut");
        pub const ACKERMANN_PATH: &str = "squirrel_repo/ackermann.nut";
        pub const ACKERMANN_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/ackermann.nut");
        pub const ARRAY_PATH: &str = "squirrel_repo/array.nut";
        pub const ARRAY_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/array.nut");
        pub const DELEGATION_PATH: &str = "squirrel_repo/delegation.nut";
        pub const DELEGATION_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/delegation.nut");
        pub const LOOPS_PATH: &str = "squirrel_repo/loops.nut";
        pub const LOOPS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/loops.nut");
        pub const CLASSATTRIBUTES_PATH: &str = "squirrel_repo/classattributes.nut";
        pub const CLASSATTRIBUTES_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/classattributes.nut");
        pub const METHCALL_PATH: &str = "squirrel_repo/methcall.nut";
        pub const METHCALL_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/methcall.nut");
        pub const COROUTINES_PATH: &str = "squirrel_repo/coroutines.nut";
        pub const COROUTINES_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/coroutines.nut");
    }
    pub mod edge_cases {
        pub const ROOT_DELEGATE_PATH: &str = "edge_cases/root_delegate.nut";
        pub const ROOT_DELEGATE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/root_delegate.nut");
        pub const ITERATION_PATH: &str = "edge_cases/iteration.nut";
        pub const ITERATION_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/iteration.nut");
        pub const FREED_LOCALS_PATH: &str = "edge_cases/freed_locals.nut";
        pub const FREED_LOCALS_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/freed_locals.nut");
        pub const FUNC_ENV_PATH: &str = "edge_cases/func_env.nut";
        pub const FUNC_ENV_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/func_env.nut");
        pub const SET_SCOPE_PATH: &str = "edge_cases/set_scope.nut";
        pub const SET_SCOPE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/set_scope.nut");
        pub const OVERWRITING_SCOPE_PATH: &str = "edge_cases/overwriting_scope.nut";
        pub const OVERWRITING_SCOPE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/overwriting_scope.nut");
        pub const INHERIT_ENV_PATH: &str = "edge_cases/inherit_env.nut";
        pub const INHERIT_ENV_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/inherit_env.nut");
        pub const CLASS_MODIFY_PATH: &str = "edge_cases/class_modify.nut";
        pub const CLASS_MODIFY_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/class_modify.nut");
        pub const DEFAULT_ARGS_PATH: &str = "edge_cases/default_args.nut";
        pub const DEFAULT_ARGS_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/default_args.nut");
        pub const SHADOW_BUILTIN_PATH: &str = "edge_cases/shadow_builtin.nut";
        pub const SHADOW_BUILTIN_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/shadow_builtin.nut");
        pub const LOST_ENV_PATH: &str = "edge_cases/lost_env.nut";
        pub const LOST_ENV_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/lost_env.nut");
        pub const PARENT_SCOPE_PATH: &str = "edge_cases/parent_scope.nut";
        pub const PARENT_SCOPE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/parent_scope.nut");
    }
    pub mod parse_test {
        pub const NEWLINES_PATH: &str = "parse_test/newlines.nut";
        pub const NEWLINES_CONTENTS: &str = include_str!("../../resources/scripts/parse_test/newlines.nut");
    }
}
