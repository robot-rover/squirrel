#[macro_export]
macro_rules! test_foreach {
    ($func:tt) => {
#[rustfmt::skip]
        mod edge_cases {
            use super::$func;
            use crate::test_macro::data::edge_cases::*;
            #[test]
            fn test_func_env() {
                $func(FUNC_ENV_PATH, FUNC_ENV_CONTENTS);
            }
            #[test]
            fn test_inherit_env() {
                $func(INHERIT_ENV_PATH, INHERIT_ENV_CONTENTS);
            }
            #[test]
            fn test_delete() {
                $func(DELETE_PATH, DELETE_CONTENTS);
            }
            #[test]
            fn test_lost_env() {
                $func(LOST_ENV_PATH, LOST_ENV_CONTENTS);
            }
            #[test]
            fn test_overwriting_scope() {
                $func(OVERWRITING_SCOPE_PATH, OVERWRITING_SCOPE_CONTENTS);
            }
            #[test]
            fn test_set_scope() {
                $func(SET_SCOPE_PATH, SET_SCOPE_CONTENTS);
            }
            #[test]
            fn test_root_delegate() {
                $func(ROOT_DELEGATE_PATH, ROOT_DELEGATE_CONTENTS);
            }
            #[test]
            fn test_freed_locals() {
                $func(FREED_LOCALS_PATH, FREED_LOCALS_CONTENTS);
            }
            #[test]
            fn test_class_modify() {
                $func(CLASS_MODIFY_PATH, CLASS_MODIFY_CONTENTS);
            }
            #[test]
            fn test_parent_scope() {
                $func(PARENT_SCOPE_PATH, PARENT_SCOPE_CONTENTS);
            }
            #[test]
            fn test_delete_to_root() {
                $func(DELETE_TO_ROOT_PATH, DELETE_TO_ROOT_CONTENTS);
            }
            #[test]
            fn test_closure_root() {
                $func(CLOSURE_ROOT_PATH, CLOSURE_ROOT_CONTENTS);
            }
            #[test]
            fn test_default_args() {
                $func(DEFAULT_ARGS_PATH, DEFAULT_ARGS_CONTENTS);
            }
            #[test]
            fn test_closure_this() {
                $func(CLOSURE_THIS_PATH, CLOSURE_THIS_CONTENTS);
            }
            #[test]
            fn test_shadow_builtin() {
                $func(SHADOW_BUILTIN_PATH, SHADOW_BUILTIN_CONTENTS);
            }
            #[test]
            fn test_iteration() {
                $func(ITERATION_PATH, ITERATION_CONTENTS);
            }
            #[test]
            fn test_meta_order() {
                $func(META_ORDER_PATH, META_ORDER_CONTENTS);
            }
        }
#[rustfmt::skip]
        mod squirrel_repo {
            use super::$func;
            use crate::test_macro::data::squirrel_repo::*;
            #[test]
            fn test_generators() {
                $func(GENERATORS_PATH, GENERATORS_CONTENTS);
            }
            #[test]
            fn test_regex() {
                $func(REGEX_PATH, REGEX_CONTENTS);
            }
            #[test]
            fn test_classattributes() {
                $func(CLASSATTRIBUTES_PATH, CLASSATTRIBUTES_CONTENTS);
            }
            #[test]
            fn test_delegation() {
                $func(DELEGATION_PATH, DELEGATION_CONTENTS);
            }
            #[test]
            fn test_fibonacci() {
                $func(FIBONACCI_PATH, FIBONACCI_CONTENTS);
            }
            #[test]
            fn test_array() {
                $func(ARRAY_PATH, ARRAY_CONTENTS);
            }
            #[test]
            fn test_metamethods() {
                $func(METAMETHODS_PATH, METAMETHODS_CONTENTS);
            }
            #[test]
            fn test_methcall() {
                $func(METHCALL_PATH, METHCALL_CONTENTS);
            }
            #[test]
            fn test_coroutines() {
                $func(COROUTINES_PATH, COROUTINES_CONTENTS);
            }
            #[test]
            fn test_ackermann() {
                $func(ACKERMANN_PATH, ACKERMANN_CONTENTS);
            }
            #[test]
            fn test_tailstate() {
                $func(TAILSTATE_PATH, TAILSTATE_CONTENTS);
            }
            #[test]
            fn test_class() {
                $func(CLASS_PATH, CLASS_CONTENTS);
            }
            #[test]
            fn test_loops() {
                $func(LOOPS_PATH, LOOPS_CONTENTS);
            }
            #[test]
            fn test_flow() {
                $func(FLOW_PATH, FLOW_CONTENTS);
            }
            #[test]
            fn test_matrix() {
                $func(MATRIX_PATH, MATRIX_CONTENTS);
            }
            #[test]
            fn test_hello() {
                $func(HELLO_PATH, HELLO_CONTENTS);
            }
            #[test]
            fn test_list() {
                $func(LIST_PATH, LIST_CONTENTS);
            }
        }
#[rustfmt::skip]
        mod verify {
            use super::$func;
            use crate::test_macro::data::verify::*;
            #[test]
            fn test_tables() {
                $func(TABLES_PATH, TABLES_CONTENTS);
            }
#[rustfmt::skip]
            mod lexical_structure {
                use super::$func;
                use crate::test_macro::data::verify::lexical_structure::*;
                #[test]
                fn test_literals() {
                    $func(LITERALS_PATH, LITERALS_CONTENTS);
                }
                #[test]
                fn test_keywords() {
                    $func(KEYWORDS_PATH, KEYWORDS_CONTENTS);
                }
            }
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
            mod metamethods {
                use super::$func;
                use crate::test_macro::data::verify::metamethods::*;
                #[test]
                fn test_cmp() {
                    $func(CMP_PATH, CMP_CONTENTS);
                }
                #[test]
                fn test_get_set() {
                    $func(GET_SET_PATH, GET_SET_CONTENTS);
                }
                #[test]
                fn test_arith() {
                    $func(ARITH_PATH, ARITH_CONTENTS);
                }
                #[test]
                fn test_classes() {
                    $func(CLASSES_PATH, CLASSES_CONTENTS);
                }
                #[test]
                fn test_misc() {
                    $func(MISC_PATH, MISC_CONTENTS);
                }
            }
#[rustfmt::skip]
            mod values_and_data_types {
                use super::$func;
                use crate::test_macro::data::verify::values_and_data_types::*;
                #[test]
                fn test_table() {
                    $func(TABLE_PATH, TABLE_CONTENTS);
                }
                #[test]
                fn test_float() {
                    $func(FLOAT_PATH, FLOAT_CONTENTS);
                }
                #[test]
                fn test_array() {
                    $func(ARRAY_PATH, ARRAY_CONTENTS);
                }
                #[test]
                fn test_class() {
                    $func(CLASS_PATH, CLASS_CONTENTS);
                }
                #[test]
                fn test_string() {
                    $func(STRING_PATH, STRING_CONTENTS);
                }
                #[test]
                fn test_integer() {
                    $func(INTEGER_PATH, INTEGER_CONTENTS);
                }
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
    pub mod edge_cases {
        pub const FUNC_ENV_PATH: &str = "edge_cases/func_env.nut";
        pub const FUNC_ENV_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/func_env.nut");
        pub const INHERIT_ENV_PATH: &str = "edge_cases/inherit_env.nut";
        pub const INHERIT_ENV_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/inherit_env.nut");
        pub const DELETE_PATH: &str = "edge_cases/delete.nut";
        pub const DELETE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/delete.nut");
        pub const LOST_ENV_PATH: &str = "edge_cases/lost_env.nut";
        pub const LOST_ENV_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/lost_env.nut");
        pub const OVERWRITING_SCOPE_PATH: &str = "edge_cases/overwriting_scope.nut";
        pub const OVERWRITING_SCOPE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/overwriting_scope.nut");
        pub const SET_SCOPE_PATH: &str = "edge_cases/set_scope.nut";
        pub const SET_SCOPE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/set_scope.nut");
        pub const ROOT_DELEGATE_PATH: &str = "edge_cases/root_delegate.nut";
        pub const ROOT_DELEGATE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/root_delegate.nut");
        pub const FREED_LOCALS_PATH: &str = "edge_cases/freed_locals.nut";
        pub const FREED_LOCALS_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/freed_locals.nut");
        pub const CLASS_MODIFY_PATH: &str = "edge_cases/class_modify.nut";
        pub const CLASS_MODIFY_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/class_modify.nut");
        pub const PARENT_SCOPE_PATH: &str = "edge_cases/parent_scope.nut";
        pub const PARENT_SCOPE_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/parent_scope.nut");
        pub const DELETE_TO_ROOT_PATH: &str = "edge_cases/delete_to_root.nut";
        pub const DELETE_TO_ROOT_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/delete_to_root.nut");
        pub const CLOSURE_ROOT_PATH: &str = "edge_cases/closure_root.nut";
        pub const CLOSURE_ROOT_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/closure_root.nut");
        pub const DEFAULT_ARGS_PATH: &str = "edge_cases/default_args.nut";
        pub const DEFAULT_ARGS_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/default_args.nut");
        pub const CLOSURE_THIS_PATH: &str = "edge_cases/closure_this.nut";
        pub const CLOSURE_THIS_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/closure_this.nut");
        pub const SHADOW_BUILTIN_PATH: &str = "edge_cases/shadow_builtin.nut";
        pub const SHADOW_BUILTIN_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/shadow_builtin.nut");
        pub const ITERATION_PATH: &str = "edge_cases/iteration.nut";
        pub const ITERATION_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/iteration.nut");
        pub const META_ORDER_PATH: &str = "edge_cases/meta_order.nut";
        pub const META_ORDER_CONTENTS: &str = include_str!("../../resources/scripts/edge_cases/meta_order.nut");
    }
    pub mod squirrel_repo {
        pub const GENERATORS_PATH: &str = "squirrel_repo/generators.nut";
        pub const GENERATORS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/generators.nut");
        pub const REGEX_PATH: &str = "squirrel_repo/regex.nut";
        pub const REGEX_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/regex.nut");
        pub const CLASSATTRIBUTES_PATH: &str = "squirrel_repo/classattributes.nut";
        pub const CLASSATTRIBUTES_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/classattributes.nut");
        pub const DELEGATION_PATH: &str = "squirrel_repo/delegation.nut";
        pub const DELEGATION_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/delegation.nut");
        pub const FIBONACCI_PATH: &str = "squirrel_repo/fibonacci.nut";
        pub const FIBONACCI_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/fibonacci.nut");
        pub const ARRAY_PATH: &str = "squirrel_repo/array.nut";
        pub const ARRAY_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/array.nut");
        pub const METAMETHODS_PATH: &str = "squirrel_repo/metamethods.nut";
        pub const METAMETHODS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/metamethods.nut");
        pub const METHCALL_PATH: &str = "squirrel_repo/methcall.nut";
        pub const METHCALL_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/methcall.nut");
        pub const COROUTINES_PATH: &str = "squirrel_repo/coroutines.nut";
        pub const COROUTINES_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/coroutines.nut");
        pub const ACKERMANN_PATH: &str = "squirrel_repo/ackermann.nut";
        pub const ACKERMANN_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/ackermann.nut");
        pub const TAILSTATE_PATH: &str = "squirrel_repo/tailstate.nut";
        pub const TAILSTATE_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/tailstate.nut");
        pub const CLASS_PATH: &str = "squirrel_repo/class.nut";
        pub const CLASS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/class.nut");
        pub const LOOPS_PATH: &str = "squirrel_repo/loops.nut";
        pub const LOOPS_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/loops.nut");
        pub const FLOW_PATH: &str = "squirrel_repo/flow.nut";
        pub const FLOW_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/flow.nut");
        pub const MATRIX_PATH: &str = "squirrel_repo/matrix.nut";
        pub const MATRIX_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/matrix.nut");
        pub const HELLO_PATH: &str = "squirrel_repo/hello.nut";
        pub const HELLO_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/hello.nut");
        pub const LIST_PATH: &str = "squirrel_repo/list.nut";
        pub const LIST_CONTENTS: &str = include_str!("../../resources/scripts/squirrel_repo/list.nut");
    }
    pub mod verify {
        pub const TABLES_PATH: &str = "verify/tables.nut";
        pub const TABLES_CONTENTS: &str = include_str!("../../resources/scripts/verify/tables.nut");
        pub mod lexical_structure {
            pub const LITERALS_PATH: &str = "verify/lexical_structure/literals.nut";
            pub const LITERALS_CONTENTS: &str = include_str!("../../resources/scripts/verify/lexical_structure/literals.nut");
            pub const KEYWORDS_PATH: &str = "verify/lexical_structure/keywords.nut";
            pub const KEYWORDS_CONTENTS: &str = include_str!("../../resources/scripts/verify/lexical_structure/keywords.nut");
        }
        pub mod statements {
            pub const LOOPS_PATH: &str = "verify/statements/loops.nut";
            pub const LOOPS_CONTENTS: &str = include_str!("../../resources/scripts/verify/statements/loops.nut");
        }
        pub mod metamethods {
            pub const CMP_PATH: &str = "verify/metamethods/cmp.nut";
            pub const CMP_CONTENTS: &str = include_str!("../../resources/scripts/verify/metamethods/cmp.nut");
            pub const GET_SET_PATH: &str = "verify/metamethods/get_set.nut";
            pub const GET_SET_CONTENTS: &str = include_str!("../../resources/scripts/verify/metamethods/get_set.nut");
            pub const ARITH_PATH: &str = "verify/metamethods/arith.nut";
            pub const ARITH_CONTENTS: &str = include_str!("../../resources/scripts/verify/metamethods/arith.nut");
            pub const CLASSES_PATH: &str = "verify/metamethods/classes.nut";
            pub const CLASSES_CONTENTS: &str = include_str!("../../resources/scripts/verify/metamethods/classes.nut");
            pub const MISC_PATH: &str = "verify/metamethods/misc.nut";
            pub const MISC_CONTENTS: &str = include_str!("../../resources/scripts/verify/metamethods/misc.nut");
        }
        pub mod values_and_data_types {
            pub const TABLE_PATH: &str = "verify/values_and_data_types/table.nut";
            pub const TABLE_CONTENTS: &str = include_str!("../../resources/scripts/verify/values_and_data_types/table.nut");
            pub const FLOAT_PATH: &str = "verify/values_and_data_types/float.nut";
            pub const FLOAT_CONTENTS: &str = include_str!("../../resources/scripts/verify/values_and_data_types/float.nut");
            pub const ARRAY_PATH: &str = "verify/values_and_data_types/array.nut";
            pub const ARRAY_CONTENTS: &str = include_str!("../../resources/scripts/verify/values_and_data_types/array.nut");
            pub const CLASS_PATH: &str = "verify/values_and_data_types/class.nut";
            pub const CLASS_CONTENTS: &str = include_str!("../../resources/scripts/verify/values_and_data_types/class.nut");
            pub const STRING_PATH: &str = "verify/values_and_data_types/string.nut";
            pub const STRING_CONTENTS: &str = include_str!("../../resources/scripts/verify/values_and_data_types/string.nut");
            pub const INTEGER_PATH: &str = "verify/values_and_data_types/integer.nut";
            pub const INTEGER_CONTENTS: &str = include_str!("../../resources/scripts/verify/values_and_data_types/integer.nut");
        }
    }
    pub mod parse_test {
        pub const NEWLINES_PATH: &str = "parse_test/newlines.nut";
        pub const NEWLINES_CONTENTS: &str = include_str!("../../resources/scripts/parse_test/newlines.nut");
    }
}
