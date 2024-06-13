#[macro_export]
macro_rules! test_foreach {
    ($func:tt) => {
    mod test {
        use super::*;
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
        mod verify {
            use super::*;
            mod lexical_structure {
                use super::*;
                const KEYWORDS_PATH: &str = "verify/lexical_structure/keywords.nut";
                const KEYWORDS_CONTENTS: &str = include_str!("../../../resources/scripts/verify/lexical_structure/keywords.nut");
                #[test]
                fn test_keywords() {
                    $func(KEYWORDS_PATH, KEYWORDS_CONTENTS);
                }
                const LITERALS_PATH: &str = "verify/lexical_structure/literals.nut";
                const LITERALS_CONTENTS: &str = include_str!("../../../resources/scripts/verify/lexical_structure/literals.nut");
                #[test]
                fn test_literals() {
                    $func(LITERALS_PATH, LITERALS_CONTENTS);
                }
            }
        }
    }
    };
}
