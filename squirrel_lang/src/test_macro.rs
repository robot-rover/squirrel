#[macro_export]
macro_rules! test_foreach {
    ($func:tt) => {
    mod test {
        use super::*;
        use std::fs;
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
    }
    };
}
