use loxide_macros::src;

#[test]
fn issue6() {
    regression!("issue6.lox", |path| {
        let src = src!(path);
        let mut parser = ::loxide_parser::parser::Parser::new(&src);
        let result = parser.parse();
        insta::assert_debug_snapshot!(result)
    });
}
