regression!(issue15, |src| {
    let mut parser = ::loxide_parser::parser::Parser::new(src);
    let result = parser.parse();
    insta::assert_debug_snapshot!(result)
});
