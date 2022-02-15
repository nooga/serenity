/*
 * Copyright (c) 2022, Marcin Gasperowicz <xnooga@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Assertions.h>
#include <AK/JsonArray.h>
#include <AK/JsonObject.h>
#include <AK/JsonValue.h>
#include <AK/StringBuilder.h>
#include <LibCore/ArgsParser.h>
#include <LibCore/File.h>
#include <LibCore/System.h>
#include <LibJS/AST.h>
#include <LibJS/Parser.h>
#include <LibJS/Printer.h>
#include <LibMain/Main.h>
#include <unistd.h>

ErrorOr<int> serenity_main(Main::Arguments arguments)
{
#ifdef __serenity__
    TRY(Core::System::pledge("stdio rpath"));
#endif

    StringView path;
    int spaces_in_indent = 2;

    Core::ArgsParser args_parser;
    args_parser.set_general_help("Pretty-print a JS file.");
    args_parser.add_option(spaces_in_indent, "Indent size", "indent-size", 'i', "spaces_in_indent");
    args_parser.add_positional_argument(path, "Path to JS file", "path", Core::ArgsParser::Required::No);
    VERIFY(spaces_in_indent >= 0);
    args_parser.parse(arguments);

    RefPtr<Core::File> file;
    if (path == nullptr)
        file = Core::File::standard_input();
    else
        file = TRY(Core::File::open(path, Core::OpenMode::ReadOnly));

#ifdef __serenity__
    TRY(Core::System::pledge("stdio"));
#endif

    auto source = file->read_all();
    auto parser = JS::Parser(JS::Lexer(source));
    auto ast = parser.parse_program();

    if (parser.has_errors()) {
        parser.print_errors();
        return 1;
    }

    auto printer = JS::Printer(ast);
    auto output = printer.format();

    out("{}", output);
    outln();

    return 0;
}