/*
 * Copyright (c) 2022, Marcin Gasperowicz <xnooga@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullRefPtr.h>
#include <AK/Stack.h>
#include <AK/String.h>
#include <AK/StringBuilder.h>
#include <LibJS/AST.h>
#include <LibJS/SourceRange.h>

namespace JS {

class Printer : public ASTNode::Visitor {
public:
    Printer(const ASTNode& start, int indent_increment = 2)
        : m_indent_increment(indent_increment)
        , m_ast(move(start))
    {
    }

    AK::String format();

private:
    void push_indent(int indent);
    void push_indent() { push_indent(m_indent_increment); }
    void pop_indent();
    void indent()
    {
        for (int i = 0; i < m_indent_level; i++) {
            emit(" ");
        }
    }
    void newline()
    {
        emit("\n");
        indent();
    }
    bool semicolon()
    {
        if (!m_should_omit_semicolon) {
            emit(";");
            return true;
        }
        m_should_omit_semicolon = false;
        return false;
    }
    void omit_semicolon()
    {
        m_should_omit_semicolon = true;
    }

    void emit(String s)
    {
        out("{}", s);
        m_string_builder.append(s);
    }

    Stack<int, 128> m_indents;
    int m_indent_level;
    int m_indent_increment;
    bool m_should_omit_semicolon;

    void visit(const LabelledStatement&);
    void visit(const ScopeNode&);
    void visit(const BinaryExpression&);
    void visit(const LogicalExpression&);
    void visit(const UnaryExpression&);
    void visit(const CallExpression&);
    void visit(const SuperCall&);
    void visit(const ClassDeclaration&);
    void visit(const ClassExpression&);
    void visit(const ClassMethod&);
    void visit(const ClassField&);
    void visit(const StaticInitializer&);
    void visit(const StringLiteral&);
    void visit(const SuperExpression&);
    void visit(const NumericLiteral&);
    void visit(const BigIntLiteral&);
    void visit(const BooleanLiteral&);
    void visit(const NullLiteral&);
    void visit(const BindingPattern&);
    void visit(const FunctionNode&);
    void visit(const FunctionDeclaration&);
    void visit(const FunctionExpression&);
    void visit(const YieldExpression&);
    void visit(const AwaitExpression&);
    void visit(const ReturnStatement&);
    void visit(const IfStatement&);
    void visit(const WhileStatement&);
    void visit(const WithStatement&);
    void visit(const DoWhileStatement&);
    void visit(const ForStatement&);
    void visit(const ForInStatement&);
    void visit(const ForOfStatement&);
    void visit(const ForAwaitOfStatement&);
    void visit(const Identifier&);
    void visit(const PrivateIdentifier&);
    void visit(const SpreadExpression&);
    void visit(const ThisExpression&);
    void visit(const AssignmentExpression&);
    void visit(const UpdateExpression&);
    void visit(const VariableDeclaration&);
    void visit(const VariableDeclarator&);
    void visit(const ObjectProperty&);
    void visit(const ObjectExpression&);
    void visit(const ExpressionStatement&);
    void visit(const MemberExpression&);
    void visit(const OptionalChain&);
    void visit(const MetaProperty&);
    void visit(const ImportCall&);
    void visit(const RegExpLiteral&);
    void visit(const ArrayExpression&);
    void visit(const TemplateLiteral&);
    void visit(const TaggedTemplateLiteral&);
    void visit(const TryStatement&);
    void visit(const CatchClause&);
    void visit(const ThrowStatement&);
    void visit(const SwitchStatement&);
    void visit(const SwitchCase&);
    void visit(const ConditionalExpression&);
    void visit(const SequenceExpression&);
    void visit(const ExportStatement&);
    void visit(const ImportStatement&);
    void visit(const ErrorExpression&);
    void visit(const ContinueStatement&);
    void visit(const SyntheticReferenceExpression&);
    void visit(const ClassFieldInitializerStatement&);
    void visit(const ErrorDeclaration&);
    void visit(const ErrorStatement&);
    void visit(const BreakStatement&);
    void visit(const DebuggerStatement&);
    void visit(const EmptyStatement&);

    Position m_position;
    StringBuilder m_string_builder;

    NonnullRefPtr<ASTNode> m_ast;
};

}
