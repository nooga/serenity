/*
 * Copyright (c) 2022, Marcin Gasperowicz <xnooga@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibJS/Printer.h>

namespace JS {

String op_to_string(BinaryOp op)
{
    switch (op) {
    case BinaryOp::Addition:
        return "+";
    case BinaryOp::Subtraction:
        return "-";
    case BinaryOp::Multiplication:
        return "*";
    case BinaryOp::Division:
        return "/";
    case BinaryOp::Modulo:
        return "%";
    case BinaryOp::GreaterThan:
        return ">";
    case BinaryOp::GreaterThanEquals:
        return ">=";
    case BinaryOp::LessThan:
        return "<";
    case BinaryOp::LessThanEquals:
        return "<=";
    case BinaryOp::BitwiseAnd:
        return "&";
    case BinaryOp::BitwiseOr:
        return "|";
    case BinaryOp::BitwiseXor:
        return "^";
    case BinaryOp::LeftShift:
        return "<<";
    case BinaryOp::RightShift:
        return ">>";
    case BinaryOp::StrictlyEquals:
        return "===";
    case BinaryOp::StrictlyInequals:
        return "!==";
    case BinaryOp::LooselyEquals:
        return "==";
    case BinaryOp::LooselyInequals:
        return "!=";
    case BinaryOp::UnsignedRightShift:
        return ">>>";
    case BinaryOp::In:
        return "in";
    case BinaryOp::InstanceOf:
        return "instanceof";
    case BinaryOp::Exponentiation:
        return "**";
    }
    VERIFY_NOT_REACHED();
    return "";
}

String op_to_string(LogicalOp op)
{
    switch (op) {
    case LogicalOp::And:
        return "&&";
    case LogicalOp::Or:
        return "||";
    case LogicalOp::NullishCoalescing:
        return "??";
    default:
        VERIFY_NOT_REACHED();
        return "";
    }
}

String op_to_string(UnaryOp op)
{
    switch (op) {
    case UnaryOp::BitwiseNot:
        return "~";
    case UnaryOp::Not:
        return "!";
    case UnaryOp::Plus:
        return "+";
    case UnaryOp::Minus:
        return "-";
    case UnaryOp::Typeof:
        return "typeof ";
    case UnaryOp::Void:
        return "void ";
    case UnaryOp::Delete:
        return "delete ";
    default:
        VERIFY_NOT_REACHED();
        return "";
    }
}

String op_to_string(AssignmentOp op)
{
    switch (op) {
    case AssignmentOp::Assignment:
        return "=";
    case AssignmentOp::AdditionAssignment:
        return "+=";
    case AssignmentOp::SubtractionAssignment:
        return "-=";
    case AssignmentOp::MultiplicationAssignment:
        return "*=";
    case AssignmentOp::DivisionAssignment:
        return "/=";
    case AssignmentOp::ExponentiationAssignment:
        return "**=";
    case AssignmentOp::BitwiseAndAssignment:
        return "&=";
    case AssignmentOp::BitwiseOrAssignment:
        return "|=";
    case AssignmentOp::BitwiseXorAssignment:
        return "^=";
    case AssignmentOp::LeftShiftAssignment:
        return "<<=";
    case AssignmentOp::RightShiftAssignment:
        return ">>=";
    case AssignmentOp::UnsignedRightShiftAssignment:
        return ">>>=";
    case AssignmentOp::AndAssignment:
        return "&&=";
    case AssignmentOp::OrAssignment:
        return "||=";
    case AssignmentOp::NullishAssignment:
        return "\?\?=";
    case AssignmentOp::ModuloAssignment:
        return "%=";
    }
    VERIFY_NOT_REACHED();
    return "";
}

String op_to_string(UpdateOp op)
{
    switch (op) {
    case UpdateOp::Increment:
        return "++";
    case UpdateOp::Decrement:
        return "--";
    }
    VERIFY_NOT_REACHED();
    return "";
}

String Printer::format()
{
    //m_ast->dump(0);
    m_ast->accept_visitor(*this);
    return m_string_builder.build();
}

void Printer::push_indent(int indent)
{
    m_indent_level += indent;
    m_indents.push(indent);
}

void Printer::pop_indent()
{
    VERIFY(!m_indents.is_empty());
    m_indent_level -= m_indents.top();
    m_indents.pop();
}

void Printer::visit(const LabelledStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ScopeNode& node)
{
    auto children = node.children();
    int n = children.size();
    if (!node.is_program()) {
        emit("{");
        push_indent();
        newline();
    }
    for (auto& child : children) {
        child.accept_visitor(*this);
        bool semi = semicolon();
        if (--n > 0 && semi)
            newline();
    }
    if (!node.is_program()) {
        pop_indent();
        newline();
        emit("}");
        newline();
    }
}
void Printer::visit(const BinaryExpression& node)
{
    node.lhs()->accept_visitor(*this);
    emit(" ");
    emit(op_to_string(node.op()));
    emit(" ");
    node.rhs()->accept_visitor(*this);
}
void Printer::visit(const LogicalExpression& node)
{
    node.lhs()->accept_visitor(*this);
    emit(" ");
    emit(op_to_string(node.op()));
    emit(" ");
    node.rhs()->accept_visitor(*this);
}
void Printer::visit(const UnaryExpression& node)
{
    emit(op_to_string(node.op()));
    node.lhs()->accept_visitor(*this);
}
void Printer::visit(const CallExpression& node)
{
    node.callee().accept_visitor(*this);
    emit("(");
    auto arguments = node.arguments();
    int n = arguments.size();
    for (auto& argument : arguments) {
        if (argument.is_spread)
            emit("...");
        argument.value->accept_visitor(*this);
        n--;
        if (n > 0)
            emit(", ");
    }
    emit(")");
}
void Printer::visit(const SuperCall& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ClassDeclaration& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ClassExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ClassMethod& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ClassField& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const StaticInitializer& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const StringLiteral& node)
{
    m_string_builder.appendff("\"{}\"", node.value());
}
void Printer::visit(const SuperExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const NumericLiteral& node)
{
    emit(node.value().to_string_without_side_effects());
}
void Printer::visit(const BigIntLiteral& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const BooleanLiteral& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const NullLiteral& node)
{
    (void)node;
    emit("null");
}
void Printer::visit(const BindingPattern& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const FunctionNode& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const FunctionDeclaration& node)
{
    auto parameters = node.parameters();
    auto& body = node.body();

    emit("function ");
    emit(node.name());
    emit("(");
    for (auto& parameter : parameters) {

        if (parameter.is_rest)
            emit("...");
        parameter.binding.visit(
            [&](FlyString const& name) {
                emit(name);
            },
            [&](BindingPattern const& pattern) {
                (void)pattern;
                emit("TODO");
            });
    }
    emit(") ");
    body.accept_visitor(*this);
    omit_semicolon();
}
void Printer::visit(const FunctionExpression& node)
{
    auto parameters = node.parameters();
    auto& body = node.body();

    emit("function ");
    emit(node.name());
    emit("(");
    for (auto& parameter : parameters) {
        if (parameter.is_rest)
            emit("...");
        parameter.binding.visit(
            [&](FlyString const& name) {
                emit(name);
            },
            [&](BindingPattern const& pattern) {
                (void)pattern;
                emit("TODO");
            });
    }
    emit(") ");
    body.accept_visitor(*this);
}
void Printer::visit(const YieldExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const AwaitExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ReturnStatement& node)
{
    emit("return ");
    node.argument()->accept_visitor(*this);
}
void Printer::visit(const IfStatement& node)
{
    emit("if(");
    node.predicate().accept_visitor(*this);
    emit(") ");

    auto& consequent = node.consequent();
    auto alternate = node.alternate();

    if (!consequent.is_scope_node()) {
        push_indent();
        newline();
        consequent.accept_visitor(*this);
        pop_indent();
    } else
        consequent.accept_visitor(*this);

    if (alternate) {
        semicolon();
        newline();
        emit("else ");
        if (!alternate->is_scope_node()) {
            push_indent();
            newline();
            alternate->accept_visitor(*this);
            pop_indent();
        } else
            alternate->accept_visitor(*this);
    }
}
void Printer::visit(const WhileStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const WithStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const DoWhileStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ForStatement& node)
{
    emit("for(");
    node.init()->accept_visitor(*this);
    emit("; ");
    node.test()->accept_visitor(*this);
    emit("; ");
    node.update()->accept_visitor(*this);
    emit(") ");
    if (!node.body().is_scope_node()) {
        push_indent();
        newline();
        node.body().accept_visitor(*this);
        pop_indent();
    } else
        node.body().accept_visitor(*this);
}
void Printer::visit(const ForInStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ForOfStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ForAwaitOfStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const Identifier& node)
{
    emit(node.string());
}
void Printer::visit(const PrivateIdentifier& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const SpreadExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ThisExpression& node)
{
    (void)node;
    emit("this");
}
void Printer::visit(const AssignmentExpression& node)
{
    auto lhs = node.lhs();
    lhs.visit(
        [&](Expression const& name) {
            name.accept_visitor(*this);
        },
        [&](BindingPattern const& pattern) {
            (void)pattern;
            emit("TODO");
        });
    emit(" ");
    emit(op_to_string(node.op()));
    emit(" ");
    node.rhs()->accept_visitor(*this);
}
void Printer::visit(const UpdateExpression& node)
{
    if (node.is_prefixed())
        emit(op_to_string(node.op()));
    node.argument()->accept_visitor(*this);
    if (!node.is_prefixed())
        emit(op_to_string(node.op()));
}
void Printer::visit(const VariableDeclaration& node)
{
    auto kind = node.declaration_kind();
    switch (kind) {
    case DeclarationKind::Const:
        emit("const ");
        break;
    case DeclarationKind::Let:
        emit("let ");
        break;
    case DeclarationKind::Var:
        emit("var ");
        break;
    default:
        VERIFY_NOT_REACHED();
        break;
    }

    auto declarations = node.declarations();

    int n = declarations.size();
    for (auto& declaration : declarations) {
        declaration.accept_visitor(*this);
        if (--n > 0)
            emit(", ");
    }
}
void Printer::visit(const VariableDeclarator& node)
{
    node.target().visit(
        [&](Identifier const& name) {
            name.accept_visitor(*this);
        },
        [&](BindingPattern const& pattern) {
            (void)pattern;
            emit("TODO");
        });
    emit(" = ");
    node.init()->accept_visitor(*this);
}
void Printer::visit(const ObjectProperty& node)
{
    VERIFY(node.type() == ObjectProperty::Type::KeyValue); // FIXME handle all types
    node.key().accept_visitor(*this);
    emit(": ");
    node.value().accept_visitor(*this); // FIXME what about the { key } situation?
}
void Printer::visit(const ObjectExpression& node)
{
    emit("{");
    auto properties = node.properties();
    int n = properties.size();
    for (auto& property : properties) {
        property.accept_visitor(*this);
        if (--n > 0)
            emit(", ");
    }
    emit("}");
}
void Printer::visit(const ExpressionStatement& node)
{
    node.expression().accept_visitor(*this);
}
void Printer::visit(const MemberExpression& node)
{
    node.object().accept_visitor(*this);
    emit(".");
    node.property().accept_visitor(*this);
}
void Printer::visit(const OptionalChain& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const MetaProperty& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ImportCall& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const RegExpLiteral& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ArrayExpression& node)
{
    emit("[");
    auto elements = node.elements();
    int n = elements.size();
    for (auto& element : elements) {
        element->accept_visitor(*this);
        if (--n > 0)
            emit(", ");
    }
    emit("]");
}
void Printer::visit(const TemplateLiteral& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const TaggedTemplateLiteral& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const TryStatement& node)
{
    emit("try ");
    node.block().accept_visitor(*this);
    auto handler = node.handler();
}
void Printer::visit(const CatchClause& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ThrowStatement& node)
{
    emit("throw ");
    node.argument().accept_visitor(*this);
}
void Printer::visit(const SwitchStatement& node)
{
    emit("switch(");
    node.discriminant()->accept_visitor(*this);
    emit(") {");
    push_indent();
    newline();
    for (auto& kase : node.cases()) {
        kase.accept_visitor(*this);
    }
    pop_indent();
    newline();
    emit("}");
    omit_semicolon();
}
void Printer::visit(const SwitchCase& node)
{
    auto test = node.test();
    if (test) {
        emit("case ");
        test->accept_visitor(*this);
        emit(":");
    } else
        emit("default:");
    push_indent();
    auto children = node.children();
    for (auto& child : children) {
        newline();
        child.accept_visitor(*this);
        semicolon();
    }
    pop_indent();
    newline();
}
void Printer::visit(const ConditionalExpression& node)
{
    node.test()->accept_visitor(*this);
    emit(" ? ");
    node.consequent()->accept_visitor(*this);
    push_indent(2);
    newline();
    emit(": ");
    node.alternate()->accept_visitor(*this);
    pop_indent();
}
void Printer::visit(const SequenceExpression& node)
{
    emit("(");
    auto expressions = node.expressions();
    int n = expressions.size();
    for (auto& expression : expressions) {
        expression.accept_visitor(*this);
        if (--n > 0)
            emit(", ");
    }
    emit(")");
}
void Printer::visit(const ExportStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ImportStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ErrorExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ContinueStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const SyntheticReferenceExpression& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ClassFieldInitializerStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ErrorDeclaration& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const ErrorStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const BreakStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const DebuggerStatement& node)
{
    (void)node;
    VERIFY_NOT_REACHED();
}
void Printer::visit(const EmptyStatement& node)
{
    (void)node;
    omit_semicolon();
}

}