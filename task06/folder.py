from model import *

ZERO = Number(0)


class ConstantFolder(ASTNodeVisitor):
    def visit_number(self, number):
        return number

    def visit_function(self, function):
        body_new = [expr.accept(self) for expr in function.body or []]
        return Function(function.args, body_new)

    def visit_function_definition(self, function_definition):
        function_new = function_definition.function.accept(self)
        return FunctionDefinition(function_definition.name, function_new)

    def visit_conditional(self, conditional):
        condition_new = conditional.condition.accept(self)
        if_true_new = [expr.accept(self)
                       for expr in conditional.if_true or []]
        if_false_new = [expr.accept(self)
                        for expr in conditional.if_false or []]

        return Conditional(condition_new, if_true_new, if_false_new)

    def visit_print(self, print_):
        return Print(print_.expr.accept(self))

    def visit_read(self, read):
        return read

    def visit_function_call(self, function_call):
        fun_expr_new = function_call.fun_expr.accept(self)
        args_new = [arg.accept(self) for arg in function_call.args or []]
        return FunctionCall(fun_expr_new, args_new)

    def visit_reference(self, reference):
        return reference

    def visit_binary_operation(self, binary_operation):
        op = binary_operation.op
        left = binary_operation.lhs.accept(self)
        right = binary_operation.rhs.accept(self)

        if isinstance(left, Number) and isinstance(right, Number):
            return BinaryOperation(left, op, right).evaluate(Scope())

        if isinstance(left, Reference) and isinstance(right, Number) and \
                right == ZERO and op == "*":
            return ZERO

        if isinstance(left, Number) and isinstance(right, Reference) and \
                left == ZERO and op == "*":
            return ZERO

        if isinstance(left, Reference) and isinstance(right, Reference) and \
                left.name == right.name and op == "-":
            return ZERO

        return BinaryOperation(left, op, right)

    def visit_unary_operation(self, unary_operation):
        op = unary_operation.op
        expr_new = unary_operation.expr.accept(self)

        if isinstance(expr_new, Number):
            return UnaryOperation(op, expr_new).evaluate(Scope())

        return UnaryOperation(op, expr_new)


def fold_constants(program):
    constant_folder = ConstantFolder()
    return program.accept(constant_folder)
