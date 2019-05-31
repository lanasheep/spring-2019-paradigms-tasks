import model

TAB = "    "


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self):
        self.result = ""
        self.depth = 0

    def check_ending(self):
        if not self.result.endswith("}"):
            self.result += ";"

    def get_result(self):
        self.check_ending()
        return self.result

    def print_tab(self):
        self.result += TAB * self.depth

    def visit_number(self, number, is_command=True):
        self.result += str(number.value)

    def visit_function(self, function):
        pass

    def visit_function_definition(self, function_definition):
        self.result += "def " + function_definition.name + "("
        self.result += ", ".join(function_definition.function.args)
        self.result += ") {\n"
        self.depth += 1
        for command in function_definition.function.body:
            self.print_tab()
            command.accept(self)
            self.check_ending()
            self.result += "\n"
        self.depth -= 1
        self.print_tab()
        self.result += "}"

    def visit_conditional(self, conditional):
        self.result += "if ("
        conditional.condition.accept(self)
        self.result += ") {\n"
        self.depth += 1
        if conditional.if_true:
            for expr in conditional.if_true:
                self.print_tab()
                expr.accept(self)
                self.check_ending()
                self.result += "\n"
        self.depth -= 1
        self.print_tab()
        self.result += "}"
        if not conditional.if_false:
            return
        self.result += " else {\n"
        self.depth += 1
        for expr in conditional.if_false:
            self.print_tab()
            expr.accept(self)
            self.check_ending()
            self.result += "\n"
        self.depth -= 1
        self.print_tab()
        self.result += "}"

    def visit_print(self, print_):
        self.result += "print "
        print_.expr.accept(self)

    def visit_read(self, read):
        self.result += "read " + read.name

    def visit_function_call(self, function_call):
        function_call.fun_expr.accept(self)
        self.result += "("
        for i, arg in enumerate(function_call.args):
            if i:
                self.result += ", "
            arg.accept(self)
        self.result += ")"

    def visit_reference(self, reference):
        self.result += reference.name

    def visit_binary_operation(self, binary_operation):
        self.result += "("
        binary_operation.lhs.accept(self)
        self.result += ")"
        self.result += " " + binary_operation.op + " "
        self.result += "("
        binary_operation.rhs.accept(self)
        self.result += ")"

    def visit_unary_operation(self, unary_operation):
        self.result += unary_operation.op + "("
        unary_operation.expr.accept(self)
        self.result += ")"


def pretty_print(program):
    printer = PrettyPrinter()
    program.accept(printer)
    print(printer.get_result())
