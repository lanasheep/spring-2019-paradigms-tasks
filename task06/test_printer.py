#!/usr/bin/env python3
import pytest
from model import *
from printer import *


def test_nothing():
    assert 2 * 2 == 4


def test_number():
    printer = PrettyPrinter()
    (Number(10)).accept(printer)
    result_correct = "10;"
    assert printer.get_result() == result_correct


def test_function_definition():
    printer = PrettyPrinter()
    (FunctionDefinition("foo", Function([], []))).accept(printer)
    result_correct = "def foo() {\n}"
    assert printer.get_result() == result_correct


def test_conditional():
    printer = PrettyPrinter()
    (Conditional(Number(42), [], [])).accept(printer)
    result_correct = "if (42) {\n}"
    assert printer.get_result() == result_correct


def test_print():
    printer = PrettyPrinter()
    (Print(Number(42))).accept(printer)
    result_correct = "print 42;"
    assert printer.get_result() == result_correct


def test_read():
    printer = PrettyPrinter()
    (Read("x")).accept(printer)
    result_correct = "read x;"
    assert printer.get_result() == result_correct


def test_function_call():
    printer = PrettyPrinter()
    lst = [Number(1), Number(2), Number(3)]
    (FunctionCall(Reference("foo"), lst)).accept(printer)
    result_correct = "foo(1, 2, 3);"
    assert printer.get_result() == result_correct


def test_reference():
    printer = PrettyPrinter()
    (Reference('x')).accept(printer)
    result_correct = "x;"
    assert printer.get_result() == result_correct


def test_binary_operation():
    printer = PrettyPrinter()
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    (mul).accept(printer)
    result_correct = "(1) * ((2) + (3));"
    assert printer.get_result() == result_correct


def test_unary_operation():
    printer = PrettyPrinter()
    (UnaryOperation('-', UnaryOperation('-', Number(42)))).accept(printer)
    result_correct = "(-(-42));"
    assert printer.get_result() == result_correct


def test_pretty_print(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))

    out, err = capsys.readouterr()
    assert out == """\
def main(arg1) {
    read x;
    print x;
    if ((2) == (3)) {
        if (1) {
        }
    } else {
        exit((-arg1));
    }
}
"""

    assert err == ""


if __name__ == "__main__":
    pytest.main()
